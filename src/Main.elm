module Main exposing (main)

import Animator
import Animator.Inline
import Browser
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Set
import Time
import Task
import Tree exposing (Tree(..))



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Post =
    { username : String
    , content : String
    , id : Int
    }


type alias Model =
    { root : Tree Post
    , exists : Animator.Timeline (Set.Set Int) -- prev, current
    , replyingId : Maybe Int -- matches id in Post
    , writing : Maybe Post
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Animator.toSubscription Tick model animator ]


animator : Animator.Animator Model
animator =
    Animator.animator
        -- this is an 'empty' animator that exists only to chain with |>
        |> Animator.watching .exists (\newExists model -> { model | exists = newExists })


init : () -> ( Model, Cmd msg )
init _ =
    let
        root =
            Branch (Post "god" "genesis" 1) [ Leaf (Post "skynet" "have fun!" 0) ]
    in
    ( Model root (Animator.init (treeToExists root)) Nothing Nothing
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Fade
    | Select Int
    | Add Int String -- Insert comment at parent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "hello" msg
    in
    case msg of
        Tick newTime ->
            ( Animator.update newTime animator model, Cmd.none )

        Fade ->
            ( model, Cmd.none)

        Select replyingId ->
            ( { model | replyingId = Just replyingId }, Cmd.none )

        Add replyingId content ->
            let
                newRoot =
                    model.root |> updateTreeRecursive replyingId content
            in
            ( { model
                | root = newRoot
                , replyingId = Nothing
                , exists = Animator.go Animator.slowly (treeToExists newRoot) model.exists
              }
            , Cmd.none
            )


updateTreeRecursive :
    Int
    -> String
    -> Tree Post
    -> Tree Post -- this order is better because it makes Tree pipeable
updateTreeRecursive replyingId content tree =
    case tree of
        Branch state children ->
            Branch state
                ((children
                    |> List.foldl (\elem result -> result ++ [ elem |> updateTreeRecursive replyingId content ]) []
                 )
                    ++ (if state.id == replyingId then
                            [ Branch (Post ("god" ++ String.fromInt (state.id+1)) content (state.id+1)) [] ]

                        else
                            []
                       )
                )

        Leaf _ ->
            tree



-- VIEW


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ Html.text "reddit v2" ]
        , treeToDivRecursive model.root model.replyingId model.exists
        ]


treeToDivRecursive : Tree Post -> Maybe Int -> Animator.Timeline (Set.Set Int) -> Html.Html Msg
treeToDivRecursive root replyingId timeline =
    case root of
        Branch state children ->
            Html.div []
                (Html.div
                    [ Html.Events.onClick (Select state.id)
                    , Animator.Inline.opacity timeline <|
                        \exists ->
                            Animator.at
                                (if Set.member state.id exists then
                                    1

                                 else
                                    0
                                )
                    ]
                    [ Html.text (state.username ++ ": \"" ++ state.content ++ "\"") ]
                    :: (if maybeEq state.id replyingId then
                            [ textBox state.id ]

                        else
                            []
                       )
                    ++ [ children
                            |> List.foldl (\elem result -> result ++ [ treeToDivRecursive elem replyingId timeline ]) []
                            |> Html.div [ Html.Attributes.class "child" ]
                       ]
                )

        Leaf state ->
            Html.div [] [ Html.text (state.username ++ " (bot): " ++ state.content) ]


treeToExists : Tree Post -> Set.Set Int
treeToExists root =
    case root of
        Leaf post ->
            Set.empty |> Set.insert post.id

        Branch post children ->
            let
                thisId =
                    Set.empty |> Set.insert post.id
            in
            List.foldl
                (\child ids -> ids |> Set.union (treeToExists child))
                thisId
                children


onSubmitField : String -> (String -> msg) -> Html.Attribute msg
onSubmitField target event =
    Html.Events.preventDefaultOn "submit"
    <| Json.Decode.map (\input -> ( event input, True ))
    <| Json.Decode.field "target"
    <| Json.Decode.field target
    <| Json.Decode.field "value"
    <| Json.Decode.string


textBox : Int -> Html.Html Msg
textBox id =
    Html.form [ onSubmitField "content" (Add id) ]
        [ Html.input [ Html.Attributes.type_ "text", Html.Attributes.name "content" ] []
        ]


maybeEq : Int -> Maybe Int -> Bool
maybeEq left right =
    case right of
        Just int ->
            left == int

        Nothing ->
            False



-- blah: List a -> b
-- blah = foldl (\x y -> y ) []
-- foldl: (a -> b -> b) -> b -> List a -> b
-- a |> (\a b c ... d -> ) []
-- (\a -> b) a
-- a = a
-- b = []
-- [=a,=b](c...d) {
-- }
-- extractState: Tree Post -> Post
-- extractState root = case root of
--   Branch state _ -> state
--   Leaf state -> state

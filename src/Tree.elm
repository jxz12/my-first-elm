-- the .. means we also expose Branch and Leaf
module Tree exposing (Tree(..))

import Set

-- content is a template type
type Tree content
    = Branch content (List (Tree content))
    | Leaf content  -- used for system messages that cannot be replied to


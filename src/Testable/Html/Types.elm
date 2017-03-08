module Testable.Html.Types exposing (..)

{-|
Types used for elm-testable html selectors and matchers

# Types
@docs Node, Attribute, Selector, Query, Options
-}

import Json.Decode as Json
import Json.Encode


{-| Html Nodes
-}
type Node msg
    = Node String (List (Attribute msg)) (List (Node msg))
    | KeyedNode String (List (Attribute msg)) (List ( String, Node msg ))
    | Text String


{-| Html Attributes
-}
type Attribute msg
    = Property String Json.Encode.Value
    | Style (List ( String, String ))
    | On String (Json.Decoder msg)
    | OnWithOptions String Options (Json.Decoder msg)


{-| Html Selector
-}
type Selector
    = Tag String
    | Attribute String String
    | Class String


{-| Html Queries, for composing selectors to find, findAll and thenFind/thenFindAll
-}
type Query
    = Single (List Selector)
    | Multiple (List Selector)
    | Children Query Query


{-| Options for Attributes Events with Options
-}
type alias Options =
    { stopPropagation : Bool
    , preventDefault : Bool
    }

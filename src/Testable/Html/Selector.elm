module Testable.Html.Selector exposing (tag, attribute, id)

{-|
A selector is used to find html nodes when testing

# Selectors
@docs tag, attribute, id
-}

import Testable.Html.Internal exposing (Selector(..))


{-| Find elements by tag name
-}
tag : String -> Selector
tag =
    Tag


{-| Find elements by any attribute with the specified value
-}
attribute : String -> String -> Selector
attribute =
    Attribute


{-| Find elements by id
-}
id : String -> Selector
id =
    Attribute "id"

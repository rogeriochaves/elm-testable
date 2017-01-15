module Testable.Html.Selectors exposing (tag, attribute, id, class)

{-|
A selector is used to find html nodes when testing

# Selectors
@docs tag, attribute, id, class
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


{-| Finds elements that have the given class (and possibly others as well).
-}
class : String -> Selector
class =
    Class

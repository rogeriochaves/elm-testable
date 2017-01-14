module Testable.Html.Selector exposing (tag)

{-|
A selector is used to find html nodes when testing

# Selectors
@docs tag
-}

import Testable.Html.Internal exposing (Selector(..))


{-| Find elements by tag name
-}
tag : String -> Selector
tag =
    Tag

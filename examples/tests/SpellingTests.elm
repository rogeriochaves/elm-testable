module SpellingTests exposing (..)

import Test exposing (..)
import Testable.TestContext exposing (..)
import Testable.Html.Selectors exposing (..)
import Spelling
import Expect


spellingComponent : Testable.TestContext.Component Spelling.Msg Spelling.Model
spellingComponent =
    { init = Spelling.init
    , update = Spelling.update
    , view = Spelling.view
    }


all : Test
all =
    describe "Spelling"
        [ test "calls suggestions check port when some suggestion is send"
            <| \() ->
                spellingComponent
                    |> startForTest
                    |> find [ tag "input" ]
                    |> trigger "input" "{\"target\": {\"value\": \"cats\"}}"
                    |> find [ tag "button" ]
                    |> trigger "click" "{}"
                    |> assertCalled (Spelling.check "cats")
        , test "renders received suggestions"
            <| \() ->
                spellingComponent
                    |> startForTest
                    |> update (Spelling.Suggest [ "dogs", "cats" ])
                    |> find [ class "results" ]
                    |> assertText (Expect.equal "dogs, cats")
        ]

module SubTests exposing (..)

import Test exposing (..)
import Expect
import Html
import TestContext exposing (TestContext)
import TestPorts


subProgram : Sub (Maybe String) -> TestContext (Maybe String) (Maybe String)
subProgram subs =
    { init = ( Nothing, Cmd.none )
    , update = \msg model -> ( msg, Cmd.none )
    , subscriptions = \_ -> subs
    , view = \_ -> Html.text ""
    }
        |> Html.program
        |> TestContext.start


all : Test
all =
    describe "Subscriptions"
        [ test "send triggers an update with the correct Msg" <|
            \() ->
                subProgram (TestPorts.stringSub Just)
                    |> TestContext.send TestPorts.stringSub "1"
                    |> Result.map TestContext.model
                    |> Expect.equal (Ok <| Just "1")
        , test "gives an error when not subscribed" <|
            \() ->
                subProgram (Sub.none)
                    |> TestContext.send TestPorts.stringSub "VALUE"
                    |> Expect.equal (Err "Not subscribed to port: stringSub")
        ]

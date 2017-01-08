module TestContext
    exposing
        ( TestContext
        , start
        , model
        , update
        , send
        , expectCmd
        , expectHttpRequest
        )

import TestContextWithMocks as WithMocks
import Expect


type alias TestableProgram model msg =
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }


type alias TestContext model msg =
    WithMocks.TestContext () model msg


start : Program flags model msg -> TestContext model msg
start realProgram =
    WithMocks.start (always realProgram) (always ())


model : TestContext model msg -> model
model context =
    WithMocks.model context


update : msg -> TestContext model msg -> TestContext model msg
update msg context =
    WithMocks.update msg context


send :
    ((value -> msg) -> Sub msg)
    -> value
    -> TestContext model msg
    -> Result String (TestContext model msg)
send subPort value context =
    WithMocks.send subPort value context


expectCmd : Cmd msg -> TestContext model msg -> Expect.Expectation
expectCmd expected context =
    WithMocks.expectCmd expected context


expectHttpRequest : String -> String -> TestContext model msg -> Expect.Expectation
expectHttpRequest method url context =
    WithMocks.expectHttpRequest method url context

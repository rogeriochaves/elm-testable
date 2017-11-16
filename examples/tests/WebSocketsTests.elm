module WebSocketsTests exposing (..)

import Expect
import Test exposing (..)
import Testable.Html.Selectors exposing (..)
import Testable.TestContext exposing (..)
import WebSocket
import WebSockets


webSocketsComponent : Testable.TestContext.Component WebSockets.Msg WebSockets.Model
webSocketsComponent =
    { init = WebSockets.init
    , update = WebSockets.update
    , view = WebSockets.view
    }


all : Test
all =
    describe "WebSockets"
        [ test "sends inputed message through websocket" <|
            \() ->
                webSocketsComponent
                    |> startForTest
                    |> find [ tag "input" ]
                    |> trigger "input" "{\"target\": {\"value\": \"dogs\"}}"
                    |> find [ tag "button" ]
                    |> trigger "click" "{}"
                    |> assertCalled (WebSocket.send WebSockets.echoServer "dogs")
        , test "writes values comming from the websocket to the screen in the right order" <|
            \() ->
                webSocketsComponent
                    |> startForTest
                    |> update (WebSockets.NewMessage "foo")
                    |> update (WebSockets.NewMessage "bar")
                    |> find [ tag "div" ]
                    |> thenFind [ tag "div" ]
                    |> assertText (Expect.equal "foobar")
        ]

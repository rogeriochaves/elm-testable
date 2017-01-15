port module TestableTests exposing (..)

import Expect
import Json.Decode as Decode
import Testable.TestContext as TestContext exposing (..)
import Testable.Cmd
import Testable.Http as Http exposing (defaultSettings)
import Testable.Html exposing (..)
import Testable.Html.Events exposing (..)
import Testable.Html.Attributes
import Test exposing (..)
import Testable.Html.Selectors exposing (..)
import Testable.Task as Task
import Testable.Process as Process
import Platform.Cmd
import Time
import Expect


type CounterMsg
    = Inc
    | Dec


counterComponent : Component CounterMsg Int
counterComponent =
    { init = ( 0, Testable.Cmd.none )
    , update =
        \msg model ->
            case msg of
                Inc ->
                    ( model + 1, Testable.Cmd.none )

                Dec ->
                    ( model - 1, Testable.Cmd.none )
    , view =
        \model ->
            div []
                [ text "Counter: "
                , button [ Testable.Html.Attributes.id "btn-inc", onClick Inc ] []
                , div [ Testable.Html.Attributes.class "counter value" ] [ text (toString model) ]
                ]
    }


type LoadingMsg
    = NewData (Result Http.Error String)


loadingComponent : Component LoadingMsg (Maybe String)
loadingComponent =
    { init =
        ( Nothing
        , Http.getString "https://example.com/"
            |> Http.toTask
            |> Task.attempt identity
            |> Testable.Cmd.map NewData
        )
    , update =
        \msg model ->
            case msg of
                NewData (Ok data) ->
                    ( Just data, Testable.Cmd.none )

                NewData (Err _) ->
                    ( model, Testable.Cmd.none )
    , view = \model -> text ""
    }


port outgoingPort : String -> Platform.Cmd.Cmd msg


all : Test
all =
    describe "Testable"
        [ test "initialized with initial model"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> assertCurrentModel 0
        , test "sending an msg"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> update Inc
                    |> update Inc
                    |> assertCurrentModel 2
        , test "records initial effects"
            <| \() ->
                loadingComponent
                    |> startForTest
                    |> assertHttpRequest (Http.getRequest "https://example.com/")
        , test "records initial effects"
            <| \() ->
                loadingComponent
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://example.com/")
                        (Http.ok "myData-1")
                    |> assertCurrentModel (Just "myData-1")
        , test "stubbing an unmatched effect should produce an error"
            <| \() ->
                loadingComponent
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://badwebsite.com/")
                        (Http.ok "_")
                    |> currentModel
                    |> Expect.equal (Err [ "No pending HTTP request: { method = \"GET\", headers = [], body = EmptyBody, timeout = Nothing, url = \"https://badwebsite.com/\", withCredentials = False }" ])
        , test "effects should be removed after they are run"
            <| \() ->
                loadingComponent
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://example.com/")
                        (Http.ok "myData-1")
                    |> resolveHttpRequest (Http.getRequest "https://example.com/")
                        (Http.ok "myData-2")
                    |> currentModel
                    |> Expect.equal (Err [ "No pending HTTP request: { method = \"GET\", headers = [], body = EmptyBody, timeout = Nothing, url = \"https://example.com/\", withCredentials = False }" ])
        , test "multiple initial effects should be resolvable"
            <| \() ->
                { init =
                    ( Nothing
                    , Testable.Cmd.batch
                        [ Task.attempt identity <| Http.toTask <| Http.getString "https://example.com/"
                        , Task.attempt identity <| Http.toTask <| Http.getString "https://secondexample.com/"
                        ]
                    )
                , update = \data model -> ( Just data, Testable.Cmd.none )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://example.com/")
                        (Http.ok "myData-1")
                    |> resolveHttpRequest (Http.getRequest "https://secondexample.com/")
                        (Http.ok "myData-2")
                    |> assertCurrentModel (Just <| Ok "myData-2")
        , test "Http.post effect"
            <| \() ->
                { init =
                    ( Ok 0
                    , Http.post "https://a" (Http.stringBody "text/plain" "requestBody") Decode.float
                        |> Http.toTask
                        |> Task.attempt identity
                    )
                , update = \value model -> ( value, Testable.Cmd.none )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> resolveHttpRequest
                        { defaultSettings
                            | url = "https://a"
                            , method = "POST"
                            , body = (Http.stringBody "text/plain" "requestBody")
                        }
                        (Http.ok "99.1")
                    |> assertCurrentModel (Ok 99.1)
        , test "Task.succeed"
            <| \() ->
                { init = ( "waiting", Task.succeed "ready" |> Task.perform identity )
                , update = \value model -> ( value, Testable.Cmd.none )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> assertCurrentModel "ready"
        , test "Task.fail"
            <| \() ->
                { init = ( Ok "waiting", Task.fail "failed" |> Task.attempt identity )
                , update = \value model -> ( value, Testable.Cmd.none )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> assertCurrentModel (Err "failed")
        , test "Task.andThen"
            <| \() ->
                { init = ( 0, Task.succeed 100 |> Task.andThen ((+) 1 >> Task.succeed) |> Task.perform identity )
                , update = \value model -> ( value, Testable.Cmd.none )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> assertCurrentModel 101
        , test "Process.sleep"
            <| \() ->
                { init =
                    ( "waiting"
                    , Process.sleep (5 * Time.second)
                        |> Task.andThen (\_ -> Task.succeed "5 seconds passed")
                        |> Task.perform identity
                    )
                , update = \value mode -> ( value, Testable.Cmd.none )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> advanceTime (4 * Time.second)
                    |> assertCurrentModel "waiting"
        , test "Process.sleep"
            <| \() ->
                { init =
                    ( "waiting"
                    , Process.sleep (5 * Time.second)
                        |> Task.andThen (\_ -> Task.succeed "5 seconds passed")
                        |> Task.perform identity
                    )
                , update = \value mode -> ( value, Testable.Cmd.none )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> advanceTime (5 * Time.second)
                    |> assertCurrentModel "5 seconds passed"
        , test "sending a value through a port"
            <| \() ->
                { init =
                    ( Nothing
                    , Testable.Cmd.none
                    )
                , update = \_ _ -> ( Nothing, Testable.Cmd.wrap <| outgoingPort "foo" )
                , view = \model -> text ""
                }
                    |> startForTest
                    |> update Inc
                    |> assertCalled (outgoingPort "foo")
        , test "asserting text"
            <| \() ->
                { init =
                    ( Nothing
                    , Testable.Cmd.none
                    )
                , update = \_ _ -> ( Nothing, Testable.Cmd.none )
                , view = \model -> text "foo"
                }
                    |> startForTest
                    |> assertText (Expect.equal "foo")
        , test "querying views"
            <| \() ->
                { init =
                    ( Nothing
                    , Testable.Cmd.none
                    )
                , update = \_ _ -> ( Nothing, Testable.Cmd.none )
                , view = \model -> div [] [ text "foo", input [] [ text "bar" ] ]
                }
                    |> startForTest
                    |> find [ tag "input" ]
                    |> assertText (Expect.equal "bar")
        , test "triggering events"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> find [ id "btn-inc" ]
                    |> trigger "click" "{}"
                    |> trigger "click" "{}"
                    |> find [ class "counter" ]
                    |> assertText (Expect.equal "2")
        , test "asserting count with findAll"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> findAll [ tag "div" ]
                    |> assertNodeCount (Expect.equal 2)
        , test "asserting count with find"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> find [ tag "div" ]
                    |> assertNodeCount (Expect.equal 1)
        , test "asserting node is present"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> find [ tag "div" ]
                    |> assertPresent
        , test "parent query"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> find [ tag "div" ]
                    |> assertText (Expect.equal "Counter: 0")
        , test "children query"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> find [ tag "div" ]
                    |> thenFind [ tag "div" ]
                    |> assertText (Expect.equal "0")
        , test "assert attribute"
            <| \() ->
                counterComponent
                    |> startForTest
                    |> find [ tag "div" ]
                    |> thenFind [ tag "div" ]
                    |> assertAttribute "className" (Expect.equal "counter value")
        ]

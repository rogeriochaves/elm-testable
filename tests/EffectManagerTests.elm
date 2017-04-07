module EffectManagerTests exposing (all)

import Test exposing (..)
import Expect
import Html
import Process
import Task
import TestContext exposing (TestContext)
import Test.EffectManager
import Time exposing (Time)


program : Cmd String -> Sub String -> TestContext String String
program cmd sub =
    { init =
        ( "INIT"
        , cmd
        )
    , update =
        \msg model ->
            case msg of
                "PING" ->
                    ( model, Test.EffectManager.pingSubs )

                _ ->
                    ( model ++ ";" ++ msg, Cmd.none )
    , subscriptions =
        \_ -> sub
    , view = \_ -> Html.text ""
    }
        |> Html.program
        |> TestContext.start


prefix : String -> String -> String
prefix pref s =
    pref ++ s


all : Test
all =
    describe "effect managers"
        [ test "it can process a Cmd" <|
            \() ->
                program
                    (Test.EffectManager.getState identity)
                    (Sub.none)
                    |> TestContext.expect (TestContext.model)
                        (Expect.equal "INIT;(INIT)")
        , test "it can process a Sub" <|
            \() ->
                program
                    (Cmd.none)
                    (Test.EffectManager.subState identity)
                    |> TestContext.update "PING"
                    |> TestContext.expect (TestContext.model)
                        (Expect.equal "INIT;[INIT]")
        , test "it works with Cmd.map" <|
            \() ->
                program
                    (Cmd.map (prefix "a") <| Test.EffectManager.getState identity)
                    (Sub.none)
                    |> TestContext.expect (TestContext.model)
                        (Expect.equal "INIT;a(INIT)")
        , test "it works with Sub.map" <|
            \() ->
                program
                    (Cmd.none)
                    (Sub.map (prefix "b") <| Test.EffectManager.subState identity)
                    |> TestContext.update "PING"
                    |> TestContext.expect (TestContext.model)
                        (Expect.equal "INIT;b[INIT]")
        ]
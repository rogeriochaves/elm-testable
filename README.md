## Note:

This is a fork from [avh4's elm-testable](https://github.com/avh4/elm-testable), which is getting rewritten in
Native code. This fork will keep being elm only.

This fork was upgraded to Elm 0.18 and has exciting new features, like testing your Html, querying, triggering events and so on.

[![Build Status](https://snap-ci.com/rogeriochaves/elm-testable/branch/master/build_image)](https://snap-ci.com/rogeriochaves/elm-testable/branch/master)

# rogeriochaves/elm-testable

This package allows you to write components that follow the Elm Architecture in a way that is testable.
To allow this, elm-testable provides testable versions of the `Html`, `Task`, `Effects`, `Http` and `Process` modules,
as well as `Testable.TestContext` to test testable components and `Testable` to integrate testable components with your Elm app.


## Example testable component

The only difference between a testable component and a standard component is the added `Testable.` in several imports. (With the exception of `Cmd`, which conflicts with the default import of `Platform.Cmd`)

Here is the diff of converting `RandomGif.elm` into a testable component:

```diff
diff --git a/examples/RandomGif.elm b/examples/RandomGif.elm
index 5ffd92e..85374a7 100644
--- a/examples/RandomGif.elm
+++ b/examples/RandomGif.elm
@@ -2,19 +2,21 @@ module RandomGif exposing (..)

 --- From example 5 of the Elm Architecture Tutorial https://github.com/evancz/elm-architecture-tutorial/blob/master/examples/05-http.elm

-import Html exposing (..)
-import Html.Attributes exposing (..)
-import Html.Events exposing (..)
-import Http
+import Testable.Html as Html exposing (..)
+import Testable.Html.Attributes exposing (..)
+import Testable.Html.Events exposing (..)
+import Testable.Http as Http
 import Json.Decode as Decode
+import Testable
+import Testable.Cmd


 main : Program Never Model Msg
 main =
     Html.program
-        { init = init "cats"
-        , view = view
-        , update = update
+        { init = Testable.init (init "cats")
+        , view = Testable.view view
+        , update = Testable.update update
         , subscriptions = subscriptions
         }

@@ -29,7 +31,7 @@ type alias Model =
     }


-init : String -> ( Model, Cmd Msg )
+init : String -> ( Model, Testable.Cmd.Cmd Msg )
 init topic =
     ( Model topic "waiting.gif"
     , getRandomGif topic
@@ -45,17 +47,17 @@ type Msg
     | NewGif (Result Http.Error String)


-update : Msg -> Model -> ( Model, Cmd Msg )
+update : Msg -> Model -> ( Model, Testable.Cmd.Cmd Msg )
 update msg model =
     case msg of
         MorePlease ->
             ( model, getRandomGif model.topic )

         NewGif (Ok newUrl) ->
-            ( Model model.topic newUrl, Cmd.none )
+            ( Model model.topic newUrl, Testable.Cmd.none )

         NewGif (Err _) ->
-            ( model, Cmd.none )
+            ( model, Testable.Cmd.none )



@@ -85,7 +87,7 @@ subscriptions model =
 -- HTTP


-getRandomGif : String -> Cmd Msg
+getRandomGif : String -> Testable.Cmd.Cmd Msg
 getRandomGif topic =
     let
         url =

```


## Example tests

Here is an example of the types of tests you can write for testable components:

```elm
all : Test
all =
    describe "RandomGif"
        [ test "sets initial topic"
            <| \() ->
                catsComponent
                    |> startForTest
                    |> find [ tag "h2" ]
                    |> assertText (Expect.equal "cats")
        , test "sets initial loading image"
            <| \() ->
                catsComponent
                    |> startForTest
                    |> assertShownImage "waiting.gif"
        , test "makes initial API request"
            <| \() ->
                catsComponent
                    |> startForTest
                    |> assertHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
        , test "pressing the button makes a new API request"
            <| \() ->
                catsComponent
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
                        (Http.ok """{"data":{"image_url":"http://giphy.com/cat2000.gif"}}""")
                    |> find [ tag "button" ]
                    |> trigger "click" "{}"
                    |> assertHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
        ]
```

Here are [complete tests for the RandomGif example](https://github.com/rogeriochaves/elm-testable/blob/master/examples/tests/RandomGifTests.elm).

## Testing Ports

You can also test that an outgoing port was called, by wrapping your ports with `Testable.Cmd.wrap`, like this:

```diff
diff --git a/examples/Spelling.elm b/examples/Spelling.elm
index 7ae91ce..d8c335e 100644
--- a/examples/Spelling.elm
+++ b/examples/Spelling.elm
@@ -2,18 +2,20 @@ port module Spelling exposing (..)

 -- From Elm Guide on JavaScript and Ports http://guide.elm-lang.org/interop/javascript.html

-import Html exposing (..)
-import Html.Attributes exposing (..)
-import Html.Events exposing (..)
+import Testable.Html as Html exposing (..)
+import Testable.Html.Events exposing (..)
+import Testable.Html.Attributes exposing (..)
 import String
+import Testable.Cmd
+import Testable


 main : Program Never Model Msg
 main =
     Html.program
-        { init = init
-        , view = view
-        , update = update
+        { init = Testable.init init
+        , update = Testable.update update
+        , view = Testable.view view
         , subscriptions = subscriptions
         }

@@ -28,9 +30,9 @@ type alias Model =
     }


-init : ( Model, Cmd Msg )
+init : ( Model, Testable.Cmd.Cmd Msg )
 init =
-    ( Model "" [], Cmd.none )
+    ( Model "" [], Testable.Cmd.none )



@@ -46,17 +48,17 @@ type Msg
 port check : String -> Cmd msg


-update : Msg -> Model -> ( Model, Cmd Msg )
+update : Msg -> Model -> ( Model, Testable.Cmd.Cmd Msg )
 update msg model =
     case msg of
         Change newWord ->
-            ( Model newWord [], Cmd.none )
+            ( Model newWord [], Testable.Cmd.none )

         Check ->
-            ( model, check model.word )
+            ( model, Testable.Cmd.wrap <| check model.word )

         Suggest newSuggestions ->
-            ( Model model.word newSuggestions, Cmd.none )
+            ( Model model.word newSuggestions, Testable.Cmd.none )



@@ -80,5 +82,5 @@ view model =
     div []
         [ input [ onInput Change ] []
         , button [ onClick Check ] [ text "Check" ]
-        , div [] [ text (String.join ", " model.suggestions) ]
+        , div [ class "results" ] [ text (String.join ", " model.suggestions) ]
         ]

```

And testing it like this:

```elm
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
```

Here are [complete tests for the Spelling example](https://github.com/rogeriochaves/elm-testable/blob/master/examples/tests/SpellingTests.elm).

There is also an example for [testing WebSockets](https://github.com/rogeriochaves/elm-testable/blob/master/examples/tests/WebSocketsTests.elm).

## Example integration with `Main`

To convert your testable `init`, `update` and `view` functions into functions that work with `Html.program`, use the `Testable` module:

```elm
main : Program Never Model Msg
main =
    Html.program
        { init = Testable.init MyComponent.init
        , update = Testable.update MyComponent.update
        , view = Testable.view MyComponent.view
        , subscriptions = MyComponent.subscriptions
        }
```

## TODO

There are still some pending functionalities, like spawning tasks, http.progress, effect managers and so on.

The Http API is not fully compatible with the original one, as you cannot pass Expect, only the Decoder for the request function, but it shouldn't be a problem unless you are doing some advanced stuff.

Also, the Html Selectors probably have some missing cases, like select a child in a specific position. If you need some Selector that is not there yet, please open an issue or send a PR.

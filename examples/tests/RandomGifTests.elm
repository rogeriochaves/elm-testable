module RandomGifTests exposing (..)

import Expect
import RandomGif
import Test exposing (..)
import Testable.Html.Selectors exposing (..)
import Testable.Http as Http
import Testable.TestContext exposing (..)


catsComponent : Component RandomGif.Msg RandomGif.Model
catsComponent =
    { init = RandomGif.init "cats"
    , update = RandomGif.update
    , view = RandomGif.view
    }


assertShownImage : String -> TestContext msg model -> Expect.Expectation
assertShownImage expectedImageUrl testContext =
    testContext
        |> find [ tag "img" ]
        |> assertAttribute "src" (Expect.equal expectedImageUrl)


all : Test
all =
    describe "RandomGif"
        [ test "sets initial topic" <|
            \() ->
                catsComponent
                    |> startForTest
                    |> find [ tag "h2" ]
                    |> assertText (Expect.equal "cats")
        , test "sets initial loading image" <|
            \() ->
                catsComponent
                    |> startForTest
                    |> assertShownImage "waiting.gif"
        , test "makes initial API request" <|
            \() ->
                catsComponent
                    |> startForTest
                    |> assertHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
        , test "shows the new image on API success" <|
            \() ->
                catsComponent
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
                        (Http.ok """{"data":{"image_url":"http://giphy.com/cat2000.gif"}}""")
                    |> assertShownImage "http://giphy.com/cat2000.gif"
        , test "shows the loading image on API failure" <|
            \() ->
                catsComponent
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
                        Http.serverError
                    |> assertShownImage "waiting.gif"
        , test "pressing the button makes a new API request" <|
            \() ->
                catsComponent
                    |> startForTest
                    |> resolveHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
                        (Http.ok """{"data":{"image_url":"http://giphy.com/cat2000.gif"}}""")
                    |> find [ tag "button" ]
                    |> trigger "click" "{}"
                    |> assertHttpRequest (Http.getRequest "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cats")
        ]

module Tests exposing (..)

import Test exposing (..)
import RandomGifTests
import SpellingTests
import WebSocketsTests


all : Test
all =
    describe "elm-testable examples"
        [ RandomGifTests.all
        , SpellingTests.all
        , WebSocketsTests.all
        ]

module Main exposing (main)

import Browser
import Html exposing (div, h1, text)
import Svg exposing (svg)


main =
    Browser.sandbox { init = init, view = view, update = update }


type alias Model =
    Int


type Msg
    = Noop


init =
    1


update msg model =
    model


view model =
    div [] [ h1 [] [ text "Really Nice Generative Art" ], svg [] [] ]

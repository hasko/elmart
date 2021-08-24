module Main exposing (main)

import Browser
import Color exposing (Color)
import Html exposing (div, h1, text)
import Random
import Random.Extra
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type alias Model =
    Maybe (List Painter)


type alias Painter =
    { px : Float, py : Float, dir : Float, step : Float, color : Color, life : Float }


type Msg
    = SeedPainters (List Painter)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, List.repeat 10 painterGenerator |> Random.Extra.sequence |> Random.generate SeedPainters )


subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeedPainters painters ->
            ( Just painters, Cmd.none )


view model =
    div []
        [ h1 [] [ text "Really Nice Generative Art" ]
        , svg [ viewBox "0 0 800 600" ] []
        ]


painterGenerator : Random.Generator Painter
painterGenerator =
    Random.map4
        (\px py dir step -> Painter px py dir step (Color.rgb 1 0 0) 1.0)
        (Random.float 0.0 800.0)
        (Random.float 0.0 600.0)
        (Random.float 0.0 (degrees 360))
        (Random.float 1.0 5.0)

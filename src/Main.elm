module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color exposing (Color)
import Html exposing (div, h1, p, text)
import Random
import Random.Extra
import Svg exposing (Svg, svg)
import Svg.Attributes as SA exposing (viewBox)


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
    | AnimationTick Float


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, List.repeat 10 painterGenerator |> Random.Extra.sequence |> Random.generate SeedPainters )


subscriptions model =
    case model of
        Nothing ->
            Sub.none

        Just _ ->
            onAnimationFrameDelta AnimationTick


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SeedPainters painters ->
            ( Just painters, Cmd.none )

        AnimationTick delta ->
            ( movePainters delta model, Cmd.none )


movePainters : Float -> Model -> Model
movePainters delta model =
    case model of
        Nothing ->
            Nothing

        Just painters ->
            Just (List.map (movePainter delta) painters)


movePainter : Float -> Painter -> Painter
movePainter delta painter =
    let
        d =
            painter.step * delta / 1000.0

        x =
            wrap 0 800 (painter.px + d * cos painter.dir)

        y =
            wrap 0 600 (painter.py + d * sin painter.dir)
    in
    { painter | px = x, py = y }


wrap : Float -> Float -> Float -> Float
wrap low hi val =
    if val < low then
        wrap low hi (val + hi - low)

    else if val > hi then
        wrap low hi (val - hi + low)

    else
        val


view model =
    div []
        [ h1 [] [ text "Really Nice Generative Art" ]
        , case model of
            Nothing ->
                p [] [ text "Seeding" ]

            Just painters ->
                svg [ viewBox "0 0 800 600" ]
                    (List.map painterToSvg painters)
        ]


painterGenerator : Random.Generator Painter
painterGenerator =
    Random.map5
        (\px py dir step col -> Painter px py dir step col 1.0)
        (Random.float 0.0 800.0)
        (Random.float 0.0 600.0)
        (Random.float 0.0 (degrees 360))
        (Random.float 1.0 100.0)
        (Random.map3
            Color.rgb
            (Random.float 0.0 1.0)
            (Random.float 0.0 1.0)
            (Random.float 0.0 1.0)
        )


painterToSvg : Painter -> Svg Msg
painterToSvg painter =
    Svg.circle
        [ painter.px |> String.fromFloat |> SA.cx
        , painter.py |> String.fromFloat |> SA.cy
        , SA.r "5"
        , SA.fill (Color.toCssString painter.color)
        ]
        []

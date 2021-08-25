module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Color exposing (Color)
import Html exposing (button, div, h1, input, label, p, text)
import Html.Attributes as HA exposing (for, id, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Random
import Random.Extra
import Svg exposing (Svg, animate, g, svg)
import Svg.Attributes as SA exposing (viewBox)
import Svg.Lazy exposing (lazy)


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }


type alias Model =
    Maybe { painters : List Painter, elements : List (List (Svg Msg)), running : Bool, mutationProbability : Float }


type alias Painter =
    { px : Float, py : Float, dir : Float, step : Float, color : Color, life : Float, genome : Genome }


type Msg
    = SeedPainters (List Painter)
    | AnimationTick Float
    | ToggleRunning
    | MutProb Float


type AGF
    = Add AGF AGF
    | Subtract AGF AGF
    | Multiply AGF AGF
    | Divide AGF AGF
    | Sin AGF
    | Cos AGF
    | Atan2 AGF AGF
    | Px
    | Py
    | Dir
    | Step
    | Hue
    | Sat
    | Light
    | Life
    | Delta
    | Number Float


type alias Genome =
    { px : AGF, py : AGF, dir : AGF, step : AGF, hue : AGF, sat : AGF, light : AGF }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, List.repeat 100 painterGenerator |> Random.Extra.sequence |> Random.generate SeedPainters )


subscriptions model =
    case model of
        Nothing ->
            Sub.none

        Just m ->
            if m.running then
                onAnimationFrameDelta AnimationTick

            else
                Sub.none



--- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Nothing ->
            case msg of
                SeedPainters painters ->
                    ( Just { painters = painters, elements = [], running = True, mutationProbability = 0.1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Just m ->
            case msg of
                AnimationTick delta ->
                    ( movePainters delta model, Cmd.none )

                ToggleRunning ->
                    ( Just { m | running = not m.running }, Cmd.none )

                MutProb p ->
                    ( Just { m | mutationProbability = p }, Cmd.none )

                _ ->
                    ( Nothing, Cmd.none )


movePainters : Float -> Model -> Model
movePainters delta model =
    case model of
        Nothing ->
            Nothing

        Just m ->
            Just
                { m
                    | painters = List.map (movePainter delta) m.painters
                    , elements = List.take 100 (List.map painterToSvg m.painters :: m.elements)
                }


movePainter : Float -> Painter -> Painter
movePainter delta painter =
    let
        newPainter =
            executeGenome delta painter
    in
    { newPainter | px = wrap 0 800 newPainter.px, py = wrap 0 600 newPainter.py }


executeGenome : Float -> Painter -> Painter
executeGenome delta painter =
    { painter
        | px = executeAGF delta painter painter.genome.px
        , py = executeAGF delta painter painter.genome.py
        , dir = executeAGF delta painter painter.genome.dir
        , step = executeAGF delta painter painter.genome.step
        , color =
            Color.hsl
                (executeAGF delta painter painter.genome.hue)
                (executeAGF delta painter painter.genome.sat)
                (executeAGF delta painter painter.genome.light)
    }


executeAGF : Float -> Painter -> AGF -> Float
executeAGF delta painter agf =
    case agf of
        Add agf1 agf2 ->
            executeAGF delta painter agf1 + executeAGF delta painter agf2

        Subtract agf1 agf2 ->
            executeAGF delta painter agf1 - executeAGF delta painter agf2

        Multiply agf1 agf2 ->
            executeAGF delta painter agf1 * executeAGF delta painter agf2

        Divide agf1 agf2 ->
            executeAGF delta painter agf1 / executeAGF delta painter agf2

        Sin agf1 ->
            sin (executeAGF delta painter agf1)

        Cos agf1 ->
            cos (executeAGF delta painter agf1)

        Atan2 agf1 agf2 ->
            atan2 (executeAGF delta painter agf1) (executeAGF delta painter agf2)

        Px ->
            painter.px

        Py ->
            painter.py

        Dir ->
            painter.dir

        Step ->
            painter.step

        Hue ->
            Color.toHsla painter.color |> .hue

        Sat ->
            Color.toHsla painter.color |> .saturation

        Light ->
            Color.toHsla painter.color |> .lightness

        Life ->
            painter.life

        Delta ->
            delta

        Number n ->
            n


wrap : Float -> Float -> Float -> Float
wrap low hi val =
    if val < low then
        wrap low hi (val + hi - low)

    else if val > hi then
        wrap low hi (val - hi + low)

    else
        val



--- VIEW


view model =
    div []
        [ h1 [] [ text "Really Nice Generative Art" ]
        , case model of
            Nothing ->
                p [] [ text "Seeding" ]

            Just m ->
                div []
                    [ div []
                        [ button [ onClick ToggleRunning ]
                            [ text
                                (if m.running then
                                    "Pause"

                                 else
                                    "Unpause"
                                )
                            ]
                        , div []
                            [ label [ for "mut-prob-input" ] [ text "Mutation probability [%]" ]
                            , input
                                [ id "mut-prob-input"
                                , type_ "number"
                                , HA.min "0.1"
                                , HA.max "1"
                                , HA.step "0.1"
                                , value (String.fromFloat m.mutationProbability)
                                , onInput (\s -> MutProb (String.toFloat s |> Maybe.withDefault m.mutationProbability))
                                ]
                                []
                            ]
                        ]
                    , svg
                        [ viewBox "0 0 800 600" ]
                        (List.map (\slice -> lazy (\sl -> g [] sl) slice) m.elements)
                    ]
        ]


painterToSvg : Painter -> Svg Msg
painterToSvg painter =
    Svg.circle
        [ painter.px |> String.fromFloat |> SA.cx
        , painter.py |> String.fromFloat |> SA.cy
        , SA.r "5"
        , SA.fill (Color.toCssString painter.color)
        ]
        []



--- Seeding


painterGenerator : Random.Generator Painter
painterGenerator =
    Random.map5
        (\px py dir step col -> Painter px py dir step col 1.0 initialGenome)
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


initialGenome : Genome
initialGenome =
    { px = Add Px (Multiply (Divide (Multiply Step Delta) (Number 1000.0)) (Cos Dir))
    , py = Add Py (Multiply (Divide (Multiply Step Delta) (Number 1000.0)) (Sin Dir))
    , dir = Dir
    , step = Step
    , hue = Hue
    , sat = Sat
    , light = Light
    }

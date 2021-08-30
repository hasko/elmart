module Main exposing (main)

import Adf exposing (Adf(..), UnaryOperator(..), executeAdf)
import Array exposing (Array)
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
import Time


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
    { params : Array Float, genome : Genome }


type Msg
    = SeedPainters (List Painter)
    | AnimationTick Float
    | ToggleRunning
    | MutProb Float
    | Mate Time.Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( Nothing, List.repeat 100 painterGenerator |> Random.Extra.sequence |> Random.generate SeedPainters )



--- Subscriptions


subscriptions model =
    case model of
        Nothing ->
            Sub.none

        Just m ->
            if m.running then
                Sub.batch
                    [ onAnimationFrameDelta AnimationTick
                    , Time.every 1000 Mate
                    ]

            else
                Sub.none



--- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Nothing ->
            case msg of
                SeedPainters painters ->
                    ( Just
                        { painters = painters
                        , elements = []
                        , running = False
                        , mutationProbability = 0.1
                        }
                    , Cmd.none
                    )

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

                Mate _ ->
                    ( model, Cmd.none )

                SeedPainters _ ->
                    ( model, Cmd.none )


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
    let
        hsla =
            Color.toHsla painter.color

        params =
            Array.fromList
                [ delta
                , painter.px
                , painter.py
                , painter.dir
                , painter.step
                , hsla.hue
                , hsla.saturation
                , hsla.lightness
                , painter.life
                ]
    in
    { painter
        | px = executeAdf params painter.genome.px |> Maybe.withDefault 0
        , py = executeAdf params painter.genome.py |> Maybe.withDefault 0
        , dir = executeAdf params painter.genome.dir |> Maybe.withDefault 0
        , step = executeAdf params painter.genome.step |> Maybe.withDefault 0
        , color =
            Color.hsl
                (executeAdf params painter.genome.hue |> Maybe.withDefault 0)
                (executeAdf params painter.genome.sat |> Maybe.withDefault 0)
                (executeAdf params painter.genome.light |> Maybe.withDefault 0)
    }


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
        [ Array.get 0 painter |> Maybe.withDefault 400 |> String.fromFloat |> SA.cx
        , Array.get 1 painter |> Maybe.withDefault 300 |> String.fromFloat |> SA.cy
        , Array.get 2 painter |> Maybe.withDefault 5 |> String.fromFloat |> SA.r
        , Maybe.map3
            (\h s l -> Color.hsl h s l)
            (Array.get 3 painter)
            (Array.get 4 painter)
            (Array.get 5 painter)
            |> Maybe.withDefault Color.black
            |> Color.toCssString
            |> SA.fill
        ]
        []



--- Seeding


painterGenerator : Random.Generator Painter
painterGenerator =
    Random.map (\p -> Painter p initialGenome)
        Random.map6
        (\px py size hue sat light ->
            Array.fromList [ px, py, size, hue, sat, light ]
                (Random.float 0.0 800.0)
                (Random.float 0.0 600.0)
                (Random.float 0.0 100.0)
                (Random.float 0.0 1.0)
                (Random.float 0.0 1.0)
                (Random.float 0.0 1.0)
        )



--- Genome


type alias Genome =
    { px : Adf, py : Adf, dir : Adf, step : Adf, hue : Adf, sat : Adf, light : Adf }


initialGenome : Genome
initialGenome =
    { px = Unary Param (Number 0)
    , py = Unary Param (Number 1)
    , dir = Unary Param (Number 2)
    , step = Unary Param (Number 3)
    , hue = Unary Param (Number 4)
    , sat = Unary Param (Number 5)
    , light = Unary Param (Number 6)
    }



{--
    { px = Add (Param (Number 1)) (Multiply (Divide (Multiply (Param (Number 4)) (Param (Number 0))) (Number 1000.0)) (Cos (Param (Number 3))))
    , py = Add (Param (Number 2)) (Multiply (Divide (Multiply (Param (Number 4)) (Param (Number 0))) (Number 1000.0)) (Sin (Param (Number 3))))
    , dir = Param (Number 3)
    , step = Param (Number 4)
    , hue = Param (Number 5)
    , sat = Param (Number 6)
    , light = Param (Number 7)
    }
    --}

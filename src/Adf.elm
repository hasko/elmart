module Adf exposing (Adf(..), BinaryOperator(..), UnaryOperator(..), executeAdf, size)

import Array exposing (Array)
import Maybe


type Adf
    = Number Float
    | Unary UnaryOperator Adf
    | Binary BinaryOperator Adf Adf


type UnaryOperator
    = Sin
    | Cos
    | Param


type BinaryOperator
    = Add
    | Subtract
    | Multiply
    | Divide
    | Atan2


executeAdf : Array Float -> Adf -> Maybe Float
executeAdf params adf =
    case adf of
        Number n ->
            Just n

        Unary op adf1 ->
            case op of
                Sin ->
                    Maybe.map sin (executeAdf params adf1)

                Cos ->
                    Maybe.map cos (executeAdf params adf1)

                Param ->
                    let
                        index =
                            executeAdf params adf1
                    in
                    case index of
                        Nothing ->
                            Nothing

                        Just i ->
                            Array.get (floor i) params

        Binary op adf1 adf2 ->
            case op of
                Add ->
                    Maybe.map2 (+) (executeAdf params adf1) (executeAdf params adf2)

                Subtract ->
                    Maybe.map2 (-) (executeAdf params adf1) (executeAdf params adf2)

                Multiply ->
                    Maybe.map2 (*) (executeAdf params adf1) (executeAdf params adf2)

                Divide ->
                    let
                        res =
                            Maybe.map2 (/) (executeAdf params adf1) (executeAdf params adf2)
                    in
                    case res of
                        Nothing ->
                            Nothing

                        Just r ->
                            if isInfinite r then
                                Nothing

                            else
                                res

                Atan2 ->
                    Maybe.map2 atan2 (executeAdf params adf1) (executeAdf params adf2)


size : Adf -> Int
size adf =
    1
        + (case adf of
            Binary _ adf1 adf2 ->
                size adf1 + size adf2

            Unary _ adf1 ->
                size adf1

            Number _ ->
                0
          )

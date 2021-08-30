module Adf exposing (Adf(..), executeAdf, size)

import Array exposing (Array)
import Maybe


type Adf
    = Add Adf Adf
    | Subtract Adf Adf
    | Multiply Adf Adf
    | Divide Adf Adf
    | Sin Adf
    | Cos Adf
    | Atan2 Adf Adf
    | Param Adf
    | Number Float


executeAdf : Array Float -> Adf -> Maybe Float
executeAdf params adf =
    case adf of
        Add adf1 adf2 ->
            Maybe.map2 (+) (executeAdf params adf1) (executeAdf params adf2)

        Subtract adf1 adf2 ->
            Maybe.map2 (-) (executeAdf params adf1) (executeAdf params adf2)

        Multiply adf1 adf2 ->
            Maybe.map2 (*) (executeAdf params adf1) (executeAdf params adf2)

        Divide adf1 adf2 ->
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

        Sin adf1 ->
            Maybe.map sin (executeAdf params adf1)

        Cos adf1 ->
            Maybe.map cos (executeAdf params adf1)

        Atan2 adf1 adf2 ->
            Maybe.map2 atan2 (executeAdf params adf1) (executeAdf params adf2)

        Param adf1 ->
            let
                index =
                    executeAdf params adf1
            in
            case index of
                Nothing ->
                    Nothing

                Just i ->
                    Array.get (clamp 0 (Array.length params - 1) (floor i)) params

        Number n ->
            Just n


size : Adf -> Int
size adf =
    1
        + (case adf of
            Add adf1 adf2 ->
                size adf1 + size adf2

            Subtract adf1 adf2 ->
                size adf1 + size adf2

            Multiply adf1 adf2 ->
                size adf1 + size adf2

            Divide adf1 adf2 ->
                size adf1 + size adf2

            Sin adf1 ->
                size adf1

            Cos adf1 ->
                size adf1

            Atan2 adf1 adf2 ->
                1 + size adf1 + size adf2

            Param adf1 ->
                size adf

            Number _ ->
                0
          )

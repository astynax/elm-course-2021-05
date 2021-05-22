module Columns exposing (main)

import Html exposing (Html)

import Columns.Field as Field exposing (Player(..), Cell(..))
import Columns.D4 exposing (D4(..))
import Columns.Render

main : Html msg
main =
    let
        put p v (x, y) = Field.put (Occupied p v) x y
        example =
            Field.empty
                |> put Blue V1 (V1, V1)
                |> put Blue V3 (V2, V1)
                |> put Blue V2 (V3, V1)
                |> put Blue V4 (V4, V1)
                |> put Red V4 (V1, V4)
                |> put Red V2 (V2, V4)
                |> put Red V3 (V3, V4)
                |> put Red V1 (V4, V4)
        try f x = Maybe.withDefault x (f x)
    in example
        |> try (Field.move (V1, V1) (V1, V2))
        |> try (Field.move (V1, V2) (V2, V2))
        |> try (Field.move (V2, V2) (V3, V2))
        |> try (Field.move (V3, V2) (V3, V1))
        |> Columns.Render.svg

module Columns exposing (main)

import Html exposing (Html)

import Columns.Field as Field exposing (Player(..), Cell(..))
import Columns.D4 exposing (D4(..))
import Columns.Render

main : Html msg
main =
    let
        example =
            Field.empty
                |> Field.put (Occupied Blue V1) (V1, V1)
                |> Field.put (Occupied Blue V3) (V2, V1)
                |> Field.put (Occupied Blue V2) (V3, V1)
                |> Field.put (Occupied Blue V4) (V4, V1)
                |> Field.put (Occupied Red V4) (V1, V4)
                |> Field.put (Occupied Red V2) (V2, V4)
                |> Field.put (Occupied Red V3) (V3, V4)
                |> Field.put (Occupied Red V1) (V4, V4)
        try f x = Maybe.withDefault x (f x)
    in example
        |> try (Field.move (V1, V1) (V1, V2))
        |> try (Field.move (V1, V2) (V2, V2))
        |> try (Field.move (V2, V2) (V3, V2))
        |> try (Field.move (V3, V2) (V3, V1))
        |> Columns.Render.svg

module Columns exposing (main)

import Html exposing (Html)

import Columns.Field exposing (Player(..), Cell(..), empty, updateXY)
import Columns.Vec4 exposing (D4(..))
import Columns.Render

main : Html msg
main =
    let
        put p v (x, y) = updateXY (always (Occupied p v)) x y
        example =
            empty
                |> put Blue V1 (V1, V1)
                |> put Blue V3 (V2, V1)
                |> put Blue V2 (V3, V1)
                |> put Blue V4 (V4, V1)
                |> put Red V4 (V1, V4)
                |> put Red V2 (V2, V4)
                |> put Red V3 (V3, V4)
                |> put Red V1 (V4, V4)
    in Columns.Render.svg example

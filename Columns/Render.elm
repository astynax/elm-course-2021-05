module Columns.Render exposing (svg)

import List

import Collage as C exposing (Collage)
import Collage.Layout as L
import Collage.Render exposing (svgBox)
import Color exposing (Color)
import Html exposing (Html)

import Columns.D4 as D4 exposing (D4(..))
import Columns.Field exposing (Field, Player(..), Cell(..))
import Columns.Vec4 as Vec4

svg : Field -> Html msg
svg = svgBox (400, 400) << L.center << field

field : Field -> Collage a
field fld =
    let
        row bs = L.horizontal << List.map2 cell bs << Vec4.toList
        bwbw = [False, True, False, True]
        wbwb = [True, False, True, False]
    in L.vertical <| List.map2 row [bwbw, wbwb, bwbw, wbwb] <| Vec4.toList fld

cell : Bool -> Cell -> Collage a
cell bg x =
    let square =
            C.rectangle 100 100
                |> C.filled (
                    (if bg then Color.yellow else Color.black)
                        |> C.uniform
                    )
        pieceList =
            case x of
                Empty -> [L.empty]
                Occupied p v ->
                    let pc = piece p
                    in case v of
                        V1 -> [pc 0]
                        V2 -> [pc 15, pc 0]
                        V3 -> [pc 30, pc 15, pc 0]
                        V4 -> [pc 45, pc 30, pc 15, pc 0]
        pieces =
            pieceList
                |> List.map (L.align L.bottom)
                |> L.stack
                |> L.center
    in L.impose pieces square

piece : Player -> Float -> Collage a
piece p s =
    let
        c =
            case p of
                Red -> Color.red
                Blue -> Color.blue
    in L.vertical
        [ C.ellipse 40 20
        |> C.styled
              ( C.uniform c
              , C.solid 3 <| C.uniform <| Color.white
              )
        , L.spacer 0 s
        ]

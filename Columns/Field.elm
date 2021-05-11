module Columns.Field exposing (..)

import Columns.Vec4 as Vec4 exposing (Vec4(..), D4)

type Player = Red | Blue

type Cell = Empty | Occupied Player D4

type alias Field = Vec4 (Vec4 Cell)

getXY : D4 -> D4 -> Vec4 (Vec4 a) -> a
getXY x y = Vec4.get x << Vec4.get y

updateXY
    : (a -> a)
    -> D4
    -> D4
    -> Vec4 (Vec4 a)
    -> Vec4 (Vec4 a)
updateXY f = Vec4.update << Vec4.update f

empty : Field
empty = Vec4.fill <| Vec4.fill Empty

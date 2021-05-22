module Columns.Field exposing
    ( Cell(..)
    , Field
    , Player(..)
    , empty
    , get
    , put
    , update
    , move
    )

import Maybe

import Columns.D4 as D4 exposing (D4(..))
import Columns.Vec4 as Vec4 exposing (Vec4(..))

type Player = Red | Blue

type Cell = Empty | Occupied Player D4

type alias Field = Vec4 (Vec4 Cell)

type FightResult = WinWith D4 | LoseTo D4 | Draw

get : D4 -> D4 -> Vec4 (Vec4 a) -> a
get x y = Vec4.get x << Vec4.get y

put
    : a
    -> D4 -> D4
    -> Vec4 (Vec4 a)
    -> Vec4 (Vec4 a)
put = update << always

update
    : (a -> a)
    -> D4 -> D4
    -> Vec4 (Vec4 a)
    -> Vec4 (Vec4 a)
update f = Vec4.update << Vec4.update f

empty : Field
empty = Vec4.fill <| Vec4.fill Empty

move
    : (D4, D4)
    -> (D4, D4)
    -> Field
    -> Maybe Field
move (x1, y1) (x2, y2) field =
    let isPossible =
            (x1 == x2 && near y1 y2)
            || (near x1 x2 && y1 == y2)
        putNew c =
            field
                |> put Empty x1 y1
                |> put c x2 y2
                |> Just
    in if not isPossible then Nothing
    else
        case (get x1 y1 field, get x2 y2 field) of
            (Empty, _) -> Nothing
            (o, Empty) -> putNew o
            (Occupied p1 v1, Occupied p2 v2) ->
                if (p1 == p2)
                then
                    D4.add v1 v2
                        |> Maybe.andThen (putNew << Occupied p1)
                else
                    (case fight v1 v2 of
                        Draw -> Empty
                        WinWith v -> Occupied p1 v
                        LoseTo v -> Occupied p2 v)
                        |> putNew

near : D4 -> D4 -> Bool
near a b = D4.inc a == Just b || D4.dec a == Just b

fight : D4 -> D4 -> FightResult
fight a b =
    case (D4.dec a, D4.dec b) of
        (_, Nothing) -> WinWith a
        (Nothing, Just V1) -> Draw
        (Nothing, Just v) -> LoseTo v
        (Just x, Just y) -> fight x y

module Lesson03 exposing (main)

import Columns.D4 as D4 exposing (D4(..))
import Columns.Field as Field exposing (Field, Player(..), Cell(..))
import Columns.Render
import Columns.Vec4 as Vec4 exposing (Vec4(..))
import Utils exposing (print)

main =
    let
        try f x = Maybe.withDefault x (f x)
    in Field.empty
        |> Field.put (Occupied Red V4) V1 V1
        |> Field.put (Occupied Blue V4) V2 V1
        |> try (move (V1, V1) (V2, V1))
        |> Columns.Render.svg

doTurn : (D4, D4) -> (D4, D4) -> Player -> Field -> Maybe (Player, Field)
doTurn =
    Debug.todo "implement me!"
    -- вам пригодятся функции Maybe.map и Maybe.andThen

move : (D4, D4) -> (D4, D4) -> Field -> Maybe Field
move (x1, y1) (x2, y2) field =
    let isPossible =
            (x1 == x2 && near y1 y2)
            || (near x1 x2 && y1 == y2)
        putNew c =
            field
                |> Field.put Empty x1 y1
                |> Field.put c x2 y2
                |> Just
    in if not isPossible then Nothing
    else
        case (Field.get x1 y1 field, Field.get x2 y2 field) of
            (Empty, _) -> Nothing
            (o, Empty) -> putNew o
            (Occupied p1 v1, Occupied p2 v2) ->
                if (p1 == p2)
                then
                    add v1 v2
                        |> Maybe.andThen (putNew << Occupied p1)
                else
                    (case fight v1 v2 of
                        Nothing -> Empty
                        Just (Ok v) -> Occupied p1 v
                        Just (Err v) -> Occupied p2 v)
                        |> putNew

near : D4 -> D4 -> Bool
near a b = inc a == Just b || dec a == Just b

inc : D4 -> Maybe D4
inc x =
    case x of
        V1 -> Just V2
        V2 -> Just V3
        V3 -> Just V4
        V4 -> Nothing

dec : D4 -> Maybe D4
dec x =
    case x of
        V1 -> Nothing
        V2 -> Just V1
        V3 -> Just V2
        V4 -> Just V3

add : D4 -> D4 -> Maybe D4
add a b =
    case (dec a, inc b) of
        (Nothing, x) -> x
        (_, Nothing) -> Nothing
        (Just x, Just y) -> add x y

sub : D4 -> D4 -> Maybe D4
sub a b =
    case (dec a, dec b) of
        (Nothing, _) -> Nothing
        (x, Nothing) -> x
        (Just x, Just y) -> sub x y

fight : D4 -> D4 -> Maybe (Result D4 D4)
fight a b =
    case (dec a, dec b) of
        (_, Nothing) -> Just (Ok a)
        (Nothing, Just V1) -> Nothing
        (Nothing, Just v) -> Just (Err v)
        (Just x, Just y) -> fight x y

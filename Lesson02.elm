module Lesson02 exposing (main)

import Html
import List
import Maybe
import String

import Utils exposing (print)

-- the domain

type Player = Red | Blue

type Cell = Empty | Occupied Player D4

type D4 = V1 | V2 | V3 | V4

type Vec4 a = Vec4 a a a a

type alias Field = Vec4 (Vec4 Cell)

-- API for Vec4

mapV4 : (a -> b) -> Vec4 a -> Vec4 b
mapV4 f (Vec4 c1 c2 c3 c4) =
    Vec4 (f c1) (f c2) (f c3) (f c4)

fromV4 : Vec4 a -> List a
fromV4 (Vec4 c1 c2 c3 c4) = [c1, c2, c3, c4]

at : D4 -> Vec4 a -> a
at pos vec =
    case (pos, vec) of
        (V1, Vec4 x _ _ _) -> x
        (V2, Vec4 _ x _ _) -> x
        (V3, Vec4 _ _ x _) -> x
        (V4, Vec4 _ _ _ x) -> x

apply : (a -> a) -> D4 -> Vec4 a -> Vec4 a
apply f pos vec =
    case (pos, vec) of
        (V1, Vec4 a b c d) -> Vec4 (f a) b c d
        (V2, Vec4 a b c d) -> Vec4 a (f b) c d
        (V3, Vec4 a b c d) -> Vec4 a b (f c) d
        (V4, Vec4 a b c d) -> Vec4 a b c (f d)

atXY : D4 -> D4 -> Vec4 (Vec4 a) -> a
atXY x y = at x << at y

applyXY
    : (a -> a)
    -> D4
    -> D4
    -> Vec4 (Vec4 a)
    -> Vec4 (Vec4 a)
applyXY f x y = apply (apply f x) y

-- game logic

emptyField : Field
emptyField =
    let
        e = Empty
        r = Vec4 e e e e
    in Vec4 r r r r

initial : Field
initial =
    let put p = always (Occupied p V1)
    in emptyField
        |> applyXY (put Blue) V1 V1
        |> applyXY (put Blue) V2 V1
        |> applyXY (put Blue) V3 V1
        |> applyXY (put Blue) V4 V1
        |> applyXY (put Red) V1 V4
        |> applyXY (put Red) V2 V4
        |> applyXY (put Red) V3 V4
        |> applyXY (put Red) V4 V4

move : (D4, D4) -> (D4, D4) -> Field -> Maybe Field
move = Debug.todo "Implement me!"

-- board "rendering"

color : Player -> String
color x =
    case x of
        Red -> "R"
        Blue -> "B"

d4 : D4 -> String
d4 x =
    case x of
        V1 -> "1"
        V2 -> "2"
        V3 -> "3"
        V4 -> "4"

cell : Cell -> String
cell x =
    let
        inner =
            case x of
                Empty -> "  "
                Occupied p d ->
                    String.append (color p) (d4 d)
    in inner
        |> String.append "["
        |> (\s -> String.append s "]")

row : Vec4 Cell -> String
row = showVec4 cell String.append

field : Vec4 (Vec4 Cell) -> String
field = showVec4 row (\a b -> String.concat [a, "\n", b])

showVec4
    : (a -> String)
    -> (String -> String -> String)
    -> Vec4 a
    -> String
showVec4 toStr join =
    List.foldl join "" << fromV4 << mapV4 toStr

-- an entry point

main =
    Html.pre []
        [ Html.text
              <| field
              <| applyXY (always (Occupied Blue V3)) V3 V2
              <| applyXY (always (Occupied Red V4)) V2 V3
              <| initial
        ]

-- some recursive structures just for example

type Tree a = Leaf | Branch a (Tree a) (Tree a)

mapTree f t =
    case t of
        Leaf -> Leaf
        Branch x t1 t2 ->
            Branch (f x) (mapTree f t1) (mapTree f t2)

type OurList a = Nil | Cons a (OurList a)

example = Cons 1 <| Cons 2 <| Cons 3 <| Nil

mapList f l =
    case l of
        (Cons x xs) -> Cons (f x) (mapList f xs)
        Nil -> Nil

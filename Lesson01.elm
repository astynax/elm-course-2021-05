module Lesson01 exposing (main)

import Utils exposing (print)

add : Int -> Int -> Int
add x y = x + y

-- flip : (a -> b -> c) -> b -> a -> c
-- flip f a b = f b a

-- id : a -> a
-- id x = x
-- comp : (a -> b) -> (b -> c) -> a -> c
-- comp f g x = g (f x)
-- >>

-- add10 = add 5 >> add 5

-- computation = add 2
--         >> add 5
--         >> add 1000

type Color = R | B | G

type Unit = Unit

type Tricolor = Tricolor Color Color Color

type Flag
    = Solid Color
    | Triple Tricolor
    | SameAs Flag

foo = { flag = Solid R, age = Age 42, name = "Bob" }

type alias Ageish = Int
type alias Soldierish = { name : String, age : Ageish, flag : Flag }

type Age = Age Int

type Soldier = Soldier
    { name : String
    , age : Age
    , flag : Flag
    }

type Enemy = Enemy
    { name : String
    , age : Age
    , flag : Flag
    }

val : (Color, Color, Color)
val = (R, G, B)

flag : Tricolor
flag = Tricolor R G R

redBob = Soldier foo
badBob = Enemy foo

main = print redBob

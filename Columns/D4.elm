module Columns.D4 exposing
    ( D4(..)
    , inc
    , dec
    , add
    , sub
    )

type D4 = V1 | V2 | V3 | V4

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

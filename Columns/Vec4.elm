module Columns.Vec4 exposing
    ( Vec4(..)
    , fill
    , map
    , toList
    , get
    , update
    )

import Columns.D4 exposing (D4(..))

type Vec4 a = Vec4 a a a a

fill : a -> Vec4 a
fill x = Vec4 x x x x

map : (a -> b) -> Vec4 a -> Vec4 b
map f (Vec4 c1 c2 c3 c4) =
    Vec4 (f c1) (f c2) (f c3) (f c4)

toList : Vec4 a -> List a
toList (Vec4 c1 c2 c3 c4) = [c1, c2, c3, c4]

get : D4 -> Vec4 a -> a
get pos vec =
    case (pos, vec) of
        (V1, Vec4 x _ _ _) -> x
        (V2, Vec4 _ x _ _) -> x
        (V3, Vec4 _ _ x _) -> x
        (V4, Vec4 _ _ _ x) -> x

update : (a -> a) -> D4 -> Vec4 a -> Vec4 a
update f pos vec =
    case (pos, vec) of
        (V1, Vec4 a b c d) -> Vec4 (f a) b c d
        (V2, Vec4 a b c d) -> Vec4 a (f b) c d
        (V3, Vec4 a b c d) -> Vec4 a b (f c) d
        (V4, Vec4 a b c d) -> Vec4 a b c (f d)

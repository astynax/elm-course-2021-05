module Lesson05 exposing (main)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Utils

type OurList a = Cons a (OurList a) | Nil

example = 4 :: 5 :: 6 :: []

flip f x y = f y x

map : (a -> b) -> List a -> List b
map f l =
    case l of
        [] -> []
        x :: xs -> f x :: map f xs

filter : (a -> Bool) -> List a -> List a
filter f l =
    case l of
        [] -> []
        x :: xs ->
            if f x
            then x :: filter f xs
            else      filter f xs

-- 4 @ (5 @ (6 @ Z))
-- ((Z @ 4) @ 5) @ 6

foldr : (a -> b -> b) -> b -> List a -> b
foldr op acc l =
    case l of
        [] -> acc
        x :: xs ->
             op x (foldr op acc xs)

-- op 4 (foldr op acc [5, 6])
-- op 4 (op 5 (foldr op acc [6]))
-- op 4 (op 5 (op 6 (foldr op acc [])))
-- op 4 (op 5 (op 6 acc))

foldl : (b -> a -> b) -> b -> List a -> b
foldl op acc l =
    case l of
        [] -> acc
        x :: xs ->
            foldl op (op acc x) xs

-- foldl op acc [4, 5, 6]
-- foldl op (op acc 4) [5, 6]
-- foldl op (op (op acc 4) 5) [6]
-- foldl op (op (op (op acc 4) 5) 6) []
-- (op (op (op acc 4) 5) 6)

reverse acc l =
    case l of
        [] -> acc
        x :: xs ->
            reverse (x :: acc) xs

mapF f = reverse [] << foldl (\acc x -> f x :: acc) []

filterF p = reverse [] << foldl (\acc x -> if p x then x :: acc else acc) []

-- queues

joinQueues l1 l2 =
    case (l1, l2) of
        ([], _) -> l2
        (_, []) -> l1
        (x :: xs, y :: ys) ->
            x :: y :: joinQueues xs ys

multiJoin = foldr joinQueues []

queue = multiJoin [[1,2,3,4,5,6], [20,30], [1000,2000]]

-- zip

zip : List a -> List b -> List (a, b)
zip = zipWith (\x y -> (x, y))

zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f l1 l2 =
    case (l1, l2) of
        ([], _) -> []
        (_, []) -> []
        (x :: xs, y :: ys) ->
            f x y :: zipWith f xs ys

type CounterAction = Inc | Dec | Set Int
type Counter = Counter Int

run : Counter -> List CounterAction -> (Counter, List Counter)
run c = Tuple.mapSecond (reverse []) << foldl step (c, [])

step : (Counter, List Counter) -> CounterAction -> (Counter, List Counter)
step (Counter v, history) a =
    (case a of
        Inc -> Counter (v + 1)
        Dec -> Counter (v - 1)
        Set x -> Counter x
    ,Counter v :: history)

simulation = run (Counter 0) [Inc, Inc, Dec, Set 42, Dec]

-- TEA, The Elm Architecture

type alias Model = (Counter, Counter)
type Msg = First CounterAction | Second CounterAction

main =
    Browser.sandbox
        { init = (Counter 0, Counter 0)
        , view = view
        , update = update
        }

view : Model -> Html Msg
view (c1, c2) =
    Html.ul []
        [ Html.li [] [Html.map First <| viewCounter c1]
        , Html.li [] [Html.map Second <| viewCounter c2]
        ]

update : Msg -> Model -> Model
update msg (c1, c2) =
    case msg of
        First m -> (updateCounter m c1, c2)
        Second m -> (c1, updateCounter m c2)

viewCounter : Counter -> Html CounterAction
viewCounter (Counter value) =
    Html.div []
        [ Html.text <| String.fromInt value
        , Html.button [onClick Inc] [Html.text "+"]
        , Html.button [onClick Dec] [Html.text "-"]
        , Html.button [onClick (Set 0)] [Html.text "0"]
        ]

updateCounter : CounterAction -> Counter -> Counter
updateCounter msg (Counter v) =
    case msg of
        Inc -> Counter (v + 1)
        Dec -> Counter (v - 1)
        Set x -> Counter x

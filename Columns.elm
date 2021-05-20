module Columns exposing (main)

import Columns.Field exposing (Cell(..), Field, Player(..), empty, getXY, updateXY)
import Columns.Render
import Columns.Vec4 exposing (D4(..), Vec4)
import Html exposing (Html)


putEmpty : ( D4, D4 ) -> Vec4 (Vec4 Cell) -> Vec4 (Vec4 Cell)
putEmpty ( x, y ) =
    updateXY (always Empty) x y


put p v ( x, y ) =
    updateXY (always (Occupied p v)) x y


main : Html msg
main =
    let
        example =
            empty
                |> put Red V1 ( V1, V4 )
                |> put Blue V1 ( V1, V3 )
                -- |> move ( V1, V3 ) ( V2, V4 ) bug diagonal
                |> move ( V1, V3 ) ( V2, V3 )
                |> move ( V1, V4 ) ( V2, V4 )
    in
    Columns.Render.svg example



-- move : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Maybe Field пока что без maybe


move : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Field
move from to field =
    let
        ( fromX, fromY ) =
            from

        ( toX, toY ) =
            to

        cell : Cell
        cell =
            getXY fromX fromY field

        putFishkaOrEmpty =
            case cell of
                Empty ->
                    -- сходило empty, в будущем не валидно или не важно
                    updateXY (always Empty) fromX fromY

                Occupied player d4 ->
                    updateXY (always (Occupied player d4)) toX toY
    in
    if validateXY from to field then
        let
            _ =
                Debug.log "Ход сделан: " ( from, to )
        in
        field
            |> putEmpty from
            |> putFishkaOrEmpty

    else
        let
            _ =
                Debug.log "Ход НЕ сделан: " ( from, to )
        in
        field


validateXY : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Bool
validateXY from to field =
    let
        ( fromX, fromY ) =
            from

        ( toX, toY ) =
            to
    in
    (validate fromX toX && validate fromY toY)
        && not (isSameCell from to)
        && not (isOccupiedByEnemy from to field)


isSameCell : ( D4, D4 ) -> ( D4, D4 ) -> Bool
isSameCell from to =
    from == to


isOccupiedByEnemy : ( D4, D4 ) -> ( D4, D4 ) -> Field -> Bool
isOccupiedByEnemy ( fromX, fromY ) ( toX, toY ) field =
    let
        fromColorOrFromEmpty =
            getXY fromX fromY field

        toColorOrToEmpty =
            getXY toX toY field
    in
    case fromColorOrFromEmpty of
        Empty ->
            -- лишний кейс, empty ходить не может
            False

        Occupied fromPlayer _ ->
            case fromPlayer of
                Red ->
                    let
                        result =
                            case toColorOrToEmpty of
                                Empty ->
                                    -- на пустое место можно сходить всегда
                                    False

                                Occupied toPlayer _ ->
                                    case toPlayer of
                                        Red ->
                                            -- Red идет на клетку занятую Red - можно
                                            -- TODO: d4 увеличить
                                            False

                                        Blue ->
                                            -- клетка занята противником
                                            True
                    in
                    result

                Blue ->
                    let
                        result =
                            case toColorOrToEmpty of
                                Empty ->
                                    -- на пустое место можно сходить всегда
                                    False

                                Occupied toPlayer _ ->
                                    case toPlayer of
                                        Red ->
                                            -- клетка занята противником
                                            True

                                        Blue ->
                                            -- Blue идет на клетку занятую Blue - можно
                                            -- TODO: d4 увеличить
                                            False
                    in
                    result


{-| можно ли и нужно ли записать короче?

  - проверяем что мы сделали шаг на 0 или 1 клетку

-}
validate : D4 -> D4 -> Bool
validate from to =
    case from of
        V1 ->
            case to of
                V1 ->
                    True

                V2 ->
                    True

                V3 ->
                    False

                V4 ->
                    False

        V2 ->
            case to of
                V1 ->
                    True

                V2 ->
                    True

                V3 ->
                    True

                V4 ->
                    False

        V3 ->
            case to of
                V1 ->
                    False

                V2 ->
                    True

                V3 ->
                    True

                V4 ->
                    True

        V4 ->
            case to of
                V1 ->
                    False

                V2 ->
                    False

                V3 ->
                    True

                V4 ->
                    True

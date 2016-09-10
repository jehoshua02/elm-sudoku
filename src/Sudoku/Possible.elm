module Sudoku.Possible
    exposing
        ( Possible
        , initialize
        , toPuzzle
        , eliminateUsed
        )

import Set
import Sudoku.Puzzle exposing (Puzzle)
import Sudoku.Grid exposing (rows, columns, groups)
import List.Extra exposing (getAt, removeAt, unique)


type alias Possible =
    List (List Int)


initialize : Puzzle -> Possible
initialize puzzle =
    puzzle
        |> List.map
            (\x ->
                if x == 0 then
                    [1..9]
                else
                    [ x ]
            )


toPuzzle : Possible -> Puzzle
toPuzzle =
    List.map
        (\xs ->
            if List.length xs == 1 then
                get 0 0 xs
            else
                0
        )


eliminateUsed : Possible -> Possible
eliminateUsed possible =
    let
        puzzle =
            toPuzzle possible
    in
        possible
            |> List.indexedMap
                (\i xs ->
                    possible
                        |> get i []
                        |> Set.fromList
                        |> flip Set.diff (Set.fromList (used i puzzle))
                        |> Set.toList
                )


used : Int -> Puzzle -> List Int
used i puzzle =
    let
        ( x, y ) =
            ( i % 9, i // 9 )

        row =
            get x [] (rows puzzle) |> removeAt y

        column =
            get y [] (columns puzzle) |> removeAt x

        group =
            let
                g =
                    -- index of group
                    x // 3 + y // 3 * 3

                i =
                    -- index within group
                    (x % 3) // 3 + (y % 3) * 3
            in
                get g [] (groups puzzle) |> removeAt i
    in
        row ++ column ++ group |> unique


get : Int -> a -> List a -> a
get i d xs =
    getAt i xs |> Maybe.withDefault d

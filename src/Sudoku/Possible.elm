module Sudoku.Possible
    exposing
        ( Possible
        , initialize
        , toPuzzle
        , eliminate
        , eliminateUsed
        , eliminateCrowds
        , eliminateSame
        )

import Set
import Sudoku.Puzzle exposing (Puzzle)
import Sudoku.Grid exposing (rows, columns, groups)
import List.Extra exposing (removeAt, updateAt, unique, findIndices)
import Util exposing (get, diff, set)


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


eliminate : List Int -> List Int -> Possible -> Possible
eliminate xs is possible =
    case is of
        [] ->
            possible

        i :: is ->
            possible
                |> updateAt i (flip diff xs)
                |> Maybe.withDefault possible
                |> eliminate xs is


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


eliminateCrowds : Possible -> Possible
eliminateCrowds possible =
    let
        coordToIndex =
            (\x y -> x + y * 9)
    in
        possible
            |> eliminateCrowds' rows coordToIndex
            |> eliminateCrowds' columns coordToIndex
            |> eliminateCrowds' groups
                (\g i ->
                    let
                        x =
                            (i % 3) + (g % 3) * 3

                        y =
                            (i // 3) + (g // 3) * 3
                    in
                        coordToIndex x y
                )


eliminateCrowds' : (Possible -> List (List (List Int))) -> (Int -> Int -> Int) -> Possible -> Possible
eliminateCrowds' chunks index possible =
    [1..9]
        |> flip List.foldl
            possible
            (\n p ->
                chunks p
                    |> List.indexedMap
                        (\c chunk ->
                            chunk
                                |> findIndices (List.member n)
                                |> List.map (index c)
                        )
                    |> List.filter (\xs -> List.length xs == 1)
                    |> List.concat
                    |> flip List.foldl
                        p
                        (\i p ->
                            set i [ n ] p
                        )
            )


eliminateSame : Possible -> Possible
eliminateSame possible =
    possible

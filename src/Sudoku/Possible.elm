module Sudoku.Possible
    exposing
        ( Possible
        , initialize
        , toPuzzle
        , eliminate
        , eliminateUsed
        , eliminateCrowds
        , eliminateSame
        , eliminateAligned
        )

import Set
import Sudoku.Grid exposing (rows, columns, groups)
import List.Extra exposing (removeAt, updateAt, unique, findIndices, elemIndices)
import Util exposing (get, set, diff)


type alias Puzzle =
    List Int


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
toPuzzle possible =
    possible
        |> List.map
            (\xs ->
                case xs of
                    [ n ] ->
                        n
                    _ ->
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
                        |> flip diff (used i puzzle)
                )


used : Int -> Puzzle -> List Int
used i puzzle =
    let
        ( x, y ) =
            ( i % 9, i // 9 )

        row =
            get y [] (rows puzzle) |> removeAt x

        column =
            get x [] (columns puzzle) |> removeAt y

        group =
            let
                g =
                    -- index of group
                    x // 3 + y // 3 * 3

                gi =
                    -- index within group
                    (x % 3) + (y % 3) * 3
            in
                get g [] (groups puzzle) |> removeAt gi
    in
        row ++ column ++ group |> List.filter ((/=) 0) |> unique


eliminateCrowds : Possible -> Possible
eliminateCrowds possible =
    possible
        |> eliminateCrowds' rows coordToIndex
        |> eliminateCrowds' columns coordToIndex
        |> eliminateCrowds' groups groupToIndex


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
        |> eliminateSame' rows (flip coordToIndex)
        |> eliminateSame' columns coordToIndex
        |> eliminateSame' groups groupToIndex


eliminateSame' : (Possible -> List (List (List Int))) -> (Int -> Int -> Int) -> Possible -> Possible
eliminateSame' chunks index possible =
    chunks possible
        |> List.indexedMap
            (\c chunk ->
                chunk
                    |> unique
                    |> List.filterMap
                        (\xs ->
                            let
                                limit =
                                    List.length chunk

                                is =
                                    elemIndices xs chunk
                            in
                                if List.length xs == limit then
                                    Nothing
                                else if List.length xs /= List.length is then
                                    Nothing
                                else
                                    Just
                                        { xs = xs
                                        , is =
                                            is
                                                |> diff [0..(limit - 1)]
                                                |> List.map (index c)
                                        }
                        )
            )
        |> List.concat
        |> flip List.foldl
            possible
            (\{ xs, is } possible -> eliminate xs is possible)


eliminateAligned : Possible -> Possible
eliminateAligned possible =
    [1..9]
        |> flip List.foldl
            possible
            (\n possible ->
                possible
                    |> groups
                    |> List.indexedMap
                        (\g group ->
                            let
                                is =
                                    group
                                        |> findIndices (List.member n)

                                same =
                                    unique >> List.length >> (==) 1

                                row =
                                    (flip (//) 3)

                                column =
                                    (flip (%) 3)
                            in
                                if List.length is == 0 then
                                    []
                                else if is |> List.map row |> same then
                                    let
                                        y =
                                            is
                                                |> List.head
                                                |> Maybe.withDefault 0
                                                |> row
                                                |> (+) ((g // 3) * 3)
                                    in
                                        is
                                            |> List.map
                                                (\i -> i % 3 + g % 3 * 3)
                                            |> diff [0..8]
                                            |> List.map (flip coordToIndex y)
                                else if is |> List.map column |> same then
                                    let
                                        x =
                                            is
                                                |> List.head
                                                |> Maybe.withDefault 0
                                                |> column
                                                |> (+) (g // 3 * 3)
                                    in
                                        is
                                            |> List.map
                                                (\i -> i // 3 + g // 3 * 3)
                                            |> diff [0..8]
                                            |> List.map (coordToIndex x)
                                else
                                    []
                        )
                    |> flip List.foldl
                        possible
                        (\is possible -> eliminate [ n ] is possible)
            )


coordToIndex : Int -> Int -> Int
coordToIndex x y =
    x + y * 9


groupToIndex : Int -> Int -> Int
groupToIndex g i =
    let
        x =
            (i % 3) + (g % 3) * 3

        y =
            (i // 3) + (g // 3) * 3
    in
        coordToIndex x y

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
        , used
        , unused
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
                |> updateAt i
                    (\existing ->
                        let
                            next =
                                diff existing xs
                        in
                            if List.length next == 0 then
                                existing
                            else
                                diff existing xs
                    )
                |> Maybe.withDefault possible
                |> eliminate xs is


eliminateUsed : Possible -> Possible
eliminateUsed possible =
    let
        puzzle =
            toPuzzle possible

        before =
            possible

        after =
            before
                |> List.indexedMap
                    (\i xs ->
                        ( i, used i puzzle )
                    )
                |> flip List.foldl
                    possible
                    (\( i, xs ) possible ->
                        possible
                            |> eliminate xs [ i ]
                    )
    in
        after


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


unused : Int -> Puzzle -> List Int
unused i puzzle =
    used i puzzle |> flip diff [1..9]


eliminateCrowds : Possible -> Possible
eliminateCrowds possible =
    possible
        |> eliminateCrowds' rows (flip coordToIndex)
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
                                else if List.length xs == 1 then
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

                                head =
                                    is
                                        |> List.head
                                        |> Maybe.withDefault 0

                                same =
                                    unique >> List.length >> (==) 1

                                rowInGroup =
                                    (flip (//) 3)

                                rowInPuzzle =
                                    (+) ((g // 3) * 3)

                                indexInRow =
                                    (\i -> i % 3 + (g % 3) * 3)

                                columnInGroup =
                                    (flip (%) 3)

                                columnInPuzzle =
                                    (+) ((g % 3) * 3)

                                indexInColumn =
                                    (\i -> i // 3 + (g // 3) * 3)

                                sameRow =
                                    is |> List.map rowInGroup |> same

                                sameColumn =
                                    is |> List.map columnInGroup |> same
                            in
                                if List.length is <= 1 then
                                    -- eliminateUsed or eliminateCrowds will handle these
                                    []
                                else
                                    let
                                        rowEliminations =
                                            if not sameRow then
                                                []
                                            else
                                                let
                                                    y =
                                                        head
                                                            |> rowInGroup
                                                            |> rowInPuzzle
                                                in
                                                    is
                                                        |> List.map indexInRow
                                                        |> diff [0..8]
                                                        |> List.map (flip coordToIndex y)

                                        columnEliminations =
                                            if not sameColumn then
                                                []
                                            else
                                                let
                                                    x =
                                                        head
                                                            |> columnInGroup
                                                            |> columnInPuzzle
                                                in
                                                    is
                                                        |> List.map indexInColumn
                                                        |> diff [0..8]
                                                        |> List.map (coordToIndex x)
                                    in
                                        rowEliminations ++ columnEliminations
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

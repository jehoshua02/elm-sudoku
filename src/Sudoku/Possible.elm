module Sudoku.Possible
    exposing
        ( Possible
        , initialize
        , toPuzzle
        , eliminate
        , eliminateUsed
        , eliminateCrowds
        )

import Set
import Sudoku.Puzzle exposing (Puzzle)
import Sudoku.Grid exposing (rows, columns, groups)
import List.Extra exposing (getAt, removeAt, updateAt, unique, setAt, findIndices)


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
    possible
        |> eliminateCrowdsInRows
        |> eliminateCrowdsInColumns
        |> eliminateCrowdsInGroups


eliminateCrowdsInRows : Possible -> Possible
eliminateCrowdsInRows possible =
    [1..9] |> flip List.foldl possible
        (\n p ->
            rows p
                |> List.indexedMap
                    (\y row ->
                        row
                            |> findIndices (List.member n)
                            |> List.map (\x -> x + y * 9)
                    )
                |> List.filter (\xs -> List.length xs == 1)
                |> List.concat
                |> flip List.foldl p
                    (\i p ->
                        setAt i [n] p |> Maybe.withDefault p
                    )
        )


eliminateCrowdsInColumns : Possible -> Possible
eliminateCrowdsInColumns possible =
    [1..9] |> flip List.foldl possible
        (\n p ->
            columns p
                |> List.indexedMap
                    (\x column ->
                        column
                            |> findIndices (List.member n)
                            |> List.map (\y -> x + y * 9)
                    )
                |> List.filter (\xs -> List.length xs == 1)
                |> List.concat
                |> flip List.foldl p
                    (\i p ->
                        setAt i [n] p |> Maybe.withDefault p
                    )
        )


eliminateCrowdsInGroups : Possible -> Possible
eliminateCrowdsInGroups possible =
    [1..9] |> flip List.foldl possible
        (\n p ->
            groups p
                |> List.indexedMap
                    (\g group ->
                        group
                            |> findIndices (List.member n)
                            |> List.map
                                (\i ->
                                    let
                                        x =
                                            i % 3 + g % 3 * 3

                                        y =
                                            i // 3 + g // 3 * 3
                                    in
                                        x + y * 9
                                )
                    )
                |> List.filter (\xs -> List.length xs == 1)
                |> List.concat
                |> flip List.foldl p
                    (\i p ->
                        setAt i [n] p |> Maybe.withDefault p
                    )
        )


get : Int -> a -> List a -> a
get i d xs =
    getAt i xs |> Maybe.withDefault d


diff : List comparable -> List comparable -> List comparable
diff a b =
    Set.diff (Set.fromList a) (Set.fromList b) |> Set.toList

module Sudoku.Possible
    exposing
        ( Possible
        , initialize
        , eliminateUsed
        )

import Set
import Sudoku.Puzzle exposing (Puzzle, rows, columns, groups)
import List.Extra exposing (getAt, removeAt, unique)


type alias Possible =
    List (List Int)


initialize : Possible
initialize =
    List.repeat (9 * 9) [1..9]


eliminateUsed : Puzzle -> Possible -> Possible
eliminateUsed puzzle possible =
    puzzle |> List.indexedMap
        (\i v ->
            if v /= 0 then
                [v]
            else
                possible
                    |> getAt i
                    |> Maybe.withDefault []
                    |> Set.fromList
                    |> flip Set.diff (Set.fromList (used i puzzle))
                    |> Set.toList
        )


used : Int -> Puzzle -> List Int
used i puzzle =
    let
        (x, y) =
            (i % 9, i // 9)

        row =
            getAt x (rows puzzle) |> Maybe.map (removeAt y)

        column =
            getAt y (columns puzzle) |> Maybe.map (removeAt x)

        group =
            let
                g =
                    -- index of group
                    x // 3 + y // 3 * 3

                i =
                    -- index within group
                    (x % 3) // 3 + (y % 3) * 3
            in
                getAt g (groups puzzle) |> Maybe.map (removeAt i)
    in
        Maybe.map3 (\a b c -> a ++ b ++ c) row column group
            |> Maybe.withDefault []
            |> List.filter ((/=) 0)
            |> unique

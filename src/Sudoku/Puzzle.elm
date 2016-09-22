module Sudoku.Puzzle
    exposing
        ( Puzzle
        , empty
        , fromList
        , Error(..)
        , solved
        , valid
        , complete
        , solve
        )

import Set
import Sudoku.Grid exposing (rows, columns, groups)
import Sudoku.Possible as Possible


type alias Puzzle =
    List Int


type Error
    = InvalidLength
    | OutOfRange
    | Unsolvable


empty : Puzzle
empty =
    List.repeat (9 * 9) 0


fromList : List Int -> Result Error Puzzle
fromList xs =
    if List.length xs /= 9 * 9 then
        Err InvalidLength
    else if List.any (\n -> n < 0 || n > 9) xs then
        Err OutOfRange
    else
        Ok xs


solved : Puzzle -> Bool
solved puzzle =
    valid puzzle && complete puzzle


valid : Puzzle -> Bool
valid xs =
    rows xs
        ++ columns xs
        ++ groups xs
        |> List.all
            (\chunk ->
                let
                    filled =
                        chunk
                            |> List.filter ((/=) 0)
                            |> List.sort

                    unique =
                        filled
                            |> Set.fromList
                            |> Set.toList
                in
                    filled == unique
            )


complete : Puzzle -> Bool
complete xs =
    xs |> List.all ((/=) 0)


solve : Puzzle -> Result Error Puzzle
solve puzzle =
    if solved puzzle then
        Ok puzzle
    else if puzzle |> List.all ((/=) 0) then
        Err Unsolvable
    else
        let
            before =
                puzzle

            after =
                before
                    |> Possible.initialize
                    |> Possible.eliminateUsed
                    |> Possible.eliminateCrowds
                    --|> Possible.eliminateSame
                    --|> Possible.eliminateAligned
                    |>
                        Possible.toPuzzle
        in
            if before == after then
                Err Unsolvable
            else
                after
                    |> solve

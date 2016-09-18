module Sudoku.Puzzle
    exposing
        ( Puzzle
        , empty
        , fromList
        , Error(..)
        , solved
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
solved xs =
    rows xs ++ columns xs ++ groups xs |> List.all (List.sort >> (==) [1..9])


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
                    |> Possible.initialize

            after =
                before
                    |> Possible.eliminateUsed
                    |> Possible.eliminateCrowds
                    |> Possible.eliminateSame
                    |> Possible.eliminateAligned
        in
            if before == after then
                Err Unsolvable
            else if after |> List.any ((==) []) then
                Err Unsolvable
            else
                after
                    |> Possible.toPuzzle
                    |> solve

module Sudoku.Puzzle
    exposing
        ( Puzzle
        , empty
        , fromList
        , Error(..)
        , solved
        )

import Set
import Sudoku.Grid exposing (rows, columns, groups)


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

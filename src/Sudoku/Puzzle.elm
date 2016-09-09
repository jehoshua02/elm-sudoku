module Sudoku.Puzzle
    exposing
        ( Puzzle
        , empty
        , fromList
        , Error(..)
        , rows
        , columns
        , groups
        , solved
        )

import List.Extra exposing (groupsOf, transpose, getAt, removeAt)
import Set


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


rows : Puzzle -> List (List Int)
rows =
    groupsOf 9


columns : Puzzle -> List (List Int)
columns =
    rows >> transpose


groups : Puzzle -> List (List Int)
groups =
    -- separate rows into 3 sections
    rows
        >> groupsOf 3
        -- chop sections into parts
        >>
            List.concatMap transpose
        -- group parts to make sudoku groups
        >>
            groupsOf 3
        -- restore original left-right/top-bottom orientation within group
        >>
            List.map transpose
        -- finally combine 3 value parts
        >>
            List.map List.concat


solved : Puzzle -> Bool
solved xs =
    rows xs ++ columns xs ++ groups xs |> List.all (List.sort >> (==) [1..9])

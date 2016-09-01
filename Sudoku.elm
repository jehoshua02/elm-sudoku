module Sudoku exposing (fromList, Errors(..), solved, rows, columns, groups)

import List.Extra exposing (groupsOf, transpose)


type alias Coord =
    ( Int, Int )


type alias Value =
    Int


type alias Puzzle =
    List Value


type Errors
    = InvalidLength
    | OutOfRange


fromList : List Int -> Result Errors Puzzle
fromList xs =
    if List.length xs /= 9 * 9 then
        Err InvalidLength
    else if List.any (\n -> n < 0 || n > 9) xs then
        Err OutOfRange
    else
        Ok xs


solved : Puzzle -> Bool
solved xs =
    rows xs ++ columns xs ++ groups xs |> List.map List.sort |> Debug.log "wat" |> List.all valid


rows : Puzzle -> List (List Value)
rows =
    groupsOf 9


columns : Puzzle -> List (List Value)
columns =
    rows >> transpose


groups : Puzzle -> List (List Value)
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


valid : List Value -> Bool
valid =
    List.sort >> (==) [1..9]

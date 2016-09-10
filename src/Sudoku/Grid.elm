module Sudoku.Grid
    exposing
        ( rows
        , columns
        , groups
        )

import List.Extra exposing (groupsOf, transpose)


rows : List a -> List (List a)
rows =
    groupsOf 9


columns : List a -> List (List a)
columns =
    rows >> transpose


groups : List a -> List (List a)
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

module Sudoku exposing (fromList, Error(..), solved, possible, rows, columns, groups, coordToGroup)

import List.Extra exposing (groupsOf, transpose)
import Set


type alias Coord =
    ( Int, Int )


type alias Value =
    Int


type alias Puzzle =
    List Value


type Error
    = InvalidLength
    | OutOfRange
    | Unsolvable


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
    rows xs ++ columns xs ++ groups xs |> List.all valid


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


possible : Coord -> Puzzle -> Result Error (List Int)
possible ( x, y ) p =
    if (x < 0 || 8 < x || y < 0 || 8 < y) then
        Err OutOfRange
    else
        let
            row =
                select x (rows p) |> Maybe.map (others y)

            column =
                select y (columns p) |> Maybe.map (others x)

            group =
                let
                    z =
                        coordToGroup ( x, y )

                    i =
                        coordToIndex ( x % 3, y % 3 )
                in
                    select z (groups p) |> Maybe.map (others i)
        in
            Maybe.map3 (\a b c -> a ++ b ++ c) row column group
                |> Maybe.withDefault []
                |> Set.fromList
                |> Set.diff (Set.fromList [1..9])
                |> Set.toList
                |> Ok


select : Int -> List a -> Maybe a
select n xs =
    xs |> List.drop (n - 1) |> List.head


others : Int -> List a -> List a
others i xs =
    List.take (i - 1) xs ++ List.drop (i + 1) xs


coordToGroup : Coord -> Int
coordToGroup ( x, y ) =
    x // 3 + y // 3 * 3


coordToIndex : Coord -> Int
coordToIndex ( x, y ) =
    x // 3 + y * 3

port module Sudoku.PossibleTests exposing (..)

import Test exposing (..)
import Expect
import Sudoku.Possible as Possible
import Sudoku.Puzzle as Puzzle
import List.Extra exposing (setAt, removeAt)


tests : Test
tests =
    describe "Possible"
        [ describe "toPuzzle"
            [ test "should convert possibilities to puzzle" <|
                \() ->
                    let
                        possible =
                            List.repeat (9 * 9) [1..9]
                                |> set 0 [2]
                                |> set 10 [7]

                        actual =
                            Possible.toPuzzle possible

                        expected =
                            Puzzle.empty
                                |> set 0 2
                                |> set 10 7
                    in
                        Expect.equal expected actual
            ]
        , describe "initialize"
            [ test "should return a new list of possibilities" <|
                \() ->
                    let
                        actual =
                            Possible.initialize Puzzle.empty

                        expected =
                            List.repeat (9 * 9) [1..9]
                    in
                        Expect.equal expected actual
            , test "should assume one possibility for filled spaces" <|
                \() ->
                    let
                        puzzle =
                            Puzzle.empty
                                |> set 0 2

                        actual =
                            Possible.initialize puzzle

                        expected =
                            List.repeat (9 * 9) [1..9]
                                |> set 0 [ 2 ]
                    in
                        Expect.equal expected actual
            ]
        , describe "eliminateUsed"
            [ test "should eliminate nothing" <|
                \() ->
                    let
                        expected =
                            Possible.initialize Puzzle.empty

                        actual =
                            Possible.eliminateUsed expected
                    in
                        Expect.equal expected actual
            , test "should eliminate used numbers" <|
                \() ->
                    let
                        puzzle =
                            Puzzle.empty
                                |> set 0 2

                        possible =
                            Possible.initialize puzzle

                        actual =
                            Possible.eliminateUsed possible

                        allBut2 =
                            removeAt 1 [1..9]

                        expected =
                            possible
                                |> set 0 [ 2 ]
                                -- remove 2 from row
                                |>
                                    setAll [1..8] allBut2
                                -- remove 2 from column
                                |>
                                    setAll ([1..8] |> List.map ((*) 9)) allBut2
                                -- remove 2 from rest of group
                                |>
                                    setAll [ 10, 11, 19, 20 ] allBut2
                    in
                        Expect.equal expected actual
            , test "should preserve existing eliminations" <|
                \() ->
                    let
                        puzzle =
                            Puzzle.empty
                                |> set 0 2

                        possible =
                            Possible.initialize puzzle
                                -- let's set the last one to something arbitrary
                                |>
                                    set (9 * 9 - 1) [ 5, 6 ]

                        actual =
                            Possible.eliminateUsed possible

                        allBut2 =
                            removeAt 1 [1..9]

                        expected =
                            possible
                                |> set 0 [ 2 ]
                                -- remove 2 from row
                                |>
                                    setAll [1..8] allBut2
                                -- remove 2 from column
                                |>
                                    setAll ([1..8] |> List.map ((*) 9)) allBut2
                                -- remove 2 from rest of group
                                |>
                                    setAll [ 10, 11, 19, 20 ] allBut2
                    in
                        Expect.equal expected actual
            ]
        , describe "eliminateCrowds"
            [ test "should eliminate nothing" <|
                \() ->
                    let
                        possible =
                            Possible.initialize Puzzle.empty

                        actual =
                            Possible.eliminateCrowds possible

                        expected =
                            possible
                    in
                        Expect.equal expected actual
            --, test "should eliminate crowds"
            --, test "should preserve existing eliminations"
            ]
        ]


setAll : List Int -> a -> List a -> List a
setAll is x xs =
    is |> List.foldl (\i b -> set i x b) xs


set : Int -> a -> List a -> List a
set i x xs =
    setAt i x xs |> Maybe.withDefault xs

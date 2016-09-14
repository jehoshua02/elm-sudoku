port module Sudoku.PossibleTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Possible as Possible exposing (eliminate)
import Sudoku.Puzzle as Puzzle
import Util exposing (set)


tests : Test
tests =
    describe "Possible"
        [ describe "toPuzzle"
            [ test "should convert possibilities to puzzle" <|
                \() ->
                    let
                        possible =
                            List.repeat (9 * 9) [1..9]
                                |> set 0 [ 2 ]
                                |> set 10 [ 7 ]

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
        , describe "eliminate"
            [ test "should eliminate number from a specific location" <|
                \() ->
                    let
                        possible =
                            Possible.initialize Puzzle.empty

                        actual =
                            Possible.eliminate [ 5 ] [ 0 ] possible

                        expected =
                            possible
                                |> set 0 [ 1, 2, 3, 4, 6, 7, 8, 9 ]
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

                        expected =
                            possible
                                |> set 0 [ 2 ]
                                |> eliminate [ 2 ] [1..8]
                                |> eliminate [ 2 ] ([1..8] |> List.map ((*) 9))
                                |> eliminate [ 2 ] [ 10, 11, 19, 20 ]
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

                        expected =
                            possible
                                |> set 0 [ 2 ]
                                |> eliminate [ 2 ] [1..8]
                                |> eliminate [ 2 ] ([1..8] |> List.map ((*) 9))
                                |> eliminate [ 2 ] [ 10, 11, 19, 20 ]
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
            , test "should eliminate crowds" <|
                \() ->
                    let
                        possible =
                            Possible.initialize Puzzle.empty
                                -- eliminate 7 in a row, except for one
                                |>
                                    eliminate [ 7 ] [1..8]
                                -- eliminate 2 in column, except for one
                                |>
                                    eliminate [ 2 ] [ 10, 19, 28, 37, 46, 55, 64, 73 ]
                                -- eliminate 5 in group, except for one
                                |>
                                    eliminate [ 5 ] [ 0, 1, 9, 10, 11, 18, 19, 20 ]

                        actual =
                            Possible.eliminateCrowds possible

                        expected =
                            possible
                                |> set 0 [ 7 ]
                                |> set 1 [ 2 ]
                                |> set 2 [ 5 ]
                    in
                        Expect.equal expected actual
            ]
        , describe "eliminateSame"
            [ test "should eliminate nothing" <|
                \() ->
                    let
                        possible =
                            Possible.initialize Puzzle.empty

                        actual =
                            Possible.eliminateSame possible

                        expected =
                            possible
                    in
                        Expect.equal expected actual
            , test "should eliminate same possibilities" <|
                \() ->
                    let
                        possible =
                            Possible.initialize Puzzle.empty
                                -- row, size 2
                                |>
                                    set 3 [ 1, 2 ]
                                |> set 8 [ 1, 2 ]
                                -- group size 3
                                |>
                                    set 60 [ 3, 4, 5 ]
                                |> set 70 [ 3, 4, 5 ]
                                |> set 80 [ 3, 4, 5 ]
                                -- column, size 4
                                |>
                                    set 0 [ 6, 7, 8, 9 ]
                                |> set 9 [ 6, 7, 8, 9 ]
                                |> set 18 [ 6, 7, 8, 9 ]
                                |> set 27 [ 6, 7, 8, 9 ]

                        actual =
                            Possible.eliminateSame possible

                        expected =
                            possible
                                |> eliminate [ 1, 2 ] [ 0, 1, 2, 4, 5, 6, 7 ]
                                |> eliminate [ 3, 4, 5 ] [ 61, 62, 69, 71, 78, 79 ]
                                |> eliminate [ 6, 7, 8, 9 ] [ 36, 45, 54, 63, 72 ]
                    in
                        Expect.equal expected actual
            ]
        , describe "eliminateAligned"
            [ test "should eliminate nothing" <|
                \() ->
                    let
                        possible =
                            Possible.initialize Puzzle.empty

                        actual =
                            Possible.eliminateAligned possible

                        expected =
                            possible
                    in
                        Expect.equal expected actual
            , test "should eliminate aligned possibilities" <|
                \() ->
                    let
                        possible =
                            Possible.initialize Puzzle.empty
                                -- first row, first column in first group
                                |>
                                    eliminate [ 3 ] [ 9, 10, 11, 18, 19, 20 ]
                                |> eliminate [ 4 ] [ 1, 10, 19, 2, 11, 20 ]
                                -- second row in second group
                                |>
                                    eliminate [ 5 ] [ 3, 4, 5, 12, 21, 22, 23 ]
                                -- second column in fourth group
                                |>
                                    eliminate [ 6 ] [ 30, 32, 39, 40, 41, 48, 50 ]

                        actual =
                            Possible.eliminateAligned possible

                        expected =
                            possible
                                |> eliminate [ 3 ] [ 3, 4, 5, 6, 7, 8 ]
                                |> eliminate [ 4 ] [ 27, 36, 45, 54, 63, 72 ]
                                |> eliminate [ 5 ] [ 9, 10, 11, 15, 16, 17 ]
                                |> eliminate [ 6 ] [ 4, 13, 22, 58, 67, 76 ]
                    in
                        Expect.equal expected actual
            ]
        ]

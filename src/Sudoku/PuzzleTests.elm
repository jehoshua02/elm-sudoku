port module Sudoku.PuzzleTests exposing (..)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))


tests : Test
tests =
    describe "Puzzle"
        [ describe "fromList"
            [ test "should error a short list" <|
                \() ->
                    Expect.equal (Err InvalidLength) (Puzzle.fromList [ 1, 2, 3 ])
            , test "should error a long list" <|
                \() ->
                    let
                        xs =
                            [ 5
                            , 2,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Err InvalidLength) (Puzzle.fromList xs)
            , test "should error on values greater than 9" <|
                \() ->
                    let
                        xs =
                            [10,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Err OutOfRange) (Puzzle.fromList xs)
            , test "should error on values less than 0" <|
                \() ->
                    let
                        xs =
                            [-1,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Err OutOfRange) (Puzzle.fromList xs)
            , test "should pass" <|
                \() ->
                    let
                        xs =
                            [ 0,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok xs) (Puzzle.fromList xs)
            ]
        , describe "rows"
            [ test "should return rows" <|
                \() ->
                    let
                        puzzle = Puzzle.fromList
                            [ 2,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                        rows =
                            [ [ 2, 9, 5, 7, 4, 3, 8, 6, 1 ]
                            , [ 4, 3, 1, 8, 6, 5, 9, 2, 7 ]
                            , [ 8, 7, 6, 1, 9, 2, 5, 4, 3 ]
                            , [ 3, 8, 7, 4, 5, 9, 2, 1, 6 ]
                            , [ 6, 1, 2, 3, 8, 7, 4, 9, 5 ]
                            , [ 5, 4, 9, 2, 1, 6, 7, 3, 8 ]
                            , [ 7, 6, 3, 5, 3, 4, 1, 8, 9 ]
                            , [ 9, 2, 8, 6, 7, 1, 3, 5, 4 ]
                            , [ 1, 5, 4, 9, 3, 8, 6, 7, 2 ]
                            ]
                    in
                        Expect.equal (Ok rows) (Result.map Puzzle.rows puzzle)
            ]
        , describe "columns"
            [ test "should return columns" <|
                \() ->
                    let
                        puzzle = Puzzle.fromList
                            [ 2,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                        columns =
                            [ [ 2, 4, 8, 3, 6, 5, 7, 9, 1 ]
                            , [ 9, 3, 7, 8, 1, 4, 6, 2, 5 ]
                            , [ 5, 1, 6, 7, 2, 9, 3, 8, 4 ]
                            , [ 7, 8, 1, 4, 3, 2, 5, 6, 9 ]
                            , [ 4, 6, 9, 5, 8, 1, 3, 7, 3 ]
                            , [ 3, 5, 2, 9, 7, 6, 4, 1, 8 ]
                            , [ 8, 9, 5, 2, 4, 7, 1, 3, 6 ]
                            , [ 6, 2, 4, 1, 9, 3, 8, 5, 7 ]
                            , [ 1, 7, 3, 6, 5, 8, 9, 4, 2 ]
                            ]
                    in
                        Expect.equal (Ok columns) (Result.map Puzzle.columns puzzle)
            ]
        , describe "groups"
            [ test "should return groups" <|
                \() ->
                    let
                        puzzle = Puzzle.fromList
                            [ 2,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                        groups =
                            [ [ 2, 9, 5, 4, 3, 1, 8, 7, 6 ]
                            , [ 7, 4, 3, 8, 6, 5, 1, 9, 2 ]
                            , [ 8, 6, 1, 9, 2, 7, 5, 4, 3 ]
                            , [ 3, 8, 7, 6, 1, 2, 5, 4, 9 ]
                            , [ 4, 5, 9, 3, 8, 7, 2, 1, 6 ]
                            , [ 2, 1, 6, 4, 9, 5, 7, 3, 8 ]
                            , [ 7, 6, 3, 9, 2, 8, 1, 5, 4 ]
                            , [ 5, 3, 4, 6, 7, 1, 9, 3, 8 ]
                            , [ 1, 8, 9, 3, 5, 4, 6, 7, 2 ]
                            ]
                    in
                        Expect.equal (Ok groups) (Result.map Puzzle.groups puzzle)
            ]
        , describe "solved"
            [ test "should say this puzzle is not solved" <|
                \() ->
                    let
                        puzzle = Puzzle.fromList
                            [ 0,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok False) (Result.map Puzzle.solved puzzle)
            , test "should say this puzzle is solved" <|
                \() ->
                    let
                        puzzle = Puzzle.fromList
                            [ 2,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,2,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok True) (Result.map Puzzle.solved puzzle)
            ]
        ]

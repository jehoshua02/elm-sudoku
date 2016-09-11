port module Sudoku.PuzzleTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))


tests : Test
tests =
    describe "Puzzle"
        [ describe "empty"
            [ test "should return empty puzzle" <|
                \() ->
                    Expect.equal (List.repeat (9 * 9) 0) Puzzle.empty
            ]
        , describe "fromList"
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
        , describe "solved"
            [ test "should say this puzzle is not solved" <|
                \() ->
                    let
                        puzzle =
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
                        Expect.equal False (Puzzle.solved puzzle)
            , test "should say this puzzle is solved" <|
                \() ->
                    let
                        puzzle =
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
                        Expect.equal True (Puzzle.solved puzzle)
            ]
        ]

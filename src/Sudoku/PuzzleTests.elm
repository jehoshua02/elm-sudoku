port module Sudoku.PuzzleTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))
import Util exposing (set)


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
                    Expect.equal (Err InvalidLength) (Puzzle.fromList (5 :: solvedPuzzle))
            , test "should error on values greater than 9" <|
                \() ->
                    Expect.equal (Err OutOfRange) (Puzzle.fromList (solvedPuzzle |> set 0 10))
            , test "should error on values less than 0" <|
                \() ->
                    Expect.equal (Err OutOfRange) (Puzzle.fromList (solvedPuzzle |> set 0 -1))
            , test "should pass" <|
                \() ->
                    Expect.equal (Ok solvedPuzzle) (Puzzle.fromList solvedPuzzle)
            ]
        , describe "solved"
            [ test "should say this puzzle is not solved" <|
                \() ->
                    Expect.equal False (Puzzle.solved (solvedPuzzle |> set 0 0))
            , test "should say this puzzle is solved" <|
                \() ->
                    Expect.equal True (Puzzle.solved solvedPuzzle)
            ]
        , describe "valid"
            [ test "should say this puzzle is invalid" <|
                \() ->
                    let
                        puzzle =
                            solvedPuzzle
                                |> set 49 4
                    in
                        Expect.equal False (Puzzle.valid puzzle)
            , test "should say this puzzle is valid" <|
                \() ->
                    Expect.equal True (Puzzle.valid solvedPuzzle)
            , test "should say all these puzzles are invalid" <|
                \() ->
                    let
                        puzzles =
                            [ solvedPuzzle |> set 0 9
                            , solvedPuzzle |> set 49 4
                            , solvedPuzzle |> set 80 9
                            ]

                        actual =
                            puzzles
                                |> List.map Puzzle.valid
                                |> List.all ((==) False)
                    in
                        Expect.equal True actual
            ]
        , describe "complete"
            [ test "should say this puzzle is complete" <|
                \() ->
                    Expect.equal True (Puzzle.complete solvedPuzzle)
            , test "should say this puzzle is incomplete" <|
                \() ->
                    let
                        puzzle =
                            solvedPuzzle
                                |> set 45 0
                    in
                        Expect.equal False (Puzzle.complete puzzle)
            ]
        , describe "solve"
            [ test "should say this puzzle is already solved" <|
                \() ->
                    Expect.equal (Ok solvedPuzzle) (Puzzle.solve solvedPuzzle)
            , test "should say complete puzzle with error is unsolvable" <|
                \() ->
                    let
                        puzzle =
                            solvedPuzzle |> set 0 9
                    in
                        Expect.equal (Err Unsolvable) (Puzzle.solve puzzle)
            , test "should say incomplete puzzle with error is unsolvable" <|
                \() ->
                    let
                        puzzle =
                            solvedPuzzle
                                |> set 0 0
                                |> set 1 5
                    in
                        Expect.equal (Err Unsolvable) (Puzzle.solve puzzle)
            , test "should solve valid, almost complete puzzle" <|
                \() ->
                    let
                        puzzle =
                            solvedPuzzle |> set 0 0
                    in
                        Expect.equal (Ok solvedPuzzle) (Puzzle.solve puzzle)
            ]
        ]


solvedPuzzle : Puzzle.Puzzle
solvedPuzzle =
    {-
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
    -}
    [ 2, 9, 5, 7, 4, 3, 8, 6, 1, 4, 3, 1, 8, 6, 5, 9, 2, 7, 8, 7, 6, 1, 9, 2, 5, 4, 3, 3, 8, 7, 4, 5, 9, 2, 1, 6, 6, 1, 2, 3, 8, 7, 4, 9, 5, 5, 4, 9, 2, 1, 6, 7, 3, 8, 7, 6, 3, 5, 2, 4, 1, 8, 9, 9, 2, 8, 6, 7, 1, 3, 5, 4, 1, 5, 4, 9, 3, 8, 6, 7, 2 ]

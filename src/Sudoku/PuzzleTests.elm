port module Sudoku.PuzzleTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))
import List.Extra exposing (setAt)


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
        ]


solvedPuzzle : Puzzle.Puzzle
solvedPuzzle =
    {- elm-format butchers this so I put in comment
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


set : Int -> a -> List a -> List a
set i x xs =
    setAt i x xs |> Maybe.withDefault xs

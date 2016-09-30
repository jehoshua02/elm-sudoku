port module Sudoku.PuzzleMakeTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))


limit : Int
limit =
    100


tests : Test
tests =
    describe "Puzzle.make"
        ([0..limit]
            |> List.map
                (\n ->
                    let
                        { puzzle, solution } =
                            Puzzle.make ((toFloat n) / (toFloat limit))
                    in
                        describe ("puzzle " ++ toString n)
                            [ describe "solution"
                                [ test "should be valid" <|
                                    \() ->
                                        Expect.equal True (Puzzle.valid solution)
                                , test "should be complete" <|
                                    \() ->
                                        Expect.equal True (Puzzle.complete solution)
                                ]
                            , describe "puzzle"
                                [ test "should be valid" <|
                                    \() ->
                                        Expect.equal True (Puzzle.valid puzzle)
                                , test "should be incomplete" <|
                                    \() ->
                                        Expect.equal False (Puzzle.complete puzzle)
                                , test "should be solvable" <|
                                    \() ->
                                        Expect.equal (Ok solution) (Puzzle.solve puzzle)
                                ]
                            ]
                )
        )

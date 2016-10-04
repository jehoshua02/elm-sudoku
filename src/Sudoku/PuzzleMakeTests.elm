port module Sudoku.PuzzleMakeTests exposing (tests)

import Test exposing (..)
import Expect
import Fuzz
import Sudoku.Puzzle as Puzzle exposing (Error(..))


tests : Test
tests =
    describe "Puzzle.make"
        [ fuzz Fuzz.percentage "should valid, solvable puzzles" <|
            (\p ->
                let
                    { puzzle, solution } =
                        Puzzle.make (Debug.log "p" p)

                    actual =
                        { solution =
                            { valid = Puzzle.valid solution
                            , complete = Puzzle.complete solution
                            , solved = Puzzle.solved solution
                            }
                        , puzzle =
                            { valid = Puzzle.valid puzzle
                            , complete = Puzzle.complete puzzle
                            , solved = Puzzle.solved puzzle
                            , solve = Puzzle.solve puzzle
                            }
                        }

                    expected =
                        { solution =
                            { valid = True
                            , complete = True
                            , solved = True
                            }
                        , puzzle =
                            { valid = True
                            , complete = False
                            , solved = False
                            , solve = Ok solution
                            }
                        }
                in
                    Expect.equal expected actual
            )
        ]

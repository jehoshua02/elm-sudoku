port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)

import Sudoku.PuzzleTests
import Sudoku.PossibleTests
import Sudoku.GridTests
import Sudoku.PuzzleSolveTests
import Sudoku.EvilPuzzleTests
import Sudoku.PuzzleMakeTests


tests : Test
tests =
    describe "Sudoku"
        [ Sudoku.GridTests.tests
        , Sudoku.PuzzleTests.tests
        , Sudoku.PossibleTests.tests
        , Sudoku.PuzzleSolveTests.tests
        , Sudoku.EvilPuzzleTests.tests
        , Sudoku.PuzzleMakeTests.tests
        ]


main : Program Value
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg

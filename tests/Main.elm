port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)

import Sudoku.PuzzleTests
import Sudoku.PossibleTests
import Sudoku.GridTests


tests : Test
tests =
    describe "Sudoku"
        [ Sudoku.GridTests.tests
        , Sudoku.PuzzleTests.tests
        , Sudoku.PossibleTests.tests
        ]


main : Program Value
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg

port module Sudoku.PossibleTests exposing (..)

import Test exposing (..)
import Expect
import Sudoku.Possible as Possible
import Sudoku.Puzzle as Puzzle


tests : Test
tests =
    describe "Possible"
        [ describe "initialize"
            [ test "should return a new list of possibilities" <|
                \() ->
                    Expect.equal (List.repeat (9 * 9) [1..9]) (Possible.initialize)
            ]
        ]

port module Sudoku.PuzzleSolveTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))
import Util exposing (set)


tests : Test
tests =
    describe "Puzzle"
        [ describe "solve"
            ([
                { id = "Sudoku #001 (Easy)"
                , puzzle =
                    [1,3,0,2,0,0,7,4,0
                    ,0,2,5,0,1,0,0,0,0
                    ,4,8,0,0,6,0,0,5,0
                    ,0,0,0,7,8,0,2,1,0
                    ,5,0,0,0,9,0,3,7,0
                    ,9,0,0,0,3,0,0,0,5
                    ,0,4,0,0,0,6,8,9,0
                    ,0,5,3,0,0,1,4,0,0
                    ,6,0,0,0,0,0,0,0,0
                    ]
                , solution =
                    [1,3,6,2,5,9,7,4,8
                    ,7,2,5,4,1,8,9,3,6
                    ,4,8,9,3,6,7,1,5,2
                    ,3,6,4,7,8,5,2,1,9
                    ,5,1,8,6,9,2,3,7,4
                    ,9,7,2,1,3,4,6,8,5
                    ,2,4,1,5,7,6,8,9,3
                    ,8,5,3,9,2,1,4,6,7
                    ,6,9,7,8,4,3,5,2,1
                    ]
                }
            ] |> List.map
                (\puzzle ->
                    test ("should solve " ++ puzzle.id) <|
                        \() ->
                            Expect.equal (Ok puzzle.solution) (Puzzle.solve puzzle.puzzle)
                )
            )
        ]

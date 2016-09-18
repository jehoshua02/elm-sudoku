port module Sudoku.PuzzleSolveTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))
import Util exposing (set)


tests : Test
tests =
    describe "Puzzle"
        [ describe "solve"
            ([ { id = "Sudoku #001 (Easy)"
               , puzzle =
                    {-
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
                    -}
                    [ 1, 3, 0, 2, 0, 0, 7, 4, 0, 0, 2, 5, 0, 1, 0, 0, 0, 0, 4, 8, 0, 0, 6, 0, 0, 5, 0, 0, 0, 0, 7, 8, 0, 2, 1, 0, 5, 0, 0, 0, 9, 0, 3, 7, 0, 9, 0, 0, 0, 3, 0, 0, 0, 5, 0, 4, 0, 0, 0, 6, 8, 9, 0, 0, 5, 3, 0, 0, 1, 4, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0 ]
               , solution =
                    {-
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
                    -}
                    [ 1, 3, 6, 2, 5, 9, 7, 4, 8, 7, 2, 5, 4, 1, 8, 9, 3, 6, 4, 8, 9, 3, 6, 7, 1, 5, 2, 3, 6, 4, 7, 8, 5, 2, 1, 9, 5, 1, 8, 6, 9, 2, 3, 7, 4, 9, 7, 2, 1, 3, 4, 6, 8, 5, 2, 4, 1, 5, 7, 6, 8, 9, 3, 8, 5, 3, 9, 2, 1, 4, 6, 7, 6, 9, 7, 8, 4, 3, 5, 2, 1 ]
               }
             , { id = "Sudoku #002 (Easy)"
               , puzzle =
                    {-
                       [1,0,0,0,0,0,2,7,6
                       ,0,0,9,1,4,0,0,0,0
                       ,0,2,0,0,0,6,0,9,1
                       ,0,8,0,0,0,9,6,1,0
                       ,7,3,0,0,8,4,0,0,0
                       ,0,0,2,0,0,5,0,8,0
                       ,5,0,6,0,0,3,0,0,0
                       ,0,0,7,0,0,0,0,5,0
                       ,3,4,0,5,9,0,0,0,0
                       ]
                    -}
                    [ 1, 0, 0, 0, 0, 0, 2, 7, 6, 0, 0, 9, 1, 4, 0, 0, 0, 0, 0, 2, 0, 0, 0, 6, 0, 9, 1, 0, 8, 0, 0, 0, 9, 6, 1, 0, 7, 3, 0, 0, 8, 4, 0, 0, 0, 0, 0, 2, 0, 0, 5, 0, 8, 0, 5, 0, 6, 0, 0, 3, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 5, 0, 3, 4, 0, 5, 9, 0, 0, 0, 0 ]
               , solution =
                    {-
                       [1,5,4,9,3,8,2,7,6
                       ,6,7,9,1,4,2,8,3,5
                       ,8,2,3,7,5,6,4,9,1
                       ,4,8,5,2,7,9,6,1,3
                       ,7,3,1,6,8,4,5,2,9
                       ,9,6,2,3,1,5,7,8,4
                       ,5,1,6,8,2,3,9,4,7
                       ,2,9,7,4,6,1,3,5,8
                       ,3,4,8,5,9,7,1,6,2
                       ]
                    -}
                    [ 1, 5, 4, 9, 3, 8, 2, 7, 6, 6, 7, 9, 1, 4, 2, 8, 3, 5, 8, 2, 3, 7, 5, 6, 4, 9, 1, 4, 8, 5, 2, 7, 9, 6, 1, 3, 7, 3, 1, 6, 8, 4, 5, 2, 9, 9, 6, 2, 3, 1, 5, 7, 8, 4, 5, 1, 6, 8, 2, 3, 9, 4, 7, 2, 9, 7, 4, 6, 1, 3, 5, 8, 3, 4, 8, 5, 9, 7, 1, 6, 2 ]
               }
             , { id = "Sudoku #001 (Medium)"
               , puzzle =
                    {-
                       [8,9,2,0,0,3,0,1,4
                       ,0,0,0,0,0,0,0,0,0
                       ,0,0,0,0,6,8,0,7,0
                       ,4,5,0,0,8,0,0,0,1
                       ,0,0,8,0,0,0,2,0,0
                       ,1,0,3,7,0,0,5,0,0
                       ,0,7,1,0,0,6,0,5,0
                       ,5,0,9,2,0,0,0,8,0
                       ,6,0,0,0,0,7,0,0,9
                       ]
                    -}
                    [ 8, 9, 2, 0, 0, 3, 0, 1, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 8, 0, 7, 0, 4, 5, 0, 0, 8, 0, 0, 0, 1, 0, 0, 8, 0, 0, 0, 2, 0, 0, 1, 0, 3, 7, 0, 0, 5, 0, 0, 0, 7, 1, 0, 0, 6, 0, 5, 0, 5, 0, 9, 2, 0, 0, 0, 8, 0, 6, 0, 0, 0, 0, 7, 0, 0, 9 ]
               , solution =
                    {-
                       [8,9,2,5,7,3,6,1,4
                       ,7,4,6,9,2,1,8,3,5
                       ,3,1,5,4,6,8,9,7,2
                       ,4,5,7,6,8,2,3,9,1
                       ,9,6,8,1,3,5,2,4,7
                       ,1,2,3,7,4,9,5,6,8
                       ,2,7,1,8,9,6,4,5,3
                       ,5,3,9,2,1,4,7,8,6
                       ,6,8,4,3,5,7,1,2,9
                       ]
                    -}
                    [ 8, 9, 2, 5, 7, 3, 6, 1, 4, 7, 4, 6, 9, 2, 1, 8, 3, 5, 3, 1, 5, 4, 6, 8, 9, 7, 2, 4, 5, 7, 6, 8, 2, 3, 9, 1, 9, 6, 8, 1, 3, 5, 2, 4, 7, 1, 2, 3, 7, 4, 9, 5, 6, 8, 2, 7, 1, 8, 9, 6, 4, 5, 3, 5, 3, 9, 2, 1, 4, 7, 8, 6, 6, 8, 4, 3, 5, 7, 1, 2, 9 ]
               }
             , { id = "Sudoku #001 (Hard)"
               , puzzle =
                    {-
                       [4,3,8,7,6,0,1,0,2
                       ,2,0,0,0,9,0,5,3,0
                       ,0,0,0,0,0,2,6,0,8
                       ,0,0,4,0,2,3,0,5,0
                       ,3,0,0,0,0,0,8,0,0
                       ,6,0,0,0,0,0,0,0,0
                       ,0,0,5,0,1,0,3,0,9
                       ,0,1,0,0,0,0,0,8,0
                       ,9,0,0,6,0,0,0,7,0
                       ]
                    -}
                    [ 4, 3, 8, 7, 6, 0, 1, 0, 2, 2, 0, 0, 0, 9, 0, 5, 3, 0, 0, 0, 0, 0, 0, 2, 6, 0, 8, 0, 0, 4, 0, 2, 3, 0, 5, 0, 3, 0, 0, 0, 0, 0, 8, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 1, 0, 3, 0, 9, 0, 1, 0, 0, 0, 0, 0, 8, 0, 9, 0, 0, 6, 0, 0, 0, 7, 0 ]
               , solution =
                    {-
                       [4,3,8,7,6,5,1,9,2
                       ,2,6,1,8,9,4,5,3,7
                       ,5,7,9,1,3,2,6,4,8
                       ,1,8,4,9,2,3,7,5,6
                       ,3,9,2,5,7,6,8,1,4
                       ,6,5,7,4,8,1,9,2,3
                       ,8,4,5,2,1,7,3,6,9
                       ,7,1,6,3,4,9,2,8,5
                       ,9,2,3,6,5,8,4,7,1
                       ]
                    -}
                    [ 4, 3, 8, 7, 6, 5, 1, 9, 2, 2, 6, 1, 8, 9, 4, 5, 3, 7, 5, 7, 9, 1, 3, 2, 6, 4, 8, 1, 8, 4, 9, 2, 3, 7, 5, 6, 3, 9, 2, 5, 7, 6, 8, 1, 4, 6, 5, 7, 4, 8, 1, 9, 2, 3, 8, 4, 5, 2, 1, 7, 3, 6, 9, 7, 1, 6, 3, 4, 9, 2, 8, 5, 9, 2, 3, 6, 5, 8, 4, 7, 1 ]
               }
             ]
                |> List.map
                    (\puzzle ->
                        test ("should solve " ++ puzzle.id) <|
                            \() ->
                                Expect.equal (Ok puzzle.solution) (Puzzle.solve puzzle.puzzle)
                    )
            )
        ]

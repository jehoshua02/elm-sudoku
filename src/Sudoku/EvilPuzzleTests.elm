port module Sudoku.EvilPuzzleTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Puzzle as Puzzle exposing (Error(..))
import Sudoku.Possible as Possible
import Util exposing (set)


tests : Test
tests =
    describe "EvilPuzzle"
        [ describe "Puzzle.valid" <|
            [ test "should say this puzzle is valid" <|
                \() ->
                    Expect.equal True (Puzzle.valid evil.puzzle)
            ]
        , describe "Possible.initialize"
            [ test "should initialize correctly" <|
                \() ->
                    let
                        actual =
                            evil.puzzle
                                |> Possible.initialize

                        expected =
                            evil.puzzle
                                |> List.map
                                    (\n ->
                                        if n == 0 then
                                            [1..9]
                                        else
                                            [ n ]
                                    )
                    in
                        Expect.equal expected actual
            ]
        , describe "Possible.eliminateUsed"
            [ test "should eliminate used" <|
                \() ->
                    let
                        possible =
                            evil.puzzle
                                |> Possible.initialize

                        actual =
                            possible
                                |> Possible.eliminateUsed

                        expected =
                            [ [ 3, 4, 6, 9 ], [ 3, 4, 8, 9 ], [ 6, 8, 9 ], [ 2, 6, 8 ], [ 7 ], [ 3, 8 ], [ 2, 8 ], [ 5 ], [ 1 ], [ 1, 4, 5, 7 ], [ 4, 7, 8 ], [ 2 ], [ 9 ], [ 1, 5, 8 ], [ 1, 5, 8 ], [ 6 ], [ 3 ], [ 4, 8 ], [ 1, 3, 5, 6, 9 ], [ 3, 8, 9 ], [ 1, 6, 8, 9 ], [ 4 ], [ 1, 2, 3, 5, 6, 8 ], [ 1, 3, 5, 8 ], [ 2, 8 ], [ 2, 8, 9 ], [ 7 ], [ 3, 7 ], [ 1 ], [ 5 ], [ 2, 7, 8 ], [ 2, 4, 8 ], [ 6 ], [ 9 ], [ 2, 4, 7, 8 ], [ 2, 3, 4, 8 ], [ 3, 6, 7, 9 ], [ 2, 3, 7, 8, 9 ], [ 6, 7, 8, 9 ], [ 1, 2, 5, 7, 8 ], [ 1, 2, 4, 5, 8 ], [ 1, 4, 5, 8, 9 ], [ 2, 3, 7, 8 ], [ 2, 4, 6, 7, 8 ], [ 2, 3, 4, 6, 8 ], [ 6, 7, 9 ], [ 2, 7, 8, 9 ], [ 4 ], [ 3 ], [ 2, 8 ], [ 8, 9 ], [ 5 ], [ 1 ], [ 2, 6, 8 ], [ 2 ], [ 4, 9 ], [ 1, 9 ], [ 1, 5, 6, 8 ], [ 1, 3, 4, 5, 6, 8 ], [ 7 ], [ 1, 3, 8 ], [ 6, 8, 9 ], [ 3, 5, 6, 8, 9 ], [ 1, 7, 9 ], [ 5 ], [ 3 ], [ 1, 6, 8 ], [ 1, 6, 8 ], [ 2 ], [ 4 ], [ 6, 7, 8, 9 ], [ 6, 8, 9 ], [ 8 ], [ 6 ], [ 1, 7 ], [ 1, 5 ], [ 9 ], [ 1, 3, 4, 5 ], [ 1, 2, 3, 7 ], [ 2, 7 ], [ 2, 3, 5 ] ]
                    in
                        Expect.equal expected actual
            ]
        , describe "Possible.eliminateCrowds"
            [ test "should eliminate crowds" <|
                \() ->
                    let
                        possible =
                            evil.puzzle
                                |> Possible.initialize
                                |> Possible.eliminateUsed

                        actual =
                            possible
                                |> Possible.eliminateCrowds

                        expected =
                            [ [ 3, 4, 6, 9 ], [ 3, 4, 8, 9 ], [ 6, 8, 9 ], [ 2, 6, 8 ], [ 7 ], [ 3, 8 ], [ 2, 8 ], [ 5 ], [ 1 ], [ 1, 4, 5, 7 ], [ 4, 7, 8 ], [ 2 ], [ 9 ], [ 1, 5, 8 ], [ 1, 5, 8 ], [ 6 ], [ 3 ], [ 4 ], [ 1, 3, 5, 6, 9 ], [ 3, 8, 9 ], [ 1, 6, 8, 9 ], [ 4 ], [ 1, 2, 3, 5, 6, 8 ], [ 1, 3, 5, 8 ], [ 2, 8 ], [ 9 ], [ 7 ], [ 3, 7 ], [ 1 ], [ 5 ], [ 2, 7, 8 ], [ 2, 4, 8 ], [ 6 ], [ 9 ], [ 2, 4, 7, 8 ], [ 2, 3, 4, 8 ], [ 3, 6, 7, 9 ], [ 2, 3, 7, 8, 9 ], [ 6, 7, 8, 9 ], [ 1, 2, 5, 7, 8 ], [ 1, 2, 4, 5, 8 ], [ 1, 4, 5, 8, 9 ], [ 2, 3, 7, 8 ], [ 2, 4, 6, 7, 8 ], [ 2, 3, 4, 6, 8 ], [ 6, 7, 9 ], [ 2, 7, 8, 9 ], [ 4 ], [ 3 ], [ 2, 8 ], [ 8, 9 ], [ 5 ], [ 1 ], [ 2, 6, 8 ], [ 2 ], [ 4 ], [ 1, 9 ], [ 1, 5, 6, 8 ], [ 3 ], [ 7 ], [ 1, 3, 8 ], [ 6, 8, 9 ], [ 3, 5, 6, 8, 9 ], [ 1, 7, 9 ], [ 5 ], [ 3 ], [ 1, 6, 8 ], [ 1, 6, 8 ], [ 2 ], [ 4 ], [ 6, 7, 8, 9 ], [ 6, 8, 9 ], [ 8 ], [ 6 ], [ 1, 7 ], [ 1, 5 ], [ 9 ], [ 4 ], [ 1, 2, 3, 7 ], [ 2, 7 ], [ 2, 3, 5 ] ]
                    in
                        Expect.equal expected actual
            ]
        , describe "Possible.eliminateSame"
            [ test "should eliminate same" <|
                \() ->
                    let
                        possible =
                            evil.puzzle
                                |> Possible.initialize
                                |> Possible.eliminateUsed
                                |> Possible.eliminateCrowds

                        actual =
                            possible
                                |> Possible.eliminateSame

                        expected =
                            [ [ 3, 4, 6, 9 ], [ 3, 4, 8, 9 ], [ 6, 8, 9 ], [ 2, 6, 8 ], [ 7 ], [ 3, 8 ], [ 2, 8 ], [ 5 ], [ 1 ], [ 1, 4, 5, 7 ], [ 4, 7, 8 ], [ 2 ], [ 9 ], [ 1, 5, 8 ], [ 1, 5, 8 ], [ 6 ], [ 3 ], [ 4 ], [ 1, 3, 5, 6, 9 ], [ 3, 8, 9 ], [ 1, 6, 8, 9 ], [ 4 ], [ 1, 2, 3, 5, 6, 8 ], [ 1, 3, 5, 8 ], [ 2, 8 ], [ 9 ], [ 7 ], [ 3, 7 ], [ 1 ], [ 5 ], [ 2, 7, 8 ], [ 2, 4, 8 ], [ 6 ], [ 9 ], [ 2, 4, 7, 8 ], [ 2, 3, 4, 8 ], [ 3, 6, 7, 9 ], [ 2, 3, 7, 8, 9 ], [ 6, 7, 8, 9 ], [ 1, 2, 5, 7, 8 ], [ 1, 2, 4, 5, 8 ], [ 1, 4, 5, 8, 9 ], [ 3, 7 ], [ 2, 4, 6, 7, 8 ], [ 2, 3, 4, 6, 8 ], [ 6, 7, 9 ], [ 2, 7, 8, 9 ], [ 4 ], [ 3 ], [ 2, 8 ], [ 8, 9 ], [ 5 ], [ 1 ], [ 2, 6, 8 ], [ 2 ], [ 4 ], [ 1, 9 ], [ 1, 5, 6, 8 ], [ 3 ], [ 7 ], [ 1, 3 ], [ 6, 8, 9 ], [ 3, 5, 6, 8, 9 ], [ 1, 7, 9 ], [ 5 ], [ 3 ], [ 1, 6, 8 ], [ 1, 6, 8 ], [ 2 ], [ 4 ], [ 6, 7, 8, 9 ], [ 6, 8, 9 ], [ 8 ], [ 6 ], [ 1, 7 ], [ 1, 5 ], [ 9 ], [ 4 ], [ 1, 3, 7 ], [ 2, 7 ], [ 2, 3, 5 ] ]
                    in
                        Expect.equal expected actual
            ]
        , describe "Possible.eliminateAligned"
            [ test "should eliminate aligned" <|
                \() ->
                    let
                        possible =
                            evil.puzzle
                                |> Possible.initialize
                                |> Possible.eliminateUsed
                                |> Possible.eliminateCrowds
                                |> Possible.eliminateSame

                        actual =
                            possible
                                |> Possible.eliminateAligned

                        expected =
                            [ [ 3, 4, 6, 9 ], [ 3, 4, 8, 9 ], [ 6, 8, 9 ], [ 2, 6, 8 ], [ 7 ], [ 3, 8 ], [ 2, 8 ], [ 5 ], [ 1 ], [ 1, 4, 5, 7 ], [ 4, 7, 8 ], [ 2 ], [ 9 ], [ 1, 5, 8 ], [ 1, 5, 8 ], [ 6 ], [ 3 ], [ 4 ], [ 1, 3, 5, 6, 9 ], [ 3, 8, 9 ], [ 1, 6, 8, 9 ], [ 4 ], [ 1, 2, 3, 5, 6, 8 ], [ 1, 3, 5, 8 ], [ 2, 8 ], [ 9 ], [ 7 ], [ 3, 7 ], [ 1 ], [ 5 ], [ 2, 7, 8 ], [ 2, 4, 8 ], [ 6 ], [ 9 ], [ 2, 4, 7, 8 ], [ 2, 3, 4, 8 ], [ 3, 6, 7, 9 ], [ 2, 3, 7, 8, 9 ], [ 6, 7, 8, 9 ], [ 1, 2, 7, 8 ], [ 1, 2, 4, 5, 8 ], [ 1, 4, 5, 8, 9 ], [ 3, 7 ], [ 2, 4, 6, 7, 8 ], [ 2, 3, 4, 6, 8 ], [ 6, 7, 9 ], [ 2, 7, 8, 9 ], [ 4 ], [ 3 ], [ 2, 8 ], [ 8, 9 ], [ 5 ], [ 1 ], [ 2, 6, 8 ], [ 2 ], [ 4 ], [ 1, 9 ], [ 1, 5, 6, 8 ], [ 3 ], [ 7 ], [ 1, 3 ], [ 6, 8, 9 ], [ 3, 5, 6, 8, 9 ], [ 1, 7, 9 ], [ 5 ], [ 3 ], [ 1, 6, 8 ], [ 1, 6, 8 ], [ 2 ], [ 4 ], [ 6, 7, 8, 9 ], [ 6, 8, 9 ], [ 8 ], [ 6 ], [ 1, 7 ], [ 1, 5 ], [ 9 ], [ 4 ], [ 1, 3, 7 ], [ 2, 7 ], [ 2, 3, 5 ] ]
                    in
                        Expect.equal expected actual
            , test "should still be a valid puzzle" <|
                \() ->
                    let
                        puzzle =
                            evil.puzzle
                                |> Possible.initialize
                                |> Possible.eliminateUsed
                                |> Possible.eliminateCrowds
                                |> Possible.eliminateSame
                                |> Possible.eliminateAligned
                                |> Possible.toPuzzle
                    in
                        Expect.equal True (Puzzle.valid puzzle)
            ]
        , describe "Puzzle.solve"
            [ test "should solve evil puzzle (http://www.websudoku.com/?level=4&set_id=5147254317)" <|
                \() ->
                    Expect.equal (Ok evil.solution) (Puzzle.solve evil.puzzle)
            ]
        ]


evil : { puzzle : List Int, solution : List Int }
evil =
    { puzzle =
        {-
           [0,0,0,0,7,0,0,5,1
           ,0,0,2,9,0,0,6,3,0
           ,0,0,0,4,0,0,0,0,7
           ,0,1,5,0,0,6,9,0,0
           ,0,0,0,0,0,0,0,0,0
           ,0,0,4,3,0,0,5,1,0
           ,2,0,0,0,0,7,0,0,0
           ,0,5,3,0,0,2,4,0,0
           ,8,6,0,0,9,0,0,0,0
           ]
        -}
        [ 0, 0, 0, 0, 7, 0, 0, 5, 1, 0, 0, 2, 9, 0, 0, 6, 3, 0, 0, 0, 0, 4, 0, 0, 0, 0, 7, 0, 1, 5, 0, 0, 6, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 3, 0, 0, 5, 1, 0, 2, 0, 0, 0, 0, 7, 0, 0, 0, 0, 5, 3, 0, 0, 2, 4, 0, 0, 8, 6, 0, 0, 9, 0, 0, 0, 0 ]
    , solution =
        []
    }

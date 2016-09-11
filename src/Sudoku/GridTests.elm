port module Sudoku.GridTests exposing (tests)

import Test exposing (..)
import Expect
import Sudoku.Grid as Grid


puzzle : List Int
puzzle =
    {-
       [ 2,9,5, 7,4,3, 8,6,1
       , 4,3,1, 8,6,5, 9,2,7
       , 8,7,6, 1,9,2, 5,4,3

       , 3,8,7, 4,5,9, 2,1,6
       , 6,1,2, 3,8,7, 4,9,5
       , 5,4,9, 2,1,6, 7,3,8

       , 7,6,3, 5,3,4, 1,8,9
       , 9,2,8, 6,7,1, 3,5,4
       , 1,5,4, 9,3,8, 6,7,2
       ]
    -}
    [ 2, 9, 5, 7, 4, 3, 8, 6, 1, 4, 3, 1, 8, 6, 5, 9, 2, 7, 8, 7, 6, 1, 9, 2, 5, 4, 3, 3, 8, 7, 4, 5, 9, 2, 1, 6, 6, 1, 2, 3, 8, 7, 4, 9, 5, 5, 4, 9, 2, 1, 6, 7, 3, 8, 7, 6, 3, 5, 3, 4, 1, 8, 9, 9, 2, 8, 6, 7, 1, 3, 5, 4, 1, 5, 4, 9, 3, 8, 6, 7, 2 ]


tests : Test
tests =
    describe "Grid"
        [ describe "rows"
            [ test "should return rows" <|
                \() ->
                    let
                        actual =
                            Grid.rows puzzle

                        expected =
                            [ [ 2, 9, 5, 7, 4, 3, 8, 6, 1 ]
                            , [ 4, 3, 1, 8, 6, 5, 9, 2, 7 ]
                            , [ 8, 7, 6, 1, 9, 2, 5, 4, 3 ]
                            , [ 3, 8, 7, 4, 5, 9, 2, 1, 6 ]
                            , [ 6, 1, 2, 3, 8, 7, 4, 9, 5 ]
                            , [ 5, 4, 9, 2, 1, 6, 7, 3, 8 ]
                            , [ 7, 6, 3, 5, 3, 4, 1, 8, 9 ]
                            , [ 9, 2, 8, 6, 7, 1, 3, 5, 4 ]
                            , [ 1, 5, 4, 9, 3, 8, 6, 7, 2 ]
                            ]
                    in
                        Expect.equal expected actual
            ]
        , describe "columns"
            [ test "should return columns" <|
                \() ->
                    let
                        actual =
                            Grid.columns puzzle

                        expected =
                            [ [ 2, 4, 8, 3, 6, 5, 7, 9, 1 ]
                            , [ 9, 3, 7, 8, 1, 4, 6, 2, 5 ]
                            , [ 5, 1, 6, 7, 2, 9, 3, 8, 4 ]
                            , [ 7, 8, 1, 4, 3, 2, 5, 6, 9 ]
                            , [ 4, 6, 9, 5, 8, 1, 3, 7, 3 ]
                            , [ 3, 5, 2, 9, 7, 6, 4, 1, 8 ]
                            , [ 8, 9, 5, 2, 4, 7, 1, 3, 6 ]
                            , [ 6, 2, 4, 1, 9, 3, 8, 5, 7 ]
                            , [ 1, 7, 3, 6, 5, 8, 9, 4, 2 ]
                            ]
                    in
                        Expect.equal expected actual
            ]
        , describe "groups"
            [ test "should return groups" <|
                \() ->
                    let
                        actual =
                            Grid.groups puzzle

                        expected =
                            [ [ 2, 9, 5, 4, 3, 1, 8, 7, 6 ]
                            , [ 7, 4, 3, 8, 6, 5, 1, 9, 2 ]
                            , [ 8, 6, 1, 9, 2, 7, 5, 4, 3 ]
                            , [ 3, 8, 7, 6, 1, 2, 5, 4, 9 ]
                            , [ 4, 5, 9, 3, 8, 7, 2, 1, 6 ]
                            , [ 2, 1, 6, 4, 9, 5, 7, 3, 8 ]
                            , [ 7, 6, 3, 9, 2, 8, 1, 5, 4 ]
                            , [ 5, 3, 4, 6, 7, 1, 9, 3, 8 ]
                            , [ 1, 8, 9, 3, 5, 4, 6, 7, 2 ]
                            ]
                    in
                        Expect.equal expected actual
            ]
        ]

port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Test exposing (..)
import Expect
import Sudoku exposing (Error(..))


tests : Test
tests =
    describe "Sudoku"
        [ describe "fromList"
            [ test "should error a short list" <|
                \() ->
                    Expect.equal (Err InvalidLength) (Sudoku.fromList [ 1, 2, 3 ])
            , test "should error a long list" <|
                \() ->
                    let
                        xs =
                            [ 5
                            , 2,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Err InvalidLength) (Sudoku.fromList xs)
            , test "should error on values greater than 9" <|
                \() ->
                    let
                        xs =
                            [10,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Err OutOfRange) (Sudoku.fromList xs)
            , test "should error on values less than 0" <|
                \() ->
                    let
                        xs =
                            [-1,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Err OutOfRange) (Sudoku.fromList xs)
            , test "should pass" <|
                \() ->
                    let
                        xs =
                            [ 0,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok xs) (Sudoku.fromList xs)
            ]
        , describe "rows"
            [ test "should return rows" <|
                \() ->
                    let
                        puzzle = Sudoku.fromList
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
                        rows =
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
                        Expect.equal (Ok rows) (Result.map Sudoku.rows puzzle)
            ]
        , describe "columns"
            [ test "should return columns" <|
                \() ->
                    let
                        puzzle = Sudoku.fromList
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
                        columns =
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
                        Expect.equal (Ok columns) (Result.map Sudoku.columns puzzle)
            ]
        , describe "groups"
            [ test "should return groups" <|
                \() ->
                    let
                        puzzle = Sudoku.fromList
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
                        groups =
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
                        Expect.equal (Ok groups) (Result.map Sudoku.groups puzzle)
            ]
        , describe "solved"
            [ test "should say this puzzle is not solved" <|
                \() ->
                    let
                        puzzle = Sudoku.fromList
                            [ 0,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,3,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok False) (Result.map Sudoku.solved puzzle)
            , test "should say this puzzle is solved" <|
                \() ->
                    let
                        puzzle = Sudoku.fromList
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
                    in
                        Expect.equal (Ok True) (Result.map Sudoku.solved puzzle)
            ]
        , describe "coordToGroup"
            ([ -- top (0-2)
               ( ( 0, 0 ), 0 )
             , ( ( 2, 2 ), 0 )
             , ( ( 3, 0 ), 1 )
             , ( ( 5, 2 ), 1 )
             , ( ( 6, 0 ), 2 )
             , ( ( 8, 2 ), 2 )
               -- middle (3-5)
             , ( ( 0, 3 ), 3 )
             , ( ( 2, 5 ), 3 )
             , ( ( 3, 3 ), 4 )
             , ( ( 5, 5 ), 4 )
             , ( ( 6, 3 ), 5 )
             , ( ( 8, 5 ), 5 )
               -- bottom (6-8)
             , ( ( 0, 6 ), 6 )
             , ( ( 2, 8 ), 6 )
             , ( ( 3, 6 ), 7 )
             , ( ( 5, 8 ), 7 )
             , ( ( 6, 6 ), 8 )
             , ( ( 8, 8 ), 8 )
             ]
                |> List.map
                    (\( coord, expected ) ->
                        let
                            should =
                                ("should map " ++ (toString coord) ++ " to " ++ (toString expected))
                        in
                            test should <|
                                \() ->
                                    Expect.equal expected (Sudoku.coordToGroup coord)
                    )
            )
        , describe "possible"
            [ test "should error on invalid coord" <|
                \() ->
                    let
                        puzzle =
                            [ 0,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,2,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Err OutOfRange) ((Sudoku.fromList puzzle) `Result.andThen` (Sudoku.possible ( -1, 0 )))
            , test "should return one possible" <|
                \() ->
                    let
                        puzzle =
                            [ 0,9,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,2,4, 1,8,9
                            , 9,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok [ 2 ]) ((Sudoku.fromList puzzle) `Result.andThen` (Sudoku.possible ( 0, 0 )))
            ]
        , describe "possible"
            [ test "should return one possible even for occupied coord" <|
                \() ->
                    let
                        puzzle =
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
                    in
                        Expect.equal (Ok [ 2 ]) ((Sudoku.fromList puzzle) `Result.andThen` (Sudoku.possible ( 0, 0 )))
            , test "should return more than one" <|
                \() ->
                    let
                        puzzle =
                            [ 0,0,5, 7,4,3, 8,6,1
                            , 4,3,1, 8,6,5, 9,2,7
                            , 8,7,6, 1,9,2, 5,4,3

                            , 3,8,7, 4,5,9, 2,1,6
                            , 6,1,2, 3,8,7, 4,9,5
                            , 5,4,9, 2,1,6, 7,3,8

                            , 7,6,3, 5,2,4, 1,8,9
                            , 0,2,8, 6,7,1, 3,5,4
                            , 1,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok [ 2, 9 ]) ((Sudoku.fromList puzzle) `Result.andThen` (Sudoku.possible ( 0, 0 )))
            , test "should return more all nine" <|
                \() ->
                    let
                        puzzle =
                            [ 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 8,6,5, 9,2,7
                            , 0,0,0, 1,9,2, 5,4,3

                            , 0,8,7, 4,5,9, 2,1,6
                            , 0,1,2, 3,8,7, 4,9,5
                            , 0,4,9, 2,1,6, 7,3,8

                            , 0,6,3, 5,2,4, 1,8,9
                            , 0,2,8, 6,7,1, 3,5,4
                            , 0,5,4, 9,3,8, 6,7,2
                            ]
                    in
                        Expect.equal (Ok [ 1..9 ]) ((Sudoku.fromList puzzle) `Result.andThen` (Sudoku.possible ( 0, 0 )))
            ]
        ]


main : Program Value
main =
    run emit tests


port emit : ( String, Value ) -> Cmd msg

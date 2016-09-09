port module Sudoku.PossibleTests exposing (..)

import Test exposing (..)
import Expect
import Sudoku.Possible as Possible
import Sudoku.Puzzle as Puzzle
import List.Extra exposing (setAt, removeAt)


tests : Test
tests =
    describe "Possible"
        [ describe "initialize"
            [ test "should return a new list of possibilities" <|
                \() ->
                    let
                        actual =
                            Possible.initialize

                        expected =
                            List.repeat (9 * 9) [1..9]
                    in
                        Expect.equal expected actual
            ]
        , describe "eliminateUsed"
            [ test "should eliminate nothing" <|
                \() ->
                    let
                        expected =
                            Possible.initialize

                        actual =
                            Possible.eliminateUsed Puzzle.empty expected
                    in
                        Expect.equal expected actual
            , test "should eliminate used numbers" <|
                \() ->
                    let
                        puzzle =
                            [ 2,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0

                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0

                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            ]

                        actual =
                            Just (Possible.eliminateUsed puzzle Possible.initialize)

                        allBut2 =
                            removeAt 1 [1..9]

                        expected =
                            Possible.initialize
                                |> set 0 [2]

                                -- remove 2 from row
                                |> setAll [1..8] allBut2

                                -- remove 2 from column
                                |> setAll ([1..8] |> List.map ((*) 9)) allBut2

                                -- remove 2 from rest of group
                                |> setAll [10,11,19,20] allBut2

                                |> Just
                    in
                        Expect.equal expected actual
            , test "should preserve existing eliminations" <|
                \() ->
                    let
                        puzzle =
                            [ 2,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0

                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0

                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            , 0,0,0, 0,0,0, 0,0,0
                            ]

                        possible =
                            Possible.initialize
                                -- let's set the last one to something arbitrary
                                |> set (9 * 9 - 1) [5,6]

                        actual =
                            Possible.eliminateUsed puzzle possible

                        allBut2 =
                            removeAt 1 [1..9]

                        expected =
                            possible
                                |> set 0 [2]

                                -- remove 2 from row
                                |> setAll [1..8] allBut2

                                -- remove 2 from column
                                |> setAll ([1..8] |> List.map ((*) 9)) allBut2

                                -- remove 2 from rest of group
                                |> setAll [10,11,19,20] allBut2
                    in
                        Expect.equal expected actual
            ]
        ]

setAll : List Int -> a -> List a -> List a
setAll is x xs =
    is |> List.foldl (\i b -> set i x b) xs

set : Int -> a -> List a -> List a
set i x xs =
    setAt i x xs |> Maybe.withDefault xs

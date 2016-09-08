module Sudoku.Possible
    exposing
        ( Possible
        , initialize
        )


type alias Possible =
    List (List Int)


initialize : Possible
initialize =
    List.repeat (9 * 9) [1..9]


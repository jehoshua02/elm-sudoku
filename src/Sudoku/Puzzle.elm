module Sudoku.Puzzle
    exposing
        ( Puzzle
        , empty
        , fromList
        , make
        , Error(..)
        , solved
        , valid
        , complete
        , solve
        )

import Set
import List.Extra exposing (findIndex, updateAt)
import Sudoku.Grid exposing (rows, columns, groups)
import Sudoku.Possible as Possible
import Util exposing (get, set, diff, shuffle)
import Native.Random
import Trampoline


type alias Puzzle =
    List Int


type Error
    = InvalidLength
    | OutOfRange
    | Unsolvable


empty : Puzzle
empty =
    List.repeat (9 * 9) 0


fromList : List Int -> Result Error Puzzle
fromList xs =
    if List.length xs /= 9 * 9 then
        Err InvalidLength
    else if List.any (\n -> n < 0 || n > 9) xs then
        Err OutOfRange
    else
        Ok xs


make : Float -> { puzzle : Puzzle, solution : Puzzle }
make percent =
    -- second, remove numbers from solution up to percent
    -- this is the puzzle
    let
        -- first, fill in puzzle one cell at a time
        -- this is the solution
        solution =
            makeSolution
                |> Debug.log "solution"

        puzzle =
            empty
    in
        { puzzle = puzzle, solution = solution }


makeSolution : Puzzle
makeSolution =
    List.repeat 9 [1..9]
        |> List.concat
        |> makeSolution'
        |> Trampoline.evaluate


makeSolution' : Puzzle -> Trampoline.Trampoline Puzzle
makeSolution' puzzle =
    Trampoline.jump
        (\() ->
            if valid puzzle then
                Trampoline.done puzzle
            else
                let
                    rows' =
                        rows puzzle

                    i =
                        Native.Random.int 0 ((List.length rows') - 1)

                    newRow =
                        get i [] rows'
                            |> shuffle
                            |> Debug.log "after"

                    newPuzzle =
                        rows'
                            |> updateAt i shuffle
                            |> Maybe.withDefault rows'
                            |> List.concat
                in
                    makeSolution' newPuzzle
                        |> Debug.log "newPuzzle"
        )


solved : Puzzle -> Bool
solved puzzle =
    valid puzzle && complete puzzle


valid : Puzzle -> Bool
valid xs =
    rows xs
        ++ columns xs
        ++ groups xs
        |> List.all
            (\chunk ->
                let
                    filled =
                        chunk
                            |> List.filter ((/=) 0)
                            |> List.sort

                    unique =
                        filled
                            |> Set.fromList
                            |> Set.toList
                in
                    filled == unique
            )


complete : Puzzle -> Bool
complete xs =
    List.length xs == 9 * 9 && List.all ((/=) 0) xs


valid : Puzzle -> Bool
valid xs =
    rows xs ++ columns xs ++ groups xs
        |> List.all
            (\chunk ->
                let
                    filled =
                        chunk
                            |> List.filter ((/=) 0)
                            |> List.sort

                    unique =
                        filled
                            |> Set.fromList
                            |> Set.toList
                in
                    filled == unique
            )


complete : Puzzle -> Bool
complete xs =
    xs |> List.all ((/=) 0)


solve : Puzzle -> Result Error Puzzle
solve puzzle =
    if solved puzzle then
        Ok puzzle
    else if puzzle |> List.all ((/=) 0) then
        Err Unsolvable
    else
        let
            before =
                puzzle

            possible =
                before
                    |> Possible.initialize
                    |> Possible.eliminateUsed
                    |> Possible.eliminateCrowds
                    |> Possible.eliminateSame
                    |> Possible.eliminateAligned

            after =
                Possible.toPuzzle possible
        in
            if before == after then
                guess possible
            else
                solve after


guess : Possible.Possible -> Result Error Puzzle
guess possible =
    let
        m =
            possible
                |> findIndex (List.length >> (==) 2)
    in
        case m of
            Nothing ->
                Err Unsolvable

            Just i ->
                let
                    options =
                        possible
                            |> get i []
                            |> List.map
                                (\n ->
                                    possible
                                        |> set i [ n ]
                                        |> Possible.toPuzzle
                                )
                in
                    case options of
                        [ a, b ] ->
                            let
                                solveA =
                                    solve a
                            in
                                if solveA == Err Unsolvable then
                                    solve b
                                else
                                    solveA

                        _ ->
                            Err Unsolvable

module Util exposing (..)

import Set
import List.Extra exposing (getAt, setAt, removeAt)
import Random


get : Int -> a -> List a -> a
get i d xs =
    getAt i xs |> Maybe.withDefault d


set : Int -> a -> List a -> List a
set i x xs =
    setAt i x xs |> Maybe.withDefault xs


diff : List comparable -> List comparable -> List comparable
diff a b =
    Set.diff (Set.fromList a) (Set.fromList b) |> Set.toList


randomIndex : Random.Seed -> List a -> (Int, Random.Seed)
randomIndex seed xs =
    let
        seed =
            Random.initialSeed 0

        generator =
            Random.int 0 ((List.length xs) - 1)
    in
        Random.step generator seed


shuffle : Random.Seed -> List a -> (List a, Random.Seed)
shuffle seed xs =
    case xs of
        [] -> ([], seed)
        [_] -> (xs, seed)
        head :: tail ->
            let
                generator =
                    Random.int 0 ((List.length xs) - 1)

                (i, newSeed) =
                    Random.step generator seed

                x =
                    get i head xs

                (tail, newNewSeed) =
                    shuffle newSeed (removeAt i xs)
            in
                (x :: tail, newNewSeed)

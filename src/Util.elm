module Util exposing (..)

import Set
import List.Extra exposing (getAt, setAt)
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


randomItem : List a -> Maybe a
randomItem xs =
    case xs of
        [] ->
            Nothing
        _ ->
            let
                seed =
                    Random.initialSeed 0

                generator =
                    Random.int 0 ((List.length xs) - 1)

                (i, _) =
                    Random.step generator seed
            in
                getAt i xs

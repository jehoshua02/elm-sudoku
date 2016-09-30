module Util exposing (..)

import Set
import List.Extra exposing (getAt, setAt)
import Random
import Platform.Cmd


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
                randomInt =
                    Random.int 0 ((List.length xs) - 1)

                i =
                    Random.generate identity randomInt
            in
                getAt i xs

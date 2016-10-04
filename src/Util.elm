module Util exposing (..)

import Set
import List.Extra exposing (getAt, setAt, removeAt)
import Native.Random


get : Int -> a -> List a -> a
get i d xs =
    getAt i xs |> Maybe.withDefault d


set : Int -> a -> List a -> List a
set i x xs =
    setAt i x xs |> Maybe.withDefault xs


diff : List comparable -> List comparable -> List comparable
diff a b =
    Set.diff (Set.fromList a) (Set.fromList b) |> Set.toList


shuffle : List a -> List a
shuffle xs =
    case xs of
        [] -> []
        [_] -> xs
        head :: tail ->
            let
                i =
                    Native.Random.int 0 ((List.length xs) - 1)

                x =
                    get i head xs

                tail =
                    shuffle (removeAt i xs)
            in
                x :: tail

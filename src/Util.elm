module Util exposing (..)

import Set
import List.Extra exposing (getAt, setAt)


get : Int -> a -> List a -> a
get i d xs =
    getAt i xs |> Maybe.withDefault d


set : Int -> a -> List a -> List a
set i x xs =
    setAt i x xs |> Maybe.withDefault xs


diff : List comparable -> List comparable -> List comparable
diff a b =
    Set.diff (Set.fromList a) (Set.fromList b) |> Set.toList

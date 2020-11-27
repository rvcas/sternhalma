module Utils exposing (cx, indexToColor, mapToList)

import Array exposing (Array)


indexToColor : Int -> String
indexToColor idx =
    case idx of
        0 ->
            "bg-red-400"

        1 ->
            "bg-purple-500"

        2 ->
            -- Orange
            "bg-yellow-600"

        3 ->
            "bg-blue-300"

        4 ->
            "bg-green-500"

        _ ->
            "bg-yellow-300"


mapToList : (a -> b) -> Array a -> List b
mapToList func arr =
    arr
        |> Array.map func
        |> Array.toList


cx : List String -> String
cx classes =
    String.join " " classes

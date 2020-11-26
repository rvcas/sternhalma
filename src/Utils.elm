module Utils exposing (indexToColor)


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
            "bg-yellow-200"

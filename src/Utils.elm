module Utils exposing
    ( assignRow
    , between
    , bottomIncludes
    , bottomLeftIncludes
    , bottomRightIncludes
    , cx
    , indexToColor
    , mapToList
    , topIncludes
    , topLeftIncludes
    , topRightIncludes
    )

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


topIncludes : Int -> Bool
topIncludes i =
    List.any ((==) i) [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


topRightIncludes : Int -> Bool
topRightIncludes i =
    List.any ((==) i) [ 19, 20, 21, 22, 32, 33, 34, 44, 45, 55 ]


bottomRightIncludes : Int -> Bool
bottomRightIncludes i =
    List.any ((==) i) [ 74, 84, 85, 95, 96, 97, 107, 108, 109, 110 ]


bottomIncludes : Int -> Bool
bottomIncludes i =
    List.any ((==) i) [ 111, 112, 113, 114, 115, 116, 117, 118, 119, 120 ]


bottomLeftIncludes : Int -> Bool
bottomLeftIncludes i =
    List.any ((==) i) [ 65, 75, 76, 86, 87, 88, 98, 99, 100, 101 ]


topLeftIncludes : Int -> Bool
topLeftIncludes i =
    List.any ((==) i) [ 10, 11, 12, 13, 23, 24, 25, 35, 36, 46 ]


between : comparable -> comparable -> comparable -> Bool
between x min max =
    x >= min && x <= max


assignRow : number -> number
assignRow i =
    let
        bw =
            between i
    in
    if i == 0 then
        0

    else if bw 1 2 then
        1

    else if bw 3 5 then
        2

    else if bw 6 9 then
        3

    else if bw 10 22 then
        4

    else if bw 23 34 then
        5

    else if bw 35 45 then
        6

    else if bw 46 55 then
        7

    else if bw 56 64 then
        8

    else if bw 65 74 then
        9

    else if bw 75 85 then
        10

    else if bw 86 97 then
        11

    else if bw 98 110 then
        12

    else if bw 111 114 then
        13

    else if bw 115 117 then
        14

    else if bw 118 119 then
        15

    else
        16


mapToList : (a -> b) -> Array a -> List b
mapToList func arr =
    arr
        |> Array.map func
        |> Array.toList


cx : List String -> String
cx classes =
    String.join " " classes

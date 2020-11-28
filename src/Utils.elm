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
    List.any ((==) i) [ 2, 4, 5, 7, 8, 9, 11, 12, 13, 14 ]


topRightIncludes : Int -> Bool
topRightIncludes i =
    List.any ((==) i) [ 25, 26, 27, 28, 39, 40, 41, 52, 53, 64 ]


bottomRightIncludes : Int -> Bool
bottomRightIncludes i =
    List.any ((==) i) [ 85, 96, 97, 108, 109, 110, 121, 122, 123, 124 ]


bottomIncludes : Int -> Bool
bottomIncludes i =
    List.any ((==) i) [ 126, 127, 128, 129, 131, 132, 133, 135, 136, 138 ]


bottomLeftIncludes : Int -> Bool
bottomLeftIncludes i =
    List.any ((==) i) [ 76, 87, 88, 99, 100, 101, 112, 113, 114, 115 ]


topLeftIncludes : Int -> Bool
topLeftIncludes i =
    List.any ((==) i) [ 16, 17, 18, 19, 30, 31, 32, 43, 44, 55 ]


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

    else if bw 10 14 then
        4

    else if bw 15 28 then
        5

    else if bw 29 41 then
        6

    else if bw 42 53 then
        7

    else if bw 54 64 then
        8

    else if bw 65 74 then
        9

    else if bw 75 85 then
        10

    else if bw 86 97 then
        11

    else if bw 98 110 then
        12

    else if bw 111 124 then
        13

    else if bw 125 129 then
        14

    else if bw 130 133 then
        15

    else if bw 134 136 then
        16

    else
        17


mapToList : (a -> b) -> Array a -> List b
mapToList func arr =
    arr
        |> Array.map func
        |> Array.toList


cx : List String -> String
cx classes =
    String.join " " classes

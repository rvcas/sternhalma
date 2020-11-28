module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array, empty)
import Browser exposing (Document)
import Html exposing (Html, button, div, h1, main_, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Utils
    exposing
        ( assignRow
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



-- Main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- Model


type alias PlayerId =
    Int


type alias Player =
    { id : PlayerId
    , score : Int
    , color : String
    }


type alias Position =
    { occupyingPlayer : Maybe PlayerId
    , scoringPlayer : Maybe PlayerId
    , row : Int
    , index : Int
    }


type alias GameState =
    { totalPlayers : Int
    , players : Array Player
    , board : Array Position
    , currentPlayer : Int
    }


type Model
    = SelectPlayers
    | Playing GameState


init : () -> ( Model, Cmd Msg )
init _ =
    ( SelectPlayers, Cmd.none )


generatePlayers : Int -> Array Player
generatePlayers count =
    Array.repeat count 0
        |> Array.indexedMap (\idx -> \_ -> Player idx 0 (indexToColor idx))


emptyBoard : Array Int
emptyBoard =
    Array.repeat 139 0
        |> Array.indexedMap (\idx -> \_ -> idx)


twoPlayers : Int -> Position
twoPlayers idx =
    { occupyingPlayer =
        if topIncludes idx then
            Just 0

        else if bottomIncludes idx then
            Just 1

        else
            Nothing
    , scoringPlayer =
        if bottomIncludes idx then
            Just 0

        else if topIncludes idx then
            Just 1

        else
            Nothing
    , row = assignRow idx
    , index = idx
    }


threePlayers : Int -> Position
threePlayers idx =
    { occupyingPlayer =
        if topLeftIncludes idx then
            Just 0

        else if topRightIncludes idx then
            Just 1

        else if bottomIncludes idx then
            Just 2

        else
            Nothing
    , scoringPlayer =
        if bottomRightIncludes idx then
            Just 0

        else if bottomLeftIncludes idx then
            Just 1

        else if topIncludes idx then
            Just 2

        else
            Nothing
    , row = assignRow idx
    , index = idx
    }


fourPlayers : Int -> Position
fourPlayers idx =
    { occupyingPlayer =
        if topLeftIncludes idx then
            Just 0

        else if topRightIncludes idx then
            Just 1

        else if bottomRightIncludes idx then
            Just 2

        else if bottomLeftIncludes idx then
            Just 3

        else
            Nothing
    , scoringPlayer =
        if bottomRightIncludes idx then
            Just 0

        else if bottomLeftIncludes idx then
            Just 1

        else if topLeftIncludes idx then
            Just 2

        else if topRightIncludes idx then
            Just 3

        else
            Nothing
    , row = assignRow idx
    , index = idx
    }


fivePlayers : Int -> Position
fivePlayers idx =
    { occupyingPlayer =
        if topLeftIncludes idx then
            Just 0

        else if topIncludes idx then
            Just 1

        else if topRightIncludes idx then
            Just 2

        else if bottomRightIncludes idx then
            Just 3

        else if bottomLeftIncludes idx then
            Just 4

        else
            Nothing
    , scoringPlayer =
        if bottomRightIncludes idx then
            Just 0

        else if bottomIncludes idx then
            Just 1

        else if bottomLeftIncludes idx then
            Just 2

        else if topLeftIncludes idx then
            Just 3

        else if topRightIncludes idx then
            Just 4

        else
            Nothing
    , row = assignRow idx
    , index = idx
    }


sixPlayers : Int -> Position
sixPlayers idx =
    { occupyingPlayer =
        if topLeftIncludes idx then
            Just 0

        else if topIncludes idx then
            Just 1

        else if topRightIncludes idx then
            Just 2

        else if bottomRightIncludes idx then
            Just 3

        else if bottomIncludes idx then
            Just 4

        else if bottomLeftIncludes idx then
            Just 5

        else
            Nothing
    , scoringPlayer =
        if bottomRightIncludes idx then
            Just 0

        else if bottomIncludes idx then
            Just 1

        else if bottomLeftIncludes idx then
            Just 2

        else if topLeftIncludes idx then
            Just 3

        else if topIncludes idx then
            Just 4

        else if topRightIncludes idx then
            Just 5

        else
            Nothing
    , row = assignRow idx
    , index = idx
    }


generateBoard : Int -> Array Position
generateBoard count =
    case count of
        3 ->
            Array.map threePlayers emptyBoard

        4 ->
            Array.map fourPlayers emptyBoard

        5 ->
            Array.map fivePlayers emptyBoard

        6 ->
            Array.map sixPlayers emptyBoard

        _ ->
            Array.map twoPlayers emptyBoard


group : Array Position -> List (List Position)
group items =
    items
        |> Array.toList
        |> List.foldr
            (\x acc ->
                case acc of
                    [] ->
                        [ ( x, [] ) ]

                    ( y, rest ) :: groups ->
                        if x.row == y.row then
                            ( x, y :: rest ) :: groups

                        else
                            ( x, [] ) :: acc
            )
            []
        |> List.map (\( _, row ) -> row)



-- Update


type Msg
    = Reset
    | SetPlayers Int
    | SetScore PlayerId Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayers count ->
            ( Playing
                { totalPlayers = count
                , players = generatePlayers count
                , board = generateBoard count
                , currentPlayer = 0
                }
            , Cmd.none
            )

        SetScore playerId score ->
            updateCurrentGame model Cmd.none <|
                \state ->
                    let
                        players =
                            updatePlayer playerId state.players <|
                                \player -> { player | score = score }
                    in
                    ( Playing { state | players = players }
                    , Cmd.none
                    )

        Reset ->
            ( SelectPlayers, Cmd.none )


updatePlayer : PlayerId -> Array Player -> (Player -> Player) -> Array Player
updatePlayer playerId players func =
    case Array.get playerId players of
        Just player ->
            Array.set playerId (func player) players

        Nothing ->
            players


updateCurrentGame : Model -> Cmd Msg -> (GameState -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
updateCurrentGame model cmd func =
    case model of
        Playing state ->
            func state

        _ ->
            ( model, cmd )



-- View


view : Model -> Document Msg
view model =
    { title = "Sternhalma"
    , body =
        [ case model of
            SelectPlayers ->
                viewSelectPlayer ()

            Playing state ->
                viewBoard state
        ]
    }


viewLayout : List (Html Msg) -> Html Msg
viewLayout =
    main_ [ class "container mx-auto flex flex-col justify-center items-center h-screen" ]


viewSelectPlayer : () -> Html Msg
viewSelectPlayer _ =
    viewLayout
        [ h1 [ class "font-mono text-2xl mb-12" ] [ text "Select the Amount of Players" ]
        , div [] viewButtons
        ]


viewButtons : List (Html Msg)
viewButtons =
    Array.repeat 5 0
        |> Array.indexedMap (\idx -> \_ -> idx)
        |> Array.map
            (\idx ->
                button
                    [ class
                        (cx
                            [ indexToColor idx
                            , "rounded-full h-20 w-20 text-white text-lg mx-4 shadow-lg font-mono"
                            ]
                        )
                    , onClick (SetPlayers (idx + 2))
                    ]
                    [ text (String.fromInt (idx + 2)) ]
            )
        |> Array.toList


viewBoard : GameState -> Html Msg
viewBoard state =
    let
        current =
            case Array.get state.currentPlayer state.players of
                Just c ->
                    c

                Nothing ->
                    Player 0 0 ""
    in
    viewLayout
        [ div [ class "font-mono text-lg mb-8" ]
            [ text
                (String.concat
                    [ "Total Players: "
                    , String.fromInt state.totalPlayers
                    , ", Current Player: "
                    , current.color
                    ]
                )
            ]
        , div [ class "w-full" ]
            (state.board
                |> group
                |> List.map (viewRow state current)
            )
        ]


viewRow : GameState -> Player -> List Position -> Html Msg
viewRow state current row =
    div [ class "flex flex-row justify-center" ]
        (row
            |> List.map (viewCol state current)
        )


viewCol : GameState -> Player -> Position -> Html Msg
viewCol state current position =
    case position.occupyingPlayer of
        Just playerId ->
            case Array.get playerId state.players of
                Just player ->
                    div
                        [ class
                            (cx
                                [ player.color
                                , "border-black rounded-full h-12 w-12 border mx-0.5"
                                ]
                            )
                        ]
                        []

                -- I don't think we'll get here
                Nothing ->
                    div [] [ text "probably shouldn't render" ]

        Nothing ->
            div
                [ class
                    (cx
                        [ "border-black rounded-full h-12 w-12 border transparent mx-0.5"
                        ]
                    )
                ]
                []

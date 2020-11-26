module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Utils exposing (indexToColor)



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
    , index : Int
    , row : Int
    , col : Int
    }


type alias GameState =
    { totalPlayers : Int
    , players : Array Player
    , board : Array (Array (Maybe Position))
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
                , board = Array.empty
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


viewSelectPlayer : () -> Html Msg
viewSelectPlayer _ =
    div []
        (Array.repeat 6 0
            |> Array.indexedMap
                (\idx ->
                    \_ ->
                        button [ class (indexToColor idx), onClick (SetPlayers (idx + 1)) ]
                            [ text (String.fromInt (idx + 1)) ]
                )
            |> Array.toList
        )


viewBoard : GameState -> Html Msg
viewBoard state =
    div []
        [ div [] [ text (String.fromInt state.totalPlayers) ]
        , div []
            (state.board
                |> Array.map (viewRow state)
                |> Array.toList
            )
        ]


viewRow : GameState -> Array (Maybe Position) -> Html Msg
viewRow state row =
    div []
        (row
            |> Array.map (viewCol state)
            |> Array.toList
        )


viewCol : GameState -> Maybe Position -> Html Msg
viewCol _ col =
    case col of
        Just position ->
            div []
                [ text (String.fromInt position.index) ]

        Nothing ->
            div [] []

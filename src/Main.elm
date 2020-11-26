module Main exposing (Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Browser exposing (Document)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- Main


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- Model


type alias Player =
    { id : PlayerId
    , score : Int
    , color : String
    }


type alias Position =
    { occupyingPlayer : Maybe Player
    , scoringPlayer : Maybe Player
    , index : Int
    , row : Int
    , col : Int
    }


type alias GameState =
    { totalPlayers : Int
    , players : Array Player
    , board : Array (Maybe Position)
    }


type Model
    = SelectPlayers
    | Playing GameState


init : Model
init =
    SelectPlayers


generatePlayers : Int -> Array Player
generatePlayers count =
    Array.repeat count 0
        |> Array.indexedMap (\idx -> \_ -> Player idx 0 "red")



-- Update


type alias PlayerId =
    Int


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
            ( init, Cmd.none )


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
                        button [ onClick (SetPlayers (idx + 1)) ]
                            [ text (String.fromInt (idx + 1)) ]
                )
            |> Array.toList
        )


viewBoard : GameState -> Html Msg
viewBoard state =
    div []
        [ div [] [ text (String.fromInt state.totalPlayers) ]
        ]

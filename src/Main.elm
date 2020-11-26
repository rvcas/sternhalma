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
    { id : Int
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
    }


type Model
    = SelectPlayers
    | Playing GameState


init : Model
init =
    SelectPlayers



-- Update


type alias PlayerId =
    Int


type Msg
    = SetPlayers Int
    | SetScore PlayerId Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayers count ->
            ( Playing { totalPlayers = count, players = Array.empty }, Cmd.none )

        SetScore playerId score ->
            case model of
                Playing state ->
                    let
                        players =
                            case Array.get playerId state.players of
                                Just player ->
                                    Array.set playerId { player | score = score } state.players

                                Nothing ->
                                    state.players
                    in
                    ( Playing { state | players = players }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- View


view : Model -> Document Msg
view model =
    { title = "Sternhalma"
    , body =
        [ case model of
            SelectPlayers ->
                div []
                    [ button [ onClick (SetPlayers 1) ] [ text "1" ]
                    , button [ onClick (SetPlayers 4) ] [ text "4" ]
                    ]

            Playing state ->
                div []
                    [ div [] [ text (String.fromInt state.totalPlayers) ]
                    ]
        ]
    }

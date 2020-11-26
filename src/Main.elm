module Main exposing (Model, Msg(..), init, main, update, view)

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


type alias GameState =
    { totalPlayers : Int
    }


type Model
    = SelectPlayers
    | Playing GameState


init : Model
init =
    SelectPlayers



-- Update


type Msg
    = SetPlayers Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayers count ->
            ( Playing { totalPlayers = count }, Cmd.none )



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

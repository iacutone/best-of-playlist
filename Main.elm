module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
-- import Native.Audio
import Time
import Debug

-- MSG

type Msg
    = NoOp

-- MODEL

type alias Model =
    { mediaUrl : String
    , playing: Bool
    , duration: Float
    }

-- INIT

initialModel : Model
initialModel =
    { mediaUrl = "songs/The Julie Ruin - I'm Done.webm"
    , playing = False
    , duration = 0.0
    }

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "elm-audio-player" ]
        [ audio
            [ src model.mediaUrl
            , id "audio-player"
            ]
            []
        ]

main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none )
        }

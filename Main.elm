module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
-- import Native.Audio
import Time
import Debug
import Port

-- MSG

type Msg
    = NoOp
    | Play
    | Pause

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
        Play ->
            ( { model | playing = True }, Port.play () )
        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "audio-player" ]
        [ audio
            [ id "audiofile"
            , src model.mediaUrl
            , controls True
            ]
            []
        , viewPlayButton model.playing
        ]

viewPlayButton : Bool -> Html Msg
viewPlayButton playing =
    if playing then
        button
            [ class "pause"
            , name "pause"
            , onClick Pause
            ]
            [ text "Pause" ]
    else
        button
            [ class "fa fa-pause play"
            , name "play"
            , onClick Play
            ]
            [ text "Play" ]

main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none )
        }

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Time
import Debug
import Port
import Json.Decode as Decode exposing (..)
-- import Json.Encode as Encode

-- MSG

type Msg
    = NoOp
    | Play
    | Pause
    | UpdateTime Float

-- MODEL

type alias Model =
    { songs: List Flags
    , playing: Bool
    , currentTime: Float
    , duration: Float
    }

type alias Flags =
    { songSource: String
    , songName: String
    }

initialModel : Flags -> Model
initialModel flags =
    { songs = [flags]
    , playing = False
    , currentTime = 0.0
    , duration = 0.0
    }

-- ENCODERS/DECODERS

onTimeUpdate : (Float -> msg) -> Attribute msg
onTimeUpdate msg =
    on "timeupdate" (Decode.map msg targetCurrentTime)

-- A `Json.Decoder` for grabbing `event.target.currentTime`.
-- http://vincent.jousse.org/en/tech/interacting-with-dom-element-using-elm-audio-video/

targetCurrentTime : Decoder Float
targetCurrentTime =
    Decode.at [ "target", "currentTime" ] Decode.float

targetDuration : Decoder Float
targetDuration =
    Decode.at [ "target", "duration" ] Decode.float

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( { model | playing = True }, Port.play () )
        Pause ->
            ( { model | playing = False }, Port.pause() )
        UpdateTime time ->
            ( { model | currentTime = time }, Cmd.none )
        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewPlayButton model.playing
        , div [ class "flags" ]
        [
            text (toString model.songs)
        ]
        ]

-- viewSong : Model -> Html Msg
-- viewSong model =
--     div [ class "viewer" ]
--         [
--             text model.songName
--         ]

-- viewTime : Model -> Html Msg
-- viewTime model =
--     div [ class "time" ]
--         [
--             text (toString model.currentTime)
--         ]

viewPlayButton : Bool -> Html Msg
viewPlayButton playing =
    if playing then
        button
            [ class "fa fa-pause pause"
            , name "pause"
            , onClick Pause
            ]
            [ text "Pause" ]
    else
        button
            [ class "fa fa-play play"
            , name "play"
            , onClick Play
            ]
            [ text "Play" ]

init : Flags -> (Model, Cmd Msg)
init flags =
    ( initialModel flags, Cmd.none )

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = (\model -> Sub.none)
        }

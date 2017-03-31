module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Time
import Debug
import Port
import Json.Decode as Decode exposing (..)

-- MSG

type Msg
    = NoOp
    | Play
    | Pause
    | UpdateTime Float
    | SetDuration Float

-- MODEL

type alias Model =
    { songs: List Song
    , currentSong: String
    , currentTime: Float
    , duration: Float
    , playing: Bool
    }

type alias Flags =
    { songs: List Song }

type alias Song =
    { songSource: String
    , songName: String
    , id: String
    }

initialModel : Flags -> Model
initialModel flags =
    { songs = flags.songs
    , currentSong = "song1"
    , currentTime = 0.0
    , duration = 0.0
    , playing = False
    }

onTimeUpdate : (Float -> msg) -> Attribute msg
onTimeUpdate msg =
    on "timeupdate" (Decode.map msg targetCurrentTime)

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
            ( { model | playing = True }, Port.play (model.currentSong) )
        Pause ->
            ( { model | playing = False }, Port.pause (model.currentSong) )
        UpdateTime time ->
            ( { model | currentTime = time }, Cmd.none )
        SetDuration time ->
            ( { model | duration = time }, Cmd.none )
        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewSongs model.songs
        , footer model
        ]

viewSongs : List Song -> Html Msg
viewSongs songs =
    ul [] (List.map audioSong songs)

audioSong : Song -> Html Msg
audioSong song =
    li [ class "songs" ]
        [ text song.songName
        , audio
            [ onTimeUpdate UpdateTime
            , src song.songSource
            , id song.id
            ]
            []
       ]

viewPlayButton : Bool -> Html Msg
viewPlayButton playing =
    if playing then
        button
            [ class "fa fa-pause pause"
            , name "pause"
            , onClick Pause
            ]
            []
    else
        button
            [ class "fa fa-play play"
            , name "play"
            , onClick Play
            ]
            []

viewSongName : String -> Html Msg
viewSongName name =
    div [] [ text name ]

viewSongCurrentTime : Float -> Html Msg
viewSongCurrentTime time =
    div [] [ text (toString time) ]

viewSongDuration : Float -> Html Msg
viewSongDuration duration =
    div [] [ text (toString duration) ]

footer : Model -> Html Msg
footer model =
    div []
        [ viewPlayButton model.playing
        , viewSongName model.currentSong
        , viewSongDuration model.duration
        , viewSongCurrentTime model.currentTime
        ]

init : Flags -> (Model, Cmd Msg)
init flags =
    ( initialModel flags, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Port.setDuration SetDuration

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

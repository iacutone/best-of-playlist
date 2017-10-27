module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Port
import Json.Decode as Decode exposing (..)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (cx, cy, r, stroke, fill, strokeWidth, strokeDasharray, strokeDashoffset)

-- MSG

type Msg
    = Play
    | Pause
    | UpdateTime Float
    | SetDuration Float
    | SetSongId String
    | SetSongName String
    | NextSong String
    | PlayNextSong
    | PlayPreviousSong

-- MODEL

type alias Model =
    { songs: List Song
    , currentSongId: String
    , currentSongName: String
    , currentTime: Float
    , duration: Float
    , playing: Bool
    , position: Float
    , title: String
    }

type alias Flags =
    { songs: List Song
    , title: String
    }

type alias Song =
    { songSource: String
    , songName: String
    , id: String
    }

initialModel : Flags -> Model
initialModel flags =
    { songs = flags.songs
    , currentSongId = "song1"
    , currentSongName = ""
    , currentTime = 0.0
    , duration = 0.0
    , playing = False
    , position = 1
    , title = flags.title
    }

onTimeUpdate : (Float -> msg) -> Attribute msg
onTimeUpdate msg =
    on "timeupdate" (Decode.map msg targetCurrentTime)

onEnd : (String -> msg) -> Attribute msg
onEnd msg =
    on "ended" (Decode.map msg targetId)

targetCurrentTime : Decoder Float
targetCurrentTime =
    Decode.at [ "target", "currentTime" ] Decode.float

targetId : Decoder String
targetId =
    Decode.at [ "target", "id" ] Decode.string

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play ->
            ( { model | playing = True }, Port.play (model.currentSongId) )
        Pause ->
            ( { model | playing = False }, Port.pause (model.currentSongId) )
        UpdateTime time ->
            ( { model | currentTime = time }, Cmd.none )
        SetDuration time ->
            ( { model | duration = time }, Cmd.none )
        SetSongId msg ->
            ( { model | currentSongId = msg }, Cmd.none )
        SetSongName msg ->
            ( { model | currentSongName = msg }, Cmd.none )
        NextSong msg ->
            songToPlay model
        PlayNextSong ->
            songToPlay model
        PlayPreviousSong ->
            if (model.position == toFloat 1) then
                let
                    playlistLength = toFloat (List.length model.songs)
                    song = "song" ++ toString playlistLength
                in
                    ( { model | currentSongId = song, position = playlistLength }, Port.previous ([model.currentSongId, song]) )
            else
                let
                    song = "song" ++ toString (model.position - 1)
                in
                    ( { model | currentSongId = song, position = model.position - 1 }, Port.previous ([model.currentSongId, song]) )



songToPlay : Model -> ( Model, Cmd Msg )
songToPlay model =
        if toFloat (List.length model.songs) == model.position then
            ( { model | currentSongId = "song1", position = 1 }, Port.next ([model.currentSongId, "song1"]) )
        else
            let
                song = "song" ++ toString (model.position + 1)
            in
                ( { model | position = model.position + 1 }, Port.next ([model.currentSongId, song]) )

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "flex-grid content" ]
        [ div [ class "songs-wrapper" ]
            [ viewTitle model.title
            , viewSongs model.songs
            , songActions model
            ]
        ]

viewCircle : Model -> Html Msg
viewCircle model =
    let
        rInt = 35
        radius = toString rInt
        cInt = 3.14 * (rInt*2)
        circumference = toString cInt
        percentComplete = ((model.duration - model.currentTime) / model.duration) * 100
        pctInt = ((100-percentComplete)/100)*cInt
        pct = toString pctInt
    in
        svg [ id "circle", height 200,  width 100 ]
            [ circle [ id "circle-stroke", cx "40", cy "40", r radius, stroke "#FA5B19", strokeWidth "3", fill "transparent", strokeDasharray circumference, strokeDashoffset pct] []
            ]

viewSongs : List Song -> Html Msg
viewSongs songs =
    ul [] (List.map audioSong songs)

viewTitle : String -> Html Msg
viewTitle title =
    h2 [ id "playlist-title" ] [ text title ]

audioSong : Song -> Html Msg
audioSong song =
    li [ class "songs"]
        [ text song.songName
        , audio
            [ onTimeUpdate UpdateTime
            , onEnd NextSong
            , src song.songSource
            , name song.songName
            , id song.id
            , preload "none"
            ]
            []
       ]

viewPreviousButton : Bool -> Html Msg
viewPreviousButton playing =
    button [ class "fa fa-backward btn-back"
           , onClick PlayPreviousSong
           , disabled (not playing)
           ]
           []

viewNextButton : Bool -> Html Msg
viewNextButton playing =
    button [ class "fa fa-forward btn-forward"
           , onClick PlayNextSong
           , disabled (not playing)
           ]
           []

viewPlayButton : Bool -> Html Msg
viewPlayButton playing =
    if playing then
        button
            [ class "fa fa-pause btn-circle pause"
            , name "pause"
            , onClick Pause
            ]
            []
    else
        button
            [ class "fa fa-play btn-circle play"
            , name "play"
            , onClick Play
            ]
            []

songActions : Model -> Html Msg
songActions model =
    div [ id "song-action-container" ]
        [ viewPreviousButton model.playing
        , viewPlayButton model.playing
        , viewNextButton model.playing
        , viewCircle model
        ]

init : Flags -> (Model, Cmd Msg)
init flags =
    ( initialModel flags, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.setDuration SetDuration
        , Port.setSongId SetSongId
        , Port.setSongName SetSongName
        ]

main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

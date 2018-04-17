module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Port
import Json.Decode as Decode exposing (..)
import Svg exposing (svg, circle)
import Svg.Attributes exposing (cx, cy, r, stroke, fill, strokeWidth, strokeDasharray, strokeDashoffset)
import SongLibrary exposing (..)
import AnimationFrame
import Animation exposing (..)
import Time exposing (second)
import Ease exposing (..)

-- MSG

type Msg
    = Select String
    | Play
    | Pause
    | UpdateTime Float
    | SetDuration Float
    | SetSongId String
    | SetSongName String
    | NextSong String
    | PlayNextSong
    | PlayPreviousSong
    | CurrentTick Time.Time

-- MODEL

type alias Model =
    { playlist : String
    , activePlaylist : Bool
    , songs : List Song
    , currentSongId : String
    , currentSongName : String
    , currentTime : Float
    , duration : Float
    , playing : Bool
    , position : Float
    , style : Animation
    , currentBrowserTime : Float
    }

type alias Song =
    { songSource : String
    , songName : String
    , id : String
    }

initialModel : Model
initialModel =
    { playlist = ""
    , activePlaylist = False
    , songs = []
    , currentSongId = "song1"
    , currentSongName = ""
    , currentTime = 0.0
    , duration = 0.1
    , playing = False
    , position = 1
    , style = animation 0 |> from 0 |> to 0
    , currentBrowserTime = 0
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
        Select playlist ->
            case playlist of
                "2016" ->
                    ( { model | songs = songs2016, activePlaylist = True, playing = False, playlist = playlist, style = animateOpacity model.currentBrowserTime }, Cmd.none )
                "2017" ->
                    ( { model | songs = songs2017, activePlaylist = True, playing = False, playlist = playlist, style = animateOpacity model.currentBrowserTime }, Cmd.none )
                _ ->
                    ( { model | songs = [], activePlaylist = False, playing = False, style = animation 0 |> from 0 |> to 0 }, Cmd.none )
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
        CurrentTick time ->
            ( { model | currentBrowserTime = time }, Cmd.none)
        -- Animate animMsg ->
        --     ( { model
        --         | style = Animation.update animMsg model.style
        --       }
        --     , Cmd.none

animateOpacity : Time.Time -> Animation
animateOpacity browserTime =
    animation browserTime |> duration (1.5*second) |> from 0 |> to 1 |> ease Ease.inQuad

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
        [ div [] 
            [ div [ class "radios" ]
                [ viewRadioInput model "2016"
                , viewRadioInput model "2017"
                ],
              viewPlaylist model
            ]
        ]

viewPlaylist : Model -> Html Msg
viewPlaylist model =
    let 
        opacity = Animation.animate model.currentBrowserTime  model.style
    in
        div []
            [ div [ class "songs-wrapper", style [ ("opacity", toString opacity)] ]
                [ viewSongs model.songs
                , songActions model
                ]
            ]

viewRadioInput : Model -> String -> Html Msg
viewRadioInput model year =
    div [ class "" ]
    [ label [ class "list-container" ]
          [ input [ type_ "radio", onClick (Select year), checked (model.playlist == year) ] []
          , text ("Top Ten Songs, " ++ year)
          , span [ class "radio-btn" ] []
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
            , preload "metadata"
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
    if model.activePlaylist then
        div [ id "song-action-container" ]
            [ viewPreviousButton model.playing
            , viewPlayButton model.playing
            , viewNextButton model.playing
            , viewCircle model
            ]
    else
        div [] []

init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Port.setDuration SetDuration
        , Port.setSongId SetSongId
        , Port.setSongName SetSongName
        , AnimationFrame.times CurrentTick
        ]

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

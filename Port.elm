port module Port exposing (..)

port play : (String) -> Cmd msg
port pause : (String) -> Cmd msg
port setDuration : (Float -> msg) -> Sub msg

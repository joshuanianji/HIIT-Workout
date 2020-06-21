port module Ports exposing (playTada, playTick, playWhistle, storeConfig, storeConfigSuccess)

import Json.Encode


port storeConfig : Json.Encode.Value -> Cmd msg


port storeConfigSuccess : (() -> msg) -> Sub msg


port playWhistle : () -> Cmd msg


port playTada : () -> Cmd msg


port playTick : () -> Cmd msg

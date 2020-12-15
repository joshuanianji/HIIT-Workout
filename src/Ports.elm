port module Ports exposing (playSound, speak, storeConfig, storeConfigSuccess, workoutStatus)

import Json.Encode


port storeConfig : Json.Encode.Value -> Cmd msg


port storeConfigSuccess : (() -> msg) -> Sub msg


port playSound : String -> Cmd msg



-- speaker stuff


port speak : String -> Cmd msg



-- Could either "end" or "start".


port workoutStatus : String -> Cmd msg

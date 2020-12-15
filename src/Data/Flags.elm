module Data.Flags exposing (Flags, Images, WindowSize)

import Json.Encode



---- TYPE ----


type alias Flags =
    { windowSize : WindowSize
    , storedConfig : Json.Encode.Value
    , showIosInstall : Bool
    , images : Images
    , version : Maybe String
    }


type alias Images =
    { smhSrc : String
    , iosShareIconSrc : String
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }

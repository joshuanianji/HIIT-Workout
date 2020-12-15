module Data.SharedState exposing (SharedState, SharedStateUpdate(..), init, mapUpdate, navigateTo, update)

import Browser.Navigation as Nav
import Data.Config
import Data.Flags exposing (Flags, Images, WindowSize)
import Data.PopupCmd as PopupCmd exposing (PopupCmd)
import Element
import Http
import Routes exposing (Route)
import Util


type alias SharedState =
    { version : String
    , windowSize : WindowSize
    , device : Element.Device
    , key : Nav.Key
    , images : Images

    -- internal configuration data. Basically the cached, parsed local storage.
    , configCache : Data.Config.Data
    }



-- when we init we don't have the version number yet - this comes with the HTTP request


init : Flags -> Nav.Key -> SharedState
init flags key =
    let
        -- TODO: use error
        ( configCache, mErr ) =
            Data.Config.init flags.storedConfig
    in
    { version = Maybe.withDefault "build..." flags.version
    , windowSize = flags.windowSize
    , device = Element.classifyDevice flags.windowSize
    , key = key
    , images = flags.images
    , configCache = configCache
    }


type SharedStateUpdate msg
    = NewWindowSize Int Int
    | UpdateConfigCache Data.Config.Data
    | PopupCmd (PopupCmd msg)
    | NoUpdate


update : SharedStateUpdate msg -> SharedState -> SharedState
update msg sharedState =
    case msg of
        NewWindowSize width height ->
            { sharedState
                | windowSize = WindowSize width height
                , device = Element.classifyDevice <| WindowSize width height
            }

        UpdateConfigCache newCache ->
            { sharedState | configCache = newCache }

        PopupCmd _ ->
            sharedState

        NoUpdate ->
            sharedState



---- HELPERS ----


navigateTo : Route -> SharedState -> Cmd msg
navigateTo route sharedState =
    Routes.navigateTo sharedState.key route


mapUpdate : (msg1 -> msg2) -> SharedStateUpdate msg1 -> SharedStateUpdate msg2
mapUpdate f ssUpdate =
    case ssUpdate of
        NewWindowSize x y ->
            NewWindowSize x y

        PopupCmd popupCmd ->
            PopupCmd <| PopupCmd.map f popupCmd

        UpdateConfigCache cache ->
            UpdateConfigCache cache

        NoUpdate ->
            NoUpdate

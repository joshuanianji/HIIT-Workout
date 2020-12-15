module Data.PopupCmd exposing (PopupCmd(..), getPopup, map, none, remove, show)

-- Communicate Popups

import Modules.Popup as Popup



-- TYPE


type PopupCmd msg
    = ShowPopup (Popup.Config msg)
    | RemovePopup
    | DoNothing



-- CONSTRUCTORS


show : Popup.Config msg -> PopupCmd msg
show =
    ShowPopup


remove : PopupCmd msg
remove =
    RemovePopup


none : PopupCmd msg
none =
    DoNothing



-- HELPERS


map : (msg1 -> msg2) -> PopupCmd msg1 -> PopupCmd msg2
map f cmd =
    case cmd of
        ShowPopup config ->
            ShowPopup <| Popup.map f config

        RemovePopup ->
            RemovePopup

        DoNothing ->
            DoNothing


getPopup : PopupCmd msg -> Maybe (Popup.Config msg)
getPopup cmd =
    case cmd of
        ShowPopup config ->
            Just config

        _ ->
            Nothing

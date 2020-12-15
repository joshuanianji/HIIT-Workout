module Modules.Popup exposing (Config, map, mapMaybe, view)

{-| Totally ripped off of [PaackEng's Elm UI Dialog](https://package.elm-lang.org/packages/PaackEng/elm-ui-dialog/1.0.0/) with some minor changes
-}

import Colours
import Element exposing (Attribute, Element, column, el, fill, height, map, none, padding, rgba, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import FeatherIcons
import Util


view : Maybe (Config msg) -> Element msg
view maybeConfig =
    case maybeConfig of
        Nothing ->
            none

        Just config ->
            el
                ([ Background.color dialogMask
                 , width fill
                 , height fill
                 ]
                    ++ config.maskAttributes
                )
            <|
                column config.containerAttributes
                    [ wrapHeader config
                    , wrapBody config
                    , wrapFooter config
                    ]


wrapHeader : Config msg -> Element msg
wrapHeader { header, headerAttributes, closeMessage } =
    if header == Nothing && closeMessage == Nothing then
        none

    else
        row
            ([ width fill, padding 2 ] ++ headerAttributes)
            [ el [ width fill ] <| Maybe.withDefault none header
            , maybe none closeButton closeMessage
            ]


closeButton : msg -> Element msg
closeButton closeMessage =
    Element.el
        [ padding 8 ]
    <|
        Util.viewIcon
            { icon = FeatherIcons.x
            , color = Colours.sunset
            , size = 20
            , msg = Just closeMessage
            , withBorder = False
            }


wrapBody : Config msg -> Element msg
wrapBody { body, bodyAttributes } =
    case body of
        Nothing ->
            none

        Just body_ ->
            el ([ width fill, padding 1 ] ++ bodyAttributes) body_


wrapFooter : Config msg -> Element msg
wrapFooter { footer, footerAttributes } =
    case footer of
        Nothing ->
            none

        Just footer_ ->
            el ([ width fill, padding 1 ] ++ footerAttributes) footer_


dialogMask : Element.Color
dialogMask =
    rgba 0 0 0 0.3


type alias Config msg =
    { closeMessage : Maybe msg
    , maskAttributes : List (Attribute msg)
    , containerAttributes : List (Attribute msg)
    , headerAttributes : List (Attribute msg)
    , bodyAttributes : List (Attribute msg)
    , footerAttributes : List (Attribute msg)
    , header : Maybe (Element msg)
    , body : Maybe (Element msg)
    , footer : Maybe (Element msg)
    }


map : (a -> b) -> Config a -> Config b
map f config =
    { closeMessage = Maybe.map f config.closeMessage
    , maskAttributes = List.map (Element.mapAttribute f) config.maskAttributes
    , containerAttributes = List.map (Element.mapAttribute f) config.containerAttributes
    , headerAttributes = List.map (Element.mapAttribute f) config.headerAttributes
    , bodyAttributes = List.map (Element.mapAttribute f) config.bodyAttributes
    , footerAttributes = List.map (Element.mapAttribute f) config.footerAttributes
    , header = Maybe.map (Element.map f) config.header
    , body = Maybe.map (Element.map f) config.body
    , footer = Maybe.map (Element.map f) config.footer
    }


{-| For convenience, a varient of `map` which assumes you're dealing with a `Maybe (Config a)`, which is often the case.
-}
mapMaybe : (a -> b) -> Maybe (Config a) -> Maybe (Config b)
mapMaybe =
    Maybe.map << map


maybe : b -> (a -> b) -> Maybe a -> b
maybe default f value =
    case value of
        Just value_ ->
            f value_

        Nothing ->
            default

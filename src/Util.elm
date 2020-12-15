module Util exposing
    ( Position(..)
    , centerOverlay
    , httpErrorToString
    , isVerticalPhone
    , surround
    , textButton
    , viewIcon
    , withTooltip
    )

-- misc functions

import Colours
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Html.Attributes
import Http


surround : Int -> Int -> Int -> Element msg -> Element msg
surround left middle right =
    \el ->
        Element.row
            [ Element.width Element.fill, Element.height Element.fill ]
            [ Element.el [ Element.width <| Element.fillPortion left, Element.height Element.fill ] Element.none
            , Element.el [ Element.width <| Element.fillPortion middle, Element.height Element.fill ] el
            , Element.el [ Element.width <| Element.fillPortion right, Element.height Element.fill ] Element.none
            ]


viewIcon :
    { icon : Icon.Icon
    , color : Element.Color
    , size : Float
    , msg : Maybe msg
    , withBorder : Bool
    }
    -> Element msg
viewIcon data =
    let
        icon =
            data.icon
                |> Icon.withStrokeWidth 1
                |> Icon.withSize data.size
                |> Icon.toHtml []
                |> Element.html

        similarAttrs =
            [ Font.color data.color
            , Element.padding (round data.size // 4)
            , Border.rounded (round data.size)
            ]
                ++ -- border stuff
                   (if data.withBorder then
                        [ Element.pointer
                        , Border.width 1
                        , Border.color data.color
                        , Element.mouseOver
                            [ Font.color Colours.white
                            , Background.color data.color
                            ]
                        ]

                    else
                        []
                   )
    in
    case data.msg of
        -- don't make it interactive
        Nothing ->
            Element.el similarAttrs icon

        Just msg ->
            Element.el
                (Events.onClick msg :: similarAttrs)
                icon



-- if the user is on a phone and is vertical (would often need to change layout for this)


isVerticalPhone : Element.Device -> Bool
isVerticalPhone device =
    case ( device.class, device.orientation ) of
        ( Element.Phone, Element.Portrait ) ->
            True

        _ ->
            False


withTooltip : { position : Position, content : String } -> Element msg -> Element msg
withTooltip { position, content } =
    Element.el
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.htmlAttribute <| Html.Attributes.class (positionToClass position)
        , Element.htmlAttribute <| Html.Attributes.attribute "data-tooltip" content
        ]


centerOverlay : Element msg -> Element.Attribute msg
centerOverlay =
    Element.el [ Element.centerY, Element.centerX ]
        >> Element.inFront


type Position
    = Top
    | Bottom
    | Left
    | Right



-- internal tooltip stuff


positionToClass : Position -> String
positionToClass p =
    let
        str =
            case p of
                Top ->
                    "top"

                Bottom ->
                    "bottom"

                Left ->
                    "left"

                Right ->
                    "right"
    in
    "simptip-position-" ++ str


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "BadUrl! " ++ url

        Http.Timeout ->
            "Timeout!"

        Http.NetworkError ->
            "NetworkError!"

        Http.BadStatus status ->
            "BadStatus! " ++ String.fromInt status

        Http.BadBody body ->
            "BadBody! " ++ body


textButton :
    { msg : msg
    , color : Element.Color
    , text : String
    }
    -> Element msg
textButton { msg, color, text } =
    Input.button
        [ Element.centerX
        , Element.paddingXY 12 8
        , Font.light
        , Font.size 20
        , Font.color color
        , Border.width 1
        , Border.color color
        , Border.rounded 4
        , Element.mouseOver
            [ Background.color color
            , Font.color Colours.white
            ]
        ]
        { onPress = Just msg
        , label = Element.text text
        }

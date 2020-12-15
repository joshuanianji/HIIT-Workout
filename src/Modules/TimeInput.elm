module Modules.TimeInput exposing (Msg, TimeInput, getDuration, init, update, view)

import Colours
import Data.Duration as Duration exposing (Duration)
import Element exposing (Element)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Util



---- TYPE ----


type TimeInput
    = TimeInput Data



-- internal data


type alias Data =
    { input : String
    , duration : Duration
    , focused : Bool
    }



-- user puts these in when they run view function


type alias Options =
    { displayText : Maybe String
    , device : Element.Device
    }



-- initialize with seconds


init : Int -> TimeInput
init secs =
    let
        duration =
            Duration.init secs
    in
    TimeInput
        { input = Duration.toString duration
        , duration = duration
        , focused = False
        }



-- HELPER FUNCTIONS


getDuration : TimeInput -> Duration
getDuration (TimeInput data) =
    data.duration



---- VIEW ----


view : Options -> TimeInput -> Element Msg
view options (TimeInput data) =
    let
        label =
            options.displayText
                |> Maybe.map Element.text
                |> Maybe.map (Element.el [ Element.centerY ])
                |> Maybe.map
                    (Element.el
                        [ Font.light
                        , Element.height (Element.px 50)
                        , Element.centerY
                        , Element.padding 8
                        ]
                    )

        input =
            Input.text
                [ Element.width (Element.px 175)
                , Element.padding 24
                , Element.htmlAttribute <| Html.Attributes.class "no-cursor"

                -- works for android
                , Element.htmlAttribute <| Html.Attributes.type_ "number"

                -- for iOS maybe
                , Element.htmlAttribute <| Html.Attributes.attribute "inputmode" "numeric"
                , Element.htmlAttribute <| Html.Attributes.attribute "pattern" "[0-9]*"

                -- idk if this is even useful tbh
                , Element.htmlAttribute <| Html.Attributes.min "0"

                -- FONT SIZE 0 HIDES THE STUPID UGLY BEHIND-THE-SCENES TEXT
                , Font.size 0

                -- it's weird how inFront works better  - if i write behindContent, it'll take two clicks to focus on the input.
                , Element.inFront <|
                    -- if it's not focused, we would have gotten the new sanitized object
                    Element.row
                        [ Element.padding 8
                        , Element.spacing 3
                        , Element.centerY
                        , Element.alignRight
                        , Font.size 20
                        , Font.light
                        , Font.color Colours.black

                        -- cursor
                        , Element.inFront <|
                            if data.focused then
                                Element.el
                                    [ Element.centerY
                                    , Element.padding 8
                                    , Element.height Element.fill
                                    , Element.alignRight
                                    , Element.htmlAttribute <| Html.Attributes.class "blinking"
                                    ]
                                    (Element.el
                                        [ Element.height Element.fill
                                        , Element.width (Element.px 1)
                                        , Background.color Colours.black
                                        ]
                                        Element.none
                                    )

                            else
                                Element.none
                        ]
                        [ Duration.viewFancy data.duration ]
                , Events.onFocus <| UpdateFocus True
                , Events.onLoseFocus <| UpdateFocus False
                ]
                { onChange = UpdateInput
                , text = data.input
                , placeholder = Nothing
                , label = Input.labelHidden <| Maybe.withDefault "" options.displayText
                }
    in
    if Util.isVerticalPhone options.device then
        Element.column
            [ Element.centerX ]
            (List.map (Element.el [ Element.centerX ])
                [ Maybe.withDefault Element.none label
                , input
                ]
            )

    else
        case label of
            Just l ->
                Element.el
                    [ Element.onRight <|
                        Element.el [ Element.centerY ] input
                    , Element.onLeft l
                    , Element.centerX
                    , Element.height (Element.px 64)
                    , Element.padding 4

                    -- HACK
                    , Element.moveDown 0
                    ]
                    Element.none

            Nothing ->
                input



---- UPDATE ----


type Msg
    = UpdateInput String
    | UpdateFocus Bool



-- the extra bool is whether or not to save the configuration into the local storage
-- I save to local storage whenever I lose focus


update : Msg -> TimeInput -> ( TimeInput, Bool )
update msg (TimeInput data) =
    case msg of
        UpdateInput newInput ->
            let
                -- so we won't get "stuck" at 0003 or something
                -- this changes 0012 -> 12
                removeLeftZeroes str =
                    String.foldl
                        (\c acc ->
                            if acc == "" && c == '0' then
                                ""

                            else
                                acc ++ String.fromChar c
                        )
                        ""
                        str

                sanitizedInput =
                    newInput
                        |> String.filter Char.isDigit
                        |> removeLeftZeroes
                        |> String.left 4
            in
            ( TimeInput
                { data
                    | input = sanitizedInput
                    , duration =
                        Duration.fromString sanitizedInput
                            |> Maybe.withDefault (Duration.init 0)
                }
            , False
            )

        UpdateFocus isFocused ->
            if isFocused then
                ( TimeInput
                    { data | focused = True }
                , False
                )

            else
                let
                    sanitizedDuration =
                        Duration.fromString data.input
                            |> Maybe.withDefault (Duration.init 0)
                            |> Duration.sanitize
                in
                -- sanitize data
                ( TimeInput
                    { data
                        | focused = False
                        , duration = sanitizedDuration
                        , input = Duration.toString sanitizedDuration
                    }
                , True
                )

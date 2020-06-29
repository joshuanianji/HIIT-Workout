module View.Application exposing
    ( Application
    , Msg
    , endWorkout
    , exercising
    , init
    , subscriptions
    , update
    , updateData
    , view
    )

import Browser.Events
import Colours
import Data.Application as Data exposing (Data)
import Data.Config
import Data.Flags as Flags exposing (Flags)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import FeatherIcons as Icon
import Keyboard exposing (Key)
import List.Nonempty exposing (Nonempty(..))
import Ports
import Time
import Util



---- TYPE ----


type Application
    = Application Model Data



-- other important data


type alias Model =
    { keys : List Key -- keys pressed down
    , smhSrc : String
    , device : Element.Device
    }



-- should only be called once


init : Data.Config.Data -> Flags -> Application
init data flags =
    Data.fromConfig data
        |> Application
            { keys = []
            , smhSrc = flags.smhSrc
            , device = Element.classifyDevice flags.windowSize
            }



-- every time the user switches to the application page


updateData : Data.Config.Data -> Application -> Application
updateData data (Application model _) =
    Data.fromConfig data
        |> Application model



-- Main.elm uses this to know whether or not they need to show the settings page at the bottom


exercising : Application -> Bool
exercising (Application _ data) =
    case data.state of
        Data.InProgress _ ->
            True

        _ ->
            False



-- Main.elm also uses this to end the workout
-- changing state so it won't keep ticking when we're on the settings page
-- I just arbitratily put the state as Finished - honestly, anything that's not Data.InProgress will work


endWorkout : Application -> Application
endWorkout (Application model _) =
    Application model
        { playing = False
        , state = Data.Finished
        }



---- VIEW ----


view : Application -> Element Msg
view (Application model data) =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Util.viewIcon
            { icon = Icon.zap
            , color = Colours.sunset
            , size = 50
            , msg = Nothing
            }
            |> Element.el [ Element.centerX ]

        -- the main view
        , case data.state of
            Data.Starting blocks ->
                -- I have to use Element.inFront to ensure that both the upper and lower blocks are the SAME height
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.below <|
                            Element.el
                                [ Element.centerX
                                , Element.moveUp 56
                                , Element.padding 4
                                ]
                            <|
                                Util.viewIcon
                                    { icon = Icon.play
                                    , color = Colours.sunset
                                    , size = 75
                                    , msg = Just <| StartExercise blocks
                                    }
                        , Element.inFront <|
                            Element.paragraph
                                [ Element.centerX
                                , Element.centerY
                                , Font.size 50
                                , Font.center
                                , Font.color Colours.sunset
                                ]
                                [ Element.text "Ready?"
                                ]
                        ]
                        Element.none
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.inFront <|
                            Element.paragraph
                                [ Element.centerX
                                , Element.centerY
                                , Font.color Colours.sunset
                                , Font.size 20
                                , Font.center
                                , Font.light
                                ]
                                [ Element.el [ Font.bold ] <| Element.text "Note:"
                                , Element.text " if you press play, you "
                                , Element.el [ Font.bold ] <| Element.text "cannot"
                                , Element.text " go back to the settings page without resetting your workout!"
                                ]
                        ]
                        Element.none
                    ]

            Data.InProgress blocks ->
                let
                    bigFont size color label =
                        Element.el
                            [ Element.centerX
                            , Font.size size
                            , Font.center
                            , Font.color color
                            , Font.light
                            ]
                        <|
                            Element.text label

                    timerText secsLeft color =
                        Element.el
                            [ Element.centerX
                            , Font.color color
                            , Font.size 125
                            ]
                        <|
                            Element.text <|
                                String.fromInt secsLeft

                    timerBar secsLeft total color =
                        Element.row
                            [ Element.width Element.fill ]
                            [ Element.el
                                [ Element.width <| Element.fillPortion secsLeft
                                , Element.height (Element.px 5)
                                , Background.color color
                                ]
                                Element.none
                            , Element.el
                                [ Element.width <| Element.fillPortion (total - secsLeft) ]
                                Element.none
                            ]

                    dataGroup =
                        case List.Nonempty.head blocks of
                            Data.CountDown secsLeft total ->
                                { upperElem = bigFont 32 Colours.sky "Countdown"
                                , timerText = timerText secsLeft Colours.sky
                                , timerBar = timerBar secsLeft total Colours.sky
                                , theme = Colours.sky
                                }

                            Data.ExerciseBreak secsLeft total ->
                                { upperElem = bigFont 32 Colours.grass "Break Between Exercise"
                                , timerText = timerText secsLeft Colours.grass
                                , timerBar = timerBar secsLeft total Colours.grass
                                , theme = Colours.grass
                                }

                            Data.SetBreak secsLeft total ->
                                { upperElem = bigFont 32 Colours.grass "Break Between Sets"
                                , timerText = timerText secsLeft Colours.grass
                                , timerBar = timerBar secsLeft total Colours.grass
                                , theme = Colours.grass
                                }

                            Data.Exercise { setName, name, duration, secsLeft } ->
                                { upperElem =
                                    Element.column
                                        [ Element.centerX
                                        , Element.spacing 16
                                        , Font.size 32
                                        , Font.center
                                        , Font.color Colours.sky
                                        ]
                                        [ bigFont 32 Colours.sunflower setName
                                        , bigFont 48 Colours.sunset name
                                        ]
                                , timerText = timerText secsLeft Colours.sunset
                                , timerBar = timerBar secsLeft duration Colours.sunset
                                , theme = Colours.sunset
                                }

                    nextupString =
                        case List.head <| List.Nonempty.tail blocks of
                            Just (Data.ExerciseBreak _ _) ->
                                "Break"

                            Just (Data.SetBreak _ _) ->
                                "Break"

                            Just (Data.Exercise d) ->
                                d.name

                            _ ->
                                "Workout Completion"

                    nextup =
                        Element.paragraph
                            [ Element.centerX
                            , Font.size 32
                            , Font.color Colours.sky
                            , Font.light
                            ]
                            [ Element.text "Next up: "
                            , Element.text nextupString
                            ]

                    centerButtonIcon =
                        if data.playing then
                            Icon.pause

                        else
                            Icon.play
                in
                Element.column
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ -- Toggle button and all things above the button
                      Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.below
                            (Util.viewIcon
                                { icon = centerButtonIcon
                                , color = dataGroup.theme
                                , size = 75
                                , msg = Just TogglePlay
                                }
                                |> Element.el
                                    [ Element.centerX
                                    , Element.moveUp 56
                                    , Element.padding 4
                                    , Background.color Colours.white
                                    ]
                                |> Element.el
                                    [ Element.width Element.fill
                                    , Element.behindContent <|
                                        Element.el
                                            [ Element.width Element.fill ]
                                            dataGroup.timerBar
                                    ]
                            )
                        , Element.inFront <|
                            Element.el
                                [ Element.centerX
                                , Element.centerY
                                ]
                                dataGroup.upperElem
                        ]
                        Element.none

                    -- all things below the button
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.inFront <|
                            Element.column
                                [ Element.centerX
                                , Element.centerY
                                , Element.spacing 16
                                ]
                                [ dataGroup.timerText
                                , nextup
                                ]
                        ]
                        Element.none
                    ]

            Data.Finished ->
                Element.column
                    [ Element.width Element.fill
                    , Element.centerY
                    , Element.spacing 32
                    ]
                    [ Util.viewIcon
                        { icon = Icon.star
                        , color = Colours.sunflower
                        , size = 100
                        , msg = Nothing
                        }
                        |> Element.el
                            [ Element.centerX ]
                    , Element.paragraph
                        [ Font.color Colours.sunflower
                        , Font.center
                        , Font.light
                        , Font.size 50
                        ]
                        [ Element.text "WOOHOO!" ]
                    , Element.paragraph
                        [ Font.color Colours.sunflower
                        , Font.center
                        , Font.light
                        ]
                        [ Element.text "Congratulations! You finished!" ]
                    ]

            Data.NeverStarted ->
            -- the smh screen
                Element.column
                    [ Element.width Element.fill
                    , Element.centerY
                    , Element.spacing 32
                    ]
                    [ Element.image
                        [ Element.width (Element.px 125)
                        , Element.centerX
                        ]
                        { src = model.smhSrc
                        , description = "Sokka is disappointed in your workout"
                        }
                    , Element.paragraph
                        [ Font.color Colours.sunset
                        , Font.center
                        , Font.light
                        , Font.size 50
                        ]
                        [ Element.text "Disappointed." ]
                    , Element.textColumn
                        [ Element.centerX
                        , Element.spacing 4
                        , Font.light
                        , Font.center
                        , Font.color Colours.sunset
                        ]
                        [ Element.paragraph
                            []
                            [ Element.text "You didn't put anything in your workout!" ]
                        , Element.paragraph
                            []
                            [ Element.text "Go to the settings and try again." ]
                        ]
                    ]
        ]



---- UPDATE ----


type Msg
    = NewWindowSize Int Int
    | StartExercise (Nonempty Data.TimeBlock)
    | NextSecond
    | TogglePlay
    | KeyMsg Keyboard.Msg -- so we can react upon the space key press


update : Msg -> Application -> ( Application, Cmd Msg )
update msg (Application model data) =
    case msg of
        NewWindowSize width height ->
            ( Application { model | device = Element.classifyDevice <| Flags.WindowSize width height } data, Cmd.none )

        StartExercise blocks ->
            ( Application model
                { data
                    | state = Data.InProgress blocks
                    , playing = True
                }
            , Ports.playWhistle ()
            )

        NextSecond ->
            case data.state of
                Data.InProgress (Nonempty block tl) ->
                    case Data.decreaseTimeBlock block of
                        Nothing ->
                            case tl of
                                -- no more exercises
                                [] ->
                                    ( Application model { data | state = Data.Finished }, Ports.playTada () )

                                x :: xs ->
                                    ( Application model { data | state = Data.InProgress <| Nonempty x xs }, Ports.playWhistle () )

                        Just newBlock ->
                            let
                                cmd =
                                    if Data.timeLeft newBlock <= 3 then
                                        Ports.playTick ()

                                    else
                                        Cmd.none
                            in
                            ( Application model { data | state = Data.InProgress <| Nonempty newBlock tl }, cmd )

                _ ->
                    -- ignore
                    ( Application model data, Cmd.none )

        TogglePlay ->
            ( Application model { data | playing = not data.playing }
            , if data.playing then
                Cmd.none

              else
                Ports.playWhistle ()
            )

        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.keys

                newModel =
                    { model | keys = newKeys }
            in
            if newKeys == [ Keyboard.Spacebar ] then
                update TogglePlay (Application newModel data)

            else
                ( Application model data, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Application -> Sub Msg
subscriptions (Application _ data) =
    let
        tickSub =
            case data.state of
                Data.InProgress _ ->
                    if data.playing then
                        Time.every 1000 (always NextSecond)

                    else
                        Sub.none

                _ ->
                    Sub.none
    in
    Sub.batch
        [ Sub.map KeyMsg Keyboard.subscriptions
        , Browser.Events.onResize NewWindowSize
        , tickSub
        ]

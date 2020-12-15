module Page.Workout exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Colours
import Data.Config
import Data.Duration as Duration
import Data.Flags exposing (Flags, Images)
import Data.SharedState as SharedState exposing (SharedState, SharedStateUpdate)
import Data.TimeBlock as TimeBlock
import Data.Workout as Data exposing (Data)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Keyboard exposing (Key)
import List.Nonempty exposing (Nonempty(..))
import Ports
import Routes exposing (Route)
import Time
import Util



---- TYPE ----


type Model
    = Model AppModel Data



-- other important data


type alias AppModel =
    { keys : List Key -- keys pressed down
    }



-- should only be called once


init : SharedState -> ( Model, Cmd Msg )
init ({ configCache } as sharedState) =
    ( Model
        { keys = []
        }
        (Data.fromConfig configCache)
    , Cmd.none
    )



--- Helpers


exercising : Data -> Bool
exercising data =
    case data.state of
        Data.InProgress _ ->
            True

        _ ->
            False



---- VIEW ----


view : SharedState -> Model -> Element Msg
view sharedState (Model model data) =
    let
        content =
            case data.state of
                Data.Starting workoutData ->
                    viewStarting workoutData sharedState

                Data.InProgress workoutData ->
                    viewInProgress workoutData sharedState data

                Data.Finished ->
                    viewFinished

                Data.NeverStarted ->
                    -- the smh screen
                    viewNeverStarted sharedState.images model
    in
    let
        phoneView =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                ]
                [ -- "nav bar"
                  Element.row
                    [ Element.width Element.fill
                    , Element.padding 8
                    , Element.inFront <|
                        Element.el
                            [ Element.alignRight
                            , Element.padding 16
                            ]
                        <|
                            Util.viewIcon
                                { icon = Icon.x
                                , color = Colours.sunset
                                , size = 30
                                , msg = Just <| NavigateTo True Routes.Config
                                , withBorder = False
                                }
                    ]
                    [ Element.el [ Element.centerX ] <|
                        Util.viewIcon
                            { icon = Icon.zap
                            , color = Colours.sunset
                            , size = 45
                            , msg = Nothing
                            , withBorder = False
                            }
                    ]
                , content
                ]

        desktopView =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 16
                ]
                [ -- zap icon at the top
                  Util.viewIcon
                    { icon = Icon.zap
                    , color = Colours.sunset
                    , size = 50
                    , msg = Nothing
                    , withBorder = False
                    }
                    |> Element.el [ Element.centerX ]
                , content
                , if exercising data then
                    Util.viewIcon
                        { icon = Icon.x
                        , color = Colours.sunset
                        , size = 40
                        , msg = Just <| NavigateTo True Routes.Config
                        , withBorder = True
                        }
                        |> Util.withTooltip
                            { position = Util.Top
                            , content = "Exit the workout"
                            }
                        |> Element.el
                            [ Element.centerX
                            , Element.alignBottom
                            ]

                  else
                    Util.viewIcon
                        { icon = Icon.settings
                        , color = Colours.sky
                        , size = 40
                        , msg = Just <| NavigateTo True Routes.Config
                        , withBorder = True
                        }
                        |> Element.el
                            [ Element.centerX
                            , Element.alignBottom
                            ]
                ]
    in
    if Util.isVerticalPhone sharedState.device then
        phoneView

    else
        desktopView


viewStarting : Data.WorkoutData -> SharedState -> Element Msg
viewStarting workoutData sharedState =
    let
        title n showInfo =
            Element.column
                [ Element.spacing 64 ]
                [ Element.paragraph
                    [ Font.size n
                    , Font.center
                    , Font.color Colours.sunset
                    ]
                    [ Element.text "Ready?" ]
                , if showInfo then
                    info (n // 2)

                  else
                    Element.none
                ]
                |> Util.surround 1 5 1

        info n =
            Element.paragraph
                [ Font.light
                , Font.size n
                , Font.center
                , Font.color Colours.sunset
                ]
                [ Element.text "Your workout is "
                , String.fromInt workoutData.info.totalExercises
                    ++ " exercises long, "
                    |> Element.text
                    |> Element.el [ Font.color Colours.sunflower ]
                , Element.text "totalling "
                , Element.el [ Font.color Colours.sunflower ] <| Duration.viewFancy workoutData.info.totalTime
                ]

        startExerciseButton =
            Util.viewIcon
                { icon = Icon.play
                , color = Colours.sunset
                , size = 75
                , msg = Just <| StartExercise workoutData
                , withBorder = True
                }

        subTitle n displayPortrait =
            let
                heading =
                    if displayPortrait then
                        Element.el
                            [ Element.centerX
                            , Font.bold
                            , Font.size 32
                            ]
                        <|
                            Element.text "Note:"

                    else
                        Element.el [ Font.bold ] <| Element.text "Note:"

                message =
                    Element.paragraph
                        []
                        [ Element.text " if you press play, you "
                        , Element.el [ Font.bold ] <| Element.text "cannot"
                        , Element.text " go back to the settings page without resetting your workout!"
                        ]

                attrs =
                    [ Font.color Colours.sunset
                    , Font.size n
                    , Font.center
                    , Font.light
                    ]
            in
            if displayPortrait then
                Element.column (Element.spacing 8 :: attrs) [ heading, message ]

            else
                Element.paragraph attrs [ heading, message ]
                    |> Util.surround 1 4 1
    in
    -- I have to use Util.centerOverlay to ensure that both the upper and lower blocks are the SAME height
    if Util.isVerticalPhone sharedState.device then
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.padding 16
            ]
            [ Element.el
                [ Element.height Element.fill
                , Element.width Element.fill

                -- don't show info on phones
                , Util.centerOverlay <| title 50 False
                ]
                Element.none
            , Element.el [ Element.centerX ] startExerciseButton
            , Element.el
                [ Element.height Element.fill
                , Element.width Element.fill
                , Util.centerOverlay <| subTitle 20 False
                ]
                Element.none
            ]

    else
        let
            data =
                case sharedState.device.orientation of
                    -- the icons are in a column, but the Ready and the Note are in the left and ride sides respectively
                    Element.Landscape ->
                        { parentElem = Element.row
                        , titleSize = 75
                        , subTitleElem = subTitle 25 True
                        , subTitleSurround = Util.surround 1 2 1
                        , startExerciseButtonParent =
                            Element.el
                                [ Element.centerY
                                , Element.width Element.shrink
                                ]
                        }

                    Element.Portrait ->
                        { parentElem = Element.column
                        , titleSize = sharedState.windowSize.height // 18
                        , subTitleElem = subTitle (sharedState.windowSize.height // 36) False
                        , subTitleSurround = identity
                        , startExerciseButtonParent =
                            Element.el
                                [ Element.centerX
                                , Element.padding 4
                                ]
                        }
        in
        data.parentElem
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Util.centerOverlay <| title data.titleSize True
                ]
                Element.none
            , data.startExerciseButtonParent
                startExerciseButton
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Util.centerOverlay data.subTitleElem
                ]
                Element.none
                |> data.subTitleSurround
            ]


viewInProgress : Data.WorkoutData -> SharedState -> Data -> Element Msg
viewInProgress workoutData sharedState data =
    let
        playPauseIcon =
            if data.playing then
                Icon.pause

            else
                Icon.play

        -- maybeData is Just { secsLeft, totalTime }  when it's a larger screen that can hold that data
        pebbles currBlockColour maybeData =
            let
                currExerciseNum =
                    List.Nonempty.length workoutData.blocksLeft

                regularPebble height color =
                    Element.el
                        [ Element.width Element.fill
                        , Element.centerY
                        , Element.height (Element.px height)
                        , Border.rounded 3
                        , Background.color color
                        ]
                        Element.none

                viewPebble n =
                    let
                        -- if the user is not on a phone, we'll make it have the little timer bar for the current pebble
                        specialPebble height colour =
                            case maybeData of
                                Just { secsLeft, totalTime } ->
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.centerY
                                        , Element.height (Element.px height)
                                        , Border.rounded <| height // 3
                                        , Border.width 1
                                        , Border.color colour
                                        ]
                                        [ Element.el
                                            [ Element.width <| Element.fillPortion (totalTime - secsLeft) ]
                                            Element.none
                                        , Element.el
                                            [ Element.width <| Element.fillPortion secsLeft ]
                                          <|
                                            regularPebble height colour
                                        ]

                                Nothing ->
                                    regularPebble height colour

                        ( regHeight, specialHeight ) =
                            case maybeData of
                                Just _ ->
                                    ( 10, 16 )

                                _ ->
                                    ( 5, 9 )

                        ( pebbleHeight, pebbleColour, useSpecial ) =
                            if n == currExerciseNum then
                                -- this pebble represents the current exercise
                                ( specialHeight, currBlockColour, True )

                            else if n > currExerciseNum then
                                ( regHeight, Colours.lightGray, False )

                            else
                                ( regHeight, Colours.withAlpha 0.6 Colours.sky, False )
                    in
                    if useSpecial then
                        specialPebble pebbleHeight pebbleColour

                    else
                        regularPebble pebbleHeight pebbleColour
            in
            List.range 1 workoutData.info.totalTimeblocks
                |> List.reverse
                |> List.map viewPebble
                |> Element.row
                    [ Element.spacing 2
                    , Element.width Element.fill
                    ]

        -- isSmall is true if it's a phone or something or something
        viewPortrait isSmall =
            let
                bigFont size color label =
                    Element.paragraph
                        [ Element.centerX
                        , Font.size size
                        , Font.center
                        , Font.color color
                        , Font.light
                        ]
                        [ Element.text label ]

                -- the sizes of the elements
                sizesData =
                    if isSmall then
                        { title = 32
                        , exercise = 48
                        , buttonMaxWidth = 150
                        , buttonIcon = 30
                        , nextUp = 20
                        }

                    else
                        { title = 48
                        , exercise = 64
                        , buttonMaxWidth = 250
                        , buttonIcon = 50
                        , nextUp = 35
                        }

                -- what elements to show
                dataGroup =
                    case List.Nonempty.head workoutData.blocksLeft of
                        TimeBlock.CountDown secsLeft totalTime ->
                            { currBlockElem = bigFont sizesData.title Colours.sky "Countdown"
                            , themeColour = Colours.sky
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        TimeBlock.ExerciseBreak secsLeft totalTime ->
                            { currBlockElem = bigFont sizesData.title Colours.grass "Break Between Exercises"
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        TimeBlock.SetBreak secsLeft totalTime ->
                            { currBlockElem = bigFont sizesData.title Colours.grass "Break Between Sets"
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        TimeBlock.Exercise { setName, name, duration, secsLeft } ->
                            { currBlockElem =
                                Element.column
                                    [ Element.centerX
                                    , Element.spacing 4
                                    , Font.center
                                    , Font.color Colours.sky
                                    ]
                                    [ bigFont sizesData.title Colours.sunflower setName
                                    , bigFont sizesData.exercise Colours.sunset name
                                    ]
                            , themeColour = Colours.sunset
                            , secsLeft = secsLeft
                            , totalTime = duration
                            }

                nextupString =
                    case List.head <| List.Nonempty.tail workoutData.blocksLeft of
                        Just (TimeBlock.ExerciseBreak _ _) ->
                            "Break"

                        Just (TimeBlock.SetBreak _ _) ->
                            "Break"

                        Just (TimeBlock.Exercise d) ->
                            d.name

                        _ ->
                            "Workout Completion"

                viewRemainingTime =
                    Element.el
                        [ Font.size <| sharedState.windowSize.height // 8
                        , Font.color dataGroup.themeColour
                        , Font.bold
                        ]
                    <|
                        Element.text (String.fromInt dataGroup.secsLeft)

                bottomElems =
                    Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spaceEvenly
                        ]
                        [ Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 1)
                            ]
                            Element.none
                        , -- play/pause toggle
                          Input.button
                            [ Element.width (Element.maximum sizesData.buttonMaxWidth Element.fill)
                            , Element.height Element.shrink
                            , Element.centerX
                            , Border.rounded <| sizesData.buttonMaxWidth // 10
                            , Background.color Colours.sky
                            ]
                            { onPress = Just TogglePlay
                            , label =
                                Element.el
                                    [ Element.centerX
                                    , Element.padding 8
                                    ]
                                <|
                                    Util.viewIcon
                                        { icon = playPauseIcon
                                        , color = Colours.white
                                        , size = sizesData.buttonIcon
                                        , msg = Nothing
                                        , withBorder = False
                                        }
                            }

                        -- next up
                        , Element.paragraph
                            [ Element.height Element.shrink
                            , Font.center
                            , Font.size sizesData.nextUp
                            , Font.light
                            , Font.color Colours.sky
                            ]
                            [ Element.text "Next up: "
                            , Element.text nextupString
                            ]

                        -- instagram-like timeline thing
                        , pebbles dataGroup.themeColour <|
                            if isSmall then
                                Nothing

                            else
                                Just
                                    { secsLeft = dataGroup.secsLeft
                                    , totalTime = dataGroup.totalTime
                                    }
                        , Element.el
                            [ Element.width Element.fill
                            , Element.height (Element.px 1)
                            ]
                            Element.none
                        ]
            in
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 16
                ]
                [ -- current exercise
                  Element.el
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 2
                    , Util.centerOverlay dataGroup.currBlockElem
                    ]
                    Element.none

                -- next exercise
                , Element.el
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 3
                    , Util.centerOverlay viewRemainingTime
                    ]
                    Element.none

                -- bottom pause bar and dots
                , Element.el
                    [ Element.width Element.fill
                    , Element.height <| Element.fillPortion 2
                    ]
                    bottomElems
                ]

        viewLandscape =
            let
                bigFont size color label =
                    Element.paragraph
                        [ Element.centerX
                        , Font.size size
                        , Font.center
                        , Font.color color
                        ]
                        [ Element.text label ]

                timerText secsLeft color =
                    Element.el
                        [ Element.centerX
                        , Font.color color
                        , Font.size 125
                        ]
                    <|
                        Element.text <|
                            String.fromInt secsLeft

                dataGroup =
                    case List.Nonempty.head workoutData.blocksLeft of
                        TimeBlock.CountDown secsLeft totalTime ->
                            { currExercise = bigFont 54 Colours.sky "Countdown"
                            , timerText = timerText secsLeft Colours.sky
                            , themeColour = Colours.sky
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        TimeBlock.ExerciseBreak secsLeft totalTime ->
                            { currExercise = bigFont 54 Colours.grass "Break Between Exercise"
                            , timerText = timerText secsLeft Colours.grass
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        TimeBlock.SetBreak secsLeft totalTime ->
                            { currExercise = bigFont 54 Colours.grass "Break Between Sets"
                            , timerText = timerText secsLeft Colours.grass
                            , themeColour = Colours.grass
                            , secsLeft = secsLeft
                            , totalTime = totalTime
                            }

                        TimeBlock.Exercise { setName, name, duration, secsLeft } ->
                            { currExercise =
                                Element.column
                                    [ Element.centerX
                                    , Element.spacing 16
                                    , Font.center
                                    , Font.color Colours.sky
                                    ]
                                    [ bigFont 32 Colours.sunflower setName
                                    , bigFont 54 Colours.sunset name
                                    ]
                            , timerText = timerText secsLeft Colours.sunset
                            , themeColour = Colours.sunset
                            , secsLeft = secsLeft
                            , totalTime = duration
                            }

                nextupString =
                    case List.head <| List.Nonempty.tail workoutData.blocksLeft of
                        Just (TimeBlock.ExerciseBreak _ _) ->
                            "Break"

                        Just (TimeBlock.SetBreak _ _) ->
                            "Break"

                        Just (TimeBlock.Exercise d) ->
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
            in
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 16
                , Element.behindContent <|
                    Element.el
                        [ Element.alignBottom
                        , Element.width Element.fill
                        , Element.paddingXY 0 16
                        ]
                        (pebbles dataGroup.themeColour
                            (Just
                                { secsLeft = dataGroup.secsLeft
                                , totalTime = dataGroup.totalTime
                                }
                            )
                        )
                ]
                [ Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    [ Element.column
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Element.spaceEvenly
                        ]
                        [ Element.el [ Element.height (Element.px 0) ] Element.none
                        , dataGroup.currExercise
                        , Element.el [ Element.centerX ] nextup
                        , Element.el [ Element.height (Element.px 0) ] Element.none
                        ]
                    , Element.el
                        [ Element.centerY
                        , Element.width Element.shrink
                        ]
                      <|
                        Util.viewIcon
                            { icon = playPauseIcon
                            , color = dataGroup.themeColour
                            , size = 75
                            , msg = Just TogglePlay
                            , withBorder = True
                            }
                    , Element.el
                        [ Element.width Element.fill
                        , Element.height Element.fill
                        , Util.centerOverlay dataGroup.timerText
                        ]
                        Element.none
                    ]
                ]
    in
    case sharedState.device.orientation of
        Element.Portrait ->
            viewPortrait <| Util.isVerticalPhone sharedState.device

        Element.Landscape ->
            viewLandscape


viewFinished : Element Msg
viewFinished =
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
            , withBorder = False
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
        , Util.textButton
            { msg = NavigateTo True Routes.Config
            , color = Colours.sunflower
            , text = "Back"
            }
        ]


viewNeverStarted : Images -> AppModel -> Element Msg
viewNeverStarted img model =
    Element.column
        [ Element.width Element.fill
        , Element.centerY
        , Element.spacing 32
        , Element.padding 8
        ]
        [ Element.image
            [ Element.width (Element.px 125)
            , Element.centerX
            ]
            { src = img.smhSrc
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
            [ Element.spacing 4
            , Element.width Element.fill
            , Font.light
            , Font.center
            , Font.color Colours.sunset
            ]
            [ Element.paragraph
                [ Element.width Element.shrink ]
                [ Element.text "You didn't put anything in your workout!" ]
            , Util.textButton
                { msg = NavigateTo True Routes.Config
                , color = Colours.sunflower
                , text = "Go Back to Settings"
                }
            ]
        ]



---- UPDATE ----


type Msg
    = NavigateTo Bool Route
    | StartExercise Data.WorkoutData
    | NextSecond
    | TogglePlay
    | KeyMsg Keyboard.Msg -- so we can react upon the space key press


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate Msg )
update sharedState msg (Model model data) =
    case msg of
        NavigateTo alwaysOn route ->
            ( Model model data
            , Cmd.batch
                [ SharedState.navigateTo route sharedState
                , if alwaysOn then
                    Ports.workoutStatus "end"

                  else
                    Cmd.none
                ]
            , SharedState.NoUpdate
            )

        StartExercise workoutData ->
            ( Model model
                { data
                    | state = Data.InProgress workoutData
                    , playing = True
                }
            , playSounds data
                { sound = Ports.playSound "whistle"
                , speak = Ports.speak "Workout Started"
                }
            , SharedState.NoUpdate
            )

        NextSecond ->
            case data.state of
                Data.InProgress workoutData ->
                    let
                        ( block, tl ) =
                            ( List.Nonempty.head workoutData.blocksLeft, List.Nonempty.tail workoutData.blocksLeft )
                    in
                    case TimeBlock.decreaseSecond block of
                        -- exercise block is complete
                        Nothing ->
                            case tl of
                                -- no more exercises. Workout complete!
                                [] ->
                                    ( Model model { data | state = Data.Finished }
                                    , playSounds data
                                        { sound = Ports.playSound "tada"
                                        , speak = Ports.speak "Congratulations! Workout complete."
                                        }
                                    , SharedState.NoUpdate
                                    )

                                bl :: xs ->
                                    let
                                        toSpokenString =
                                            case bl of
                                                TimeBlock.CountDown _ _ ->
                                                    -- technically this shouldn't happen
                                                    "Exercise Started"

                                                TimeBlock.Exercise ex ->
                                                    ex.name

                                                _ ->
                                                    -- also says next exercise
                                                    "Take a break. " ++ blockAfterString

                                        -- the block AFTER the current current block. Used when it's a break so we can tell the next exercise
                                        blockAfterString =
                                            case xs of
                                                (TimeBlock.Exercise ex) :: _ ->
                                                    "Next exercise: " ++ ex.name

                                                -- screw it don't say anything for any other occurence. Should any other occurance even happen??
                                                _ ->
                                                    ""
                                    in
                                    ( Model model
                                        { data | state = Data.InProgress { workoutData | blocksLeft = Nonempty bl xs } }
                                    , playSounds data
                                        { sound = Ports.playSound "whistle"
                                        , speak = Ports.speak toSpokenString
                                        }
                                    , SharedState.NoUpdate
                                    )

                        Just newBlock ->
                            let
                                timeleft =
                                    TimeBlock.timeLeft newBlock

                                cmd =
                                    if timeleft <= 3 then
                                        playSounds data
                                            { sound = Ports.playSound "tick"
                                            , speak = Ports.speak <| String.fromInt timeleft
                                            }

                                    else
                                        Cmd.none
                            in
                            ( Model model
                                { data | state = Data.InProgress { workoutData | blocksLeft = Nonempty newBlock tl } }
                            , cmd
                            , SharedState.NoUpdate
                            )

                _ ->
                    -- ignore
                    ( Model model data, Cmd.none, SharedState.NoUpdate )

        TogglePlay ->
            ( Model model { data | playing = not data.playing }
            , if data.playing then
                playSounds data
                    { sound = Cmd.none
                    , speak = Ports.speak "Paused"
                    }

              else
                playSounds data
                    { sound = Ports.playSound "whistle"
                    , speak = Ports.speak "Resumed"
                    }
            , SharedState.NoUpdate
            )

        KeyMsg keyMsg ->
            let
                newKeys =
                    Keyboard.update keyMsg model.keys

                newModel =
                    { model | keys = newKeys }
            in
            if newKeys == [ Keyboard.Spacebar ] then
                update sharedState TogglePlay (Model newModel data)

            else
                ( Model model data, Cmd.none, SharedState.NoUpdate )


playSounds : Data -> { sound : Cmd Msg, speak : Cmd Msg } -> Cmd Msg
playSounds data { sound, speak } =
    let
        soundMsg =
            if data.sounds then
                sound

            else
                Cmd.none

        speakMsg =
            if data.speak then
                speak

            else
                Cmd.none
    in
    Cmd.batch
        [ soundMsg, speakMsg ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions (Model _ data) =
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
        , tickSub
        ]

module Modules.Config exposing (Config, Msg, encode, getData, init, update, view)

import Colours
import Data.Config
import Data.Duration as Duration exposing (Duration)
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import FeatherIcons as Icon
import Json.Decode
import Json.Encode
import Modules.Exercise as Exercise
import Modules.Set as Set
import Modules.TimeInput as TimeInput
import Util


type Config
    = Config Data.Config.Data



-- INIT AND JSON STUFF


init : Json.Encode.Value -> Config
init localStorageValue =
    let
        decodeLocalStorageAttempt =
            Data.Config.decodeLocalStorage localStorageValue

        ( actualData, mErr ) =
            case decodeLocalStorageAttempt of
                -- there was no config stored in the first place
                Ok Nothing ->
                    ( Data.Config.default, Nothing )

                -- success
                Ok (Just config) ->
                    ( Data.Config.fromLocalStorage config, Nothing )

                -- failure to decode
                Err jsonErr ->
                    ( Data.Config.default, Just <| Json.Decode.errorToString jsonErr )
    in
    Config { actualData | error = mErr }



-- helpers


encode : Config -> Json.Encode.Value
encode (Config data) =
    Data.Config.encode data


getData : Config -> Data.Config.Data
getData (Config data) =
    data



-- internal helpers


totalTime : Data.Config.Data -> Duration
totalTime data =
    Dict.toList data.set
        |> List.map Tuple.second
        |> List.map
            (Set.totalTime
                { exerciseDuration = TimeInput.getDuration data.exerciseInput
                , breakDuration = TimeInput.getDuration data.breakInput
                }
            )
        |> List.intersperse (TimeInput.getDuration data.setBreakInput)
        |> List.foldl Duration.add (Duration.init 0)



-- VIEW


view : Config -> Element Msg
view (Config data) =
    Element.column
        [ Element.width (Element.fill |> Element.maximum 1000)
        , Element.centerX
        , Element.height Element.fill
        , Element.padding 32
        , Element.spacing 48
        ]
        [ Util.viewIcon
            { icon = Icon.settings
            , color = Colours.sky
            , size = 50
            , msg = Nothing
            }
            |> Element.el [ Element.centerX ]
        , data.error
            |> Maybe.map Element.text
            |> Maybe.withDefault Element.none

        -- actual settings stuff
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 8
            ]
            [ Element.column
                [ Element.centerX
                , Element.spacing 64
                ]
                [ Element.el
                    [ Element.onRight <|
                        Element.el
                            [ Element.centerY ]
                        <|
                            TimeInput.view
                                { updateInput = UpdateInput Exercise
                                , updateFocus = UpdateFocus Exercise
                                , displayText = Nothing
                                }
                                data.exerciseInput
                    , Element.text "Exercise Duration:"
                        |> Element.el [ Element.centerY ]
                        |> Element.el
                            [ Font.light
                            , Element.height (Element.px 50)
                            , Element.centerY
                            ]
                        |> Element.onLeft
                    , Element.centerX
                    ]
                    Element.none
                , Element.el
                    [ Element.onRight <|
                        Element.el
                            [ Element.centerY ]
                        <|
                            TimeInput.view
                                { updateInput = UpdateInput Break
                                , updateFocus = UpdateFocus Break
                                , displayText = Nothing
                                }
                                data.breakInput
                    , Element.text "Break Between Exercises:"
                        |> Element.el [ Element.centerY ]
                        |> Element.el
                            [ Font.light
                            , Element.height (Element.px 50)
                            , Element.centerY
                            ]
                        |> Element.onLeft
                    , Element.centerX
                    ]
                    Element.none
                , Element.el
                    [ Element.onRight <|
                        Element.el
                            [ Element.centerY ]
                        <|
                            TimeInput.view
                                { updateInput = UpdateInput SetBreak
                                , updateFocus = UpdateFocus SetBreak
                                , displayText = Nothing
                                }
                                data.setBreakInput
                    , Element.text "Break Between Sets:"
                        |> Element.el [ Element.centerY ]
                        |> Element.el
                            [ Font.light
                            , Element.height (Element.px 50)
                            , Element.centerY
                            ]
                        |> Element.onLeft
                    , Element.centerX
                    ]
                    Element.none

                -- countdown
                , Element.row
                    [ Element.spacing 8
                    , Element.centerX
                    ]
                    [ Input.checkbox
                        [ Font.light
                        , Element.padding 4
                        ]
                        { onChange = ToggleCountdown
                        , icon =
                            \on ->
                                if on then
                                    Util.viewIcon
                                        { icon = Icon.checkSquare
                                        , color = Colours.grass
                                        , size = 30
                                        , msg = Nothing
                                        }

                                else
                                    Util.viewIcon
                                        { icon = Icon.xSquare
                                        , color = Colours.sunset
                                        , size = 30
                                        , msg = Nothing
                                        }
                        , checked = data.countdown
                        , label = Input.labelLeft [ Element.padding 8, Element.centerY ] <| Element.text "Countdown:"
                        }
                        |> Element.el
                            [ Element.centerX ]
                    , if data.countdown then
                        data.countdownInput
                            |> TimeInput.view
                                { updateInput = UpdateInput Countdown
                                , updateFocus = UpdateFocus Countdown
                                , displayText = Nothing
                                }

                      else
                        Element.el
                            [ Element.width <| Element.px 188
                            , Element.padding 4
                            ]
                            Element.none
                    ]
                ]
            ]

        -- set stuff
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 18
            ]
            [ -- total time
              Element.row
                [ Element.spacing 2
                , Element.centerX
                , Font.light
                ]
                [ Element.text "Total time: "
                , totalTime data
                    |> Duration.viewFancy
                    |> Element.el
                        [ Font.color Colours.sunflower ]
                , Element.text ", with "
                , Dict.size data.set
                    - 1
                    |> String.fromInt
                    |> Element.text
                    |> Element.el [ Font.color Colours.sunflower ]
                , Element.el [ Font.color Colours.sunflower ] <|
                    Element.text
                        (if Dict.size data.set == 2 then
                            " break."

                         else
                            " breaks."
                        )
                ]
            , Dict.toList
                data.set
                |> List.map
                    (\( _, set ) ->
                        Set.view
                            { onNewExercise = NewElement

                            -- exercise position then set position
                            , onDeleteExercise = DeleteElement
                            , onDelete = DeleteSet
                            , onUpdateRepeat = NewSetRepeat
                            , toggleExpand = ToggleSetExpand
                            , updateName = UpdateSetName
                            , updateExerciseName = UpdateExerciseName
                            , exerciseDuration = TimeInput.getDuration data.exerciseInput
                            , breakDuration = TimeInput.getDuration data.breakInput
                            }
                            set
                    )
                |> List.intersperse (Exercise.breakView <| TimeInput.getDuration data.setBreakInput)
                |> Element.column
                    [ Element.spacing 32
                    , Element.width Element.fill
                    ]

            -- add set
            , Element.el
                [ Element.alignBottom
                , Element.centerX
                ]
                (Util.viewIcon
                    { icon = Icon.plus
                    , color = Colours.sunflower
                    , size = 40
                    , msg = Just AddSet
                    }
                    |> Element.el
                        [ Element.alignBottom
                        , Element.centerX
                        , Background.color Colours.white
                        ]
                )
            ]
        ]



-- UPDATE
-- either the exercise, break, countdown or setBreak


type Input
    = Exercise
    | Break
    | SetBreak
    | Countdown


type Msg
    = UpdateInput Input String
    | UpdateFocus Input Bool
    | NewElement Int
    | DeleteElement Int Int
    | NewSetRepeat Int Int
    | DeleteSet Int
    | AddSet
    | ToggleSetExpand Int
    | UpdateSetName Int String
    | UpdateExerciseName Int Int String
    | ToggleCountdown Bool


update : Msg -> Config -> Config
update msg (Config data) =
    case msg of
        UpdateInput Exercise newVal ->
            Config { data | exerciseInput = TimeInput.updateInput data.exerciseInput newVal }

        UpdateFocus Exercise isFocused ->
            Config { data | exerciseInput = TimeInput.updateFocus data.exerciseInput isFocused }

        UpdateInput Break newVal ->
            Config { data | breakInput = TimeInput.updateInput data.breakInput newVal }

        UpdateFocus Break isFocused ->
            Config { data | breakInput = TimeInput.updateFocus data.breakInput isFocused }

        UpdateInput SetBreak newVal ->
            Config { data | setBreakInput = TimeInput.updateInput data.setBreakInput newVal }

        UpdateFocus SetBreak isFocused ->
            Config { data | setBreakInput = TimeInput.updateFocus data.setBreakInput isFocused }

        UpdateInput Countdown newVal ->
            Config { data | countdownInput = TimeInput.updateInput data.countdownInput newVal }

        UpdateFocus Countdown isFocused ->
            Config { data | countdownInput = TimeInput.updateFocus data.countdownInput isFocused }

        ToggleCountdown bool ->
            Config { data | countdown = bool }

        NewElement setPos ->
            Config
                { data
                    | set =
                        Dict.update
                            setPos
                            (Maybe.map Set.newExercise)
                            data.set
                }

        DeleteElement setPos elemPos ->
            Config
                { data
                    | set =
                        Dict.update
                            setPos
                            (Maybe.map <| Set.deleteExercise elemPos)
                            data.set
                }

        NewSetRepeat setPos repeat ->
            Config
                { data
                    | set =
                        Dict.update
                            setPos
                            (Maybe.map <| Set.updateRepeat repeat)
                            data.set
                }

        DeleteSet setPos ->
            Config { data | set = Dict.remove setPos data.set }

        AddSet ->
            let
                newN =
                    data.setCounter + 1
            in
            Config
                { data
                    | set = Dict.insert newN (Set.init newN) data.set
                    , setCounter = newN
                }

        ToggleSetExpand setPos ->
            Config { data | set = Dict.update setPos (Maybe.map Set.toggleExpand) data.set }

        UpdateSetName setPos newName ->
            Config { data | set = Dict.update setPos (Maybe.map <| Set.updateName newName) data.set }

        UpdateExerciseName setPos exercisePos newName ->
            Config { data | set = Dict.update setPos (Maybe.map <| Set.updateExerciseName exercisePos newName) data.set }

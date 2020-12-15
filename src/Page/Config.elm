module Page.Config exposing
    ( Model
    , Msg
    , getData
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Events
import Colours
import Data.Config as Data exposing (Data)
import Data.Duration as Duration
import Data.Flags as Flags exposing (Flags)
import Data.PopupCmd as PopupCmd
import Data.SharedState as SharedState exposing (SharedState, SharedStateUpdate)
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import FeatherIcons as Icon
import File.Download
import Html.Attributes
import Json.Decode
import Json.Encode
import Modules.Exercise as Exercise
import Modules.Popup as Popup
import Modules.Set as Set
import Modules.TimeInput as TimeInput
import Ports
import Routes exposing (Route)
import Util



---- TYPE ----


type Model
    = Model AppModel Data



-- stuff purely in the app


type alias AppModel =
    { saving : Bool }



-- INIT AND JSON STUFF


init : SharedState -> ( Model, Cmd Msg )
init ({ configCache } as sharedState) =
    ( Model
        { saving = False
        }
        configCache
    , Cmd.none
    )



-- Public Helpers


getData : Model -> Data.Data
getData (Model _ data) =
    data



---- VIEW ----


view : SharedState -> Model -> Element Msg
view sharedState (Model model data) =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.spacing 32
        , Element.paddingXY 0 16
        ]
        [ content sharedState (Model model data)
        , Element.row
            [ Element.spacing 16
            , Element.centerX
            ]
            [ -- save settings; go to applications as well as save to localhostUtil.viewIcon
              Util.viewIcon
                { icon = Icon.check
                , color = Colours.grass
                , size = 50
                , msg = Just <| NavigateTo True Routes.Workout
                , withBorder = True
                }
                |> Util.withTooltip
                    { position = Util.Top
                    , content = "Finish editing"
                    }

            -- download file as JSON
            , Util.viewIcon
                { icon = Icon.share
                , color = Colours.sky
                , size = 50
                , msg = Just ExportConfig
                , withBorder = True
                }
                |> Util.withTooltip
                    { position = Util.Top
                    , content = "Export Data"
                    }
            ]
        , Element.paragraph
            [ Font.size 16
            , Font.center
            , Font.light
            , Font.color Colours.lightGray
            ]
            [ Element.text "Version "
            , Element.text sharedState.version
            ]
        ]


content : SharedState -> Model -> Element Msg
content sharedState (Model model data) =
    let
        paddingX =
            case sharedState.device.class of
                Element.Phone ->
                    4

                _ ->
                    32
    in
    Element.column
        [ Element.width (Element.fill |> Element.maximum 1000)
        , Element.centerX
        , Element.height Element.fill
        , Element.paddingXY paddingX 32
        , Element.spacing 52
        ]
        [ Util.viewIcon
            { icon = Icon.settings
            , color = Colours.sky
            , size = 50
            , msg = Nothing
            , withBorder = False
            }
            |> Element.el [ Element.centerX ]
        , durations sharedState.device data
        , viewSounds data
        , viewSets sharedState.device data

        -- If the settings is saved or not
        , Element.el
            [ Element.centerX ]
          <|
            if model.saving then
                Element.paragraph
                    [ Font.light
                    , Font.color Colours.sunflower
                    ]
                    [ Element.text "saving..." ]

            else
                Element.paragraph
                    [ Font.light
                    , Font.color Colours.grass
                    ]
                    [ Element.text "All Changes Saved to Local Storage " ]
        ]


durations : Element.Device -> Data.Data -> Element Msg
durations device data =
    Element.column
        [ Element.centerX
        , Element.spacing 12
        ]
        [ Util.viewIcon
            { icon = Icon.clock
            , color = Colours.sunset
            , size = 40
            , msg = Nothing
            , withBorder = False
            }
            |> Element.el [ Element.centerX ]
        , TimeInput.view
            { displayText = Just "Exercise Duration:"
            , device = device
            }
            data.exerciseInput
            |> Element.map (UpdateTimeInput ExerciseIpt)
        , TimeInput.view
            { displayText = Just "Break Between Exercises:"
            , device = device
            }
            data.breakInput
            |> Element.map (UpdateTimeInput BreakIpt)
        , TimeInput.view
            { displayText = Just "Break Between Sets:"
            , device = device
            }
            data.setBreakInput
            |> Element.map (UpdateTimeInput SetBreakIpt)

        -- countdown
        , let
            countdownToggle =
                Element.el
                    [ Element.centerY
                    , Element.centerX
                    ]
                <|
                    checkbox
                        { onChange = ToggleCheckbox CountdownCbx
                        , checked = data.countdown
                        , label = Element.text "Countdown:"
                        }

            countdownInput =
                if data.countdown then
                    data.countdownInput
                        |> TimeInput.view
                            { displayText = Nothing
                            , device = device
                            }
                        |> Element.map (UpdateTimeInput CountdownIpt)

                else
                    Element.el
                        [ Element.width <| Element.px 188
                        , Element.padding 4
                        ]
                        Element.none
          in
          if Util.isVerticalPhone device then
            -- orient vertically
            Element.column
                [ Element.spacing 8
                , Element.centerX
                ]
                [ countdownToggle
                , countdownInput
                ]

          else
            -- orient horizontally (center the space between label and input)
            Element.el
                [ Element.onRight <|
                    Element.el [ Element.centerY ] countdownInput
                , Element.onLeft countdownToggle

                -- HACK (to fix elm-ui bug #243)
                , Element.moveDown 0
                , Element.centerX
                , Element.height (Element.px 64)
                , Element.padding 4
                ]
                Element.none
        ]


viewSounds : Data.Data -> Element Msg
viewSounds data =
    let
        helpIcon =
            Util.viewIcon
                { icon = Icon.helpCircle
                , color = Colours.black
                , size = 20
                , msg = Just OpenSpeechSynthesis
                , withBorder = False
                }
                |> Element.el
                    [ Element.pointer
                    ]

        speechSynthCheckbox =
            checkbox
                { onChange = ToggleCheckbox SpeakCbx
                , checked = data.speak
                , label =
                    Element.row
                        [ Element.spacing 4 ]
                        [ Element.text "Speech Synthesis:"
                        , helpIcon
                        ]
                }
                |> Element.el [ Element.centerX ]

        defaultSoundsCheckbox =
            checkbox
                { onChange = ToggleCheckbox SoundsCbx
                , checked = data.sounds
                , label = Element.text "Default Sounds:"
                }
                |> Element.el [ Element.centerX ]

        toggles =
            Element.column
                [ Element.spacing 8
                , Element.centerX
                ]
                [ speechSynthCheckbox
                , defaultSoundsCheckbox
                ]
    in
    Element.column
        [ Element.centerX
        , Element.spacing 12
        ]
        [ Util.viewIcon
            { icon = Icon.volume2
            , color = Colours.sky
            , size = 40
            , msg = Nothing
            , withBorder = False
            }
            |> Element.el [ Element.centerX ]
        , toggles
        , if not data.sounds && not data.speak then
            Element.paragraph
                [ Font.light
                ]
                [ Element.el [ Font.bold, Font.color Colours.sunset ] <| Element.text "Warning: "
                , Element.text "Your workout will be completely silent."
                ]

          else
            Element.none
        ]


viewSets : Element.Device -> Data.Data -> Element Msg
viewSets device data =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 18
        ]
        [ Util.viewIcon
            { icon = Icon.zap
            , color = Colours.sunflower
            , size = 40
            , msg = Nothing
            , withBorder = False
            }
            |> Element.el [ Element.centerX ]
        , -- total time
          Element.row
            [ Element.spacing 2
            , Element.centerX
            , Font.light
            ]
            [ Element.text "Total time: "
            , Data.totalTime data
                |> Duration.viewFancy
                |> Element.el
                    [ Font.color Colours.sunflower ]
            ]
        , Dict.toList
            data.sets
            |> List.map
                (\( _, set ) ->
                    Lazy.lazy2
                        Set.view
                        { onNewExercise = NewElement

                        -- exercise position then set position
                        , onDeleteExercise = DeleteElement
                        , onDelete = DeleteSet
                        , onUpdateRepeat = NewSetRepeat
                        , sanitizeRepeat = SanitizeRepeat
                        , onCopy = CopySet
                        , toggleExpand = ToggleSetExpand
                        , updateName = UpdateSetName
                        , updateExerciseName = UpdateExerciseName
                        , exerciseDuration = TimeInput.getDuration data.exerciseInput
                        , breakDuration = TimeInput.getDuration data.breakInput
                        , device = device
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
                , withBorder = True
                }
                |> Element.el
                    [ Element.alignBottom
                    , Element.centerX
                    , Background.color Colours.white
                    ]
            )
        ]



-- the popup when the user clicks the (?) on the speech synthesis toggle


speechSynthesisPopup : AppModel -> Popup.Config Msg
speechSynthesisPopup model =
    let
        body =
            Element.textColumn
                [ Element.spacing 16
                , Element.width Element.fill
                , Font.light
                , Font.size 20
                ]
                [ Element.paragraph
                    [ Font.bold
                    , Font.size 32
                    ]
                    [ Element.text "Speech Synthesis" ]
                , Element.paragraph []
                    [ Element.text "An experimental technology that would voice out the exercises and breaks as you work out." ]
                , Element.paragraph
                    []
                    [ Element.el [ Font.bold ] <| Element.text "NOTE: "
                    , Element.text "This will not work on Internet Explorer. "
                    ]
                ]

        closeButton =
            Util.viewIcon
                { icon = Icon.x
                , color = Colours.sunset
                , size = 30
                , msg = Just ClosePopup
                , withBorder = True
                }
                |> Element.el
                    [ Element.centerX ]
    in
    { closeMessage = Nothing
    , maskAttributes = [ Background.color Colours.transparent ]
    , containerAttributes =
        [ Element.spacing 32
        , Element.centerX
        , Element.centerY
        , Element.width (Element.maximum 600 Element.fill)
        , Element.padding 32
        , Background.color Colours.white
        , Border.width 1
        , Border.color Colours.sky
        , Border.rounded 8
        ]
    , headerAttributes = []
    , bodyAttributes = [ Element.width Element.fill ]
    , footerAttributes = [ Element.width Element.fill ]
    , header = Nothing
    , body = Just body
    , footer = Just closeButton
    }



-- my own checkmark


checkbox : { onChange : Bool -> Msg, checked : Bool, label : Element Msg } -> Element Msg
checkbox data =
    Input.checkbox
        [ Font.light
        , Element.padding 4
        ]
        { onChange = data.onChange
        , icon =
            \on ->
                if on then
                    Util.viewIcon
                        { icon = Icon.checkSquare
                        , color = Colours.grass
                        , size = 30
                        , msg = Nothing
                        , withBorder = False
                        }

                else
                    Util.viewIcon
                        { icon = Icon.xSquare
                        , color = Colours.sunset
                        , size = 30
                        , msg = Nothing
                        , withBorder = False
                        }
        , checked = data.checked
        , label =
            Input.labelLeft
                [ Element.padding 8
                , Element.centerY
                , Element.htmlAttribute <| Html.Attributes.class "no-select"
                ]
                data.label
        }



---- UPDATE ----


type Msg
    = NavigateTo Bool Route -- bool is whether or not we should start the always on screen
    | ClosePopup
    | UpdateTimeInput Input TimeInput.Msg
    | ToggleCheckbox Checkbox Bool
    | NewElement Int
    | DeleteElement Int Int
    | NewSetRepeat Int String
    | SanitizeRepeat Int
    | DeleteSet Int
    | AddSet
    | CopySet Int
    | ToggleSetExpand Int
    | UpdateSetName Int String
    | UpdateExerciseName Int Int String
    | OpenSpeechSynthesis
    | ToLocalStorage -- save to local storage
    | StoreConfigSuccess -- when local storage succeeds
    | ExportConfig



-- helps me differentiate between the different config stuff


type Input
    = ExerciseIpt
    | BreakIpt
    | SetBreakIpt
    | CountdownIpt


type Checkbox
    = CountdownCbx
    | SpeakCbx
    | SoundsCbx


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate Msg )
update sharedState msg (Model model data) =
    case msg of
        NavigateTo alwaysOn route ->
            ( Model model data
            , Cmd.batch
                [ SharedState.navigateTo route sharedState
                , if alwaysOn then
                    Ports.workoutStatus "start"

                  else
                    Cmd.none
                ]
            , SharedState.NoUpdate
            )

        ClosePopup ->
            ( Model model data, Cmd.none, SharedState.PopupCmd PopupCmd.remove )

        UpdateTimeInput ExerciseIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.exerciseInput
            in
            Model model { data | exerciseInput = newInput }
                |> (if save then
                        saveToLocalStorage sharedState

                    else
                        \c -> ( c, Cmd.none, SharedState.NoUpdate )
                   )

        UpdateTimeInput BreakIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.breakInput
            in
            Model model { data | breakInput = newInput }
                |> (if save then
                        saveToLocalStorage sharedState

                    else
                        \c -> ( c, Cmd.none, SharedState.NoUpdate )
                   )

        UpdateTimeInput SetBreakIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.setBreakInput
            in
            Model model { data | setBreakInput = newInput }
                |> (if save then
                        saveToLocalStorage sharedState

                    else
                        \c -> ( c, Cmd.none, SharedState.NoUpdate )
                   )

        UpdateTimeInput CountdownIpt timeInputMsg ->
            let
                ( newInput, save ) =
                    TimeInput.update timeInputMsg data.countdownInput
            in
            Model model { data | countdownInput = newInput }
                |> (if save then
                        saveToLocalStorage sharedState

                    else
                        \c -> ( c, Cmd.none, SharedState.NoUpdate )
                   )

        ToggleCheckbox CountdownCbx bool ->
            Model model { data | countdown = bool }
                |> saveToLocalStorage sharedState

        ToggleCheckbox SpeakCbx bool ->
            Model model { data | speak = bool }
                |> saveToLocalStorage sharedState

        ToggleCheckbox SoundsCbx bool ->
            Model model { data | sounds = bool }
                |> saveToLocalStorage sharedState

        NewElement setPos ->
            Model model (updateSetDictionary data setPos Set.newExercise)
                |> saveToLocalStorage sharedState

        DeleteElement setPos elemPos ->
            Model model (updateSetDictionary data setPos (Set.deleteExercise elemPos >> Set.sanitizeExercises))
                |> saveToLocalStorage sharedState

        NewSetRepeat setPos repeat ->
            ( Model model (updateSetDictionary data setPos <| Set.updateRepeatInput repeat)
            , Cmd.none
            , SharedState.NoUpdate
            )

        SanitizeRepeat setPos ->
            Model model (updateSetDictionary data setPos Set.sanitizeRepeat)
                |> saveToLocalStorage sharedState

        DeleteSet setPos ->
            Model model (Data.sanitizeSets { data | sets = Dict.remove setPos data.sets })
                |> saveToLocalStorage sharedState

        AddSet ->
            let
                newN =
                    data.setCounter + 1
            in
            Model model
                { data
                    | sets = Dict.insert newN (Set.init newN) data.sets
                    , setCounter = newN
                }
                |> saveToLocalStorage sharedState

        CopySet setPos ->
            case Dict.get setPos data.sets of
                Nothing ->
                    ( Model model data, Cmd.none, SharedState.NoUpdate )

                Just setToCopy ->
                    let
                        -- creating a new dict, shifting the keys of the element to make room for the duplicated element
                        newSets =
                            Dict.toList data.sets
                                |> List.map
                                    (\( n, set ) ->
                                        if n > setPos then
                                            ( n + 1, set )

                                        else
                                            ( n, set )
                                    )
                                |> Dict.fromList
                    in
                    Model model
                        { data
                            | sets = Dict.insert (setPos + 1) (Set.updatePosition (setPos + 1) setToCopy) newSets
                            , setCounter = Dict.size data.sets + 1
                        }
                        |> saveToLocalStorage sharedState

        ToggleSetExpand setPos ->
            ( Model model (updateSetDictionary data setPos Set.toggleExpand), Cmd.none, SharedState.NoUpdate )

        UpdateSetName setPos newName ->
            Model model (updateSetDictionary data setPos <| Set.updateName newName)
                |> saveToLocalStorage sharedState

        UpdateExerciseName setPos exercisePos newName ->
            Model model (updateSetDictionary data setPos <| Set.updateExerciseName exercisePos newName)
                |> saveToLocalStorage sharedState

        OpenSpeechSynthesis ->
            ( Model model data, Cmd.none, SharedState.PopupCmd <| PopupCmd.show (speechSynthesisPopup model) )

        ToLocalStorage ->
            ( Model { model | saving = True } data, Ports.storeConfig (Data.encode data), SharedState.NoUpdate )

        StoreConfigSuccess ->
            -- update config whenever we successfully store config
            ( Model { model | saving = False } data, Cmd.none, SharedState.UpdateConfigCache data )

        ExportConfig ->
            ( Model model data
            , Data.encode data
                -- prettify the JSON file
                |> Json.Encode.encode 4
                |> File.Download.string "workout.json" "application/json"
            , SharedState.NoUpdate
            )



-- helpers


updateSetDictionary : Data.Data -> Int -> (Set.Set -> Set.Set) -> Data.Data
updateSetDictionary data setPos f =
    { data
        | sets =
            Dict.update
                setPos
                (Maybe.map f)
                data.sets
    }


saveToLocalStorage : SharedState -> Model -> ( Model, Cmd Msg, SharedStateUpdate Msg )
saveToLocalStorage sharedState model =
    update sharedState ToLocalStorage model



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions (Model model _) =
    Ports.storeConfigSuccess <| always StoreConfigSuccess

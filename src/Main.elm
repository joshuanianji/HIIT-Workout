module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Colours
import Data.Config
import Data.Flags as Flags exposing (Flags, WindowSize)
import Data.PopupCmd as PopupCmd
import Data.SharedState as SharedState exposing (SharedState, SharedStateUpdate)
import Element exposing (Element, scrollbarY)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import FeatherIcons as Icon
import File.Download
import GithubLogo
import Html exposing (Html)
import Http
import Json.Encode
import Modules.Popup as Popup
import Page.Config as Config
import Page.NotFound as NotFound
import Page.Workout as Workout
import Routes exposing (Route)
import Url exposing (Url)
import Util



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = viewApplication
        , init = init
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



---- MODEL ----


type alias Model =
    { sharedState : SharedState
    , route : Route

    -- popup letting them know that you can install it as a native app
    , showIosInstall : Bool
    , iosShareIcon : String
    , popup : Maybe (Popup.Config Msg)

    -- when the local storage is saved, show the checkmark for 2 seconds
    , showSavedCheck : Bool
    , page : Page
    }


type Page
    = Config Config.Model
    | Workout Workout.Model
    | NotFound NotFound.Model


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        sharedState =
            SharedState.init flags key

        route =
            Routes.fromUrl url

        ( page, cmd ) =
            routeToPage route sharedState
    in
    ( { sharedState = sharedState
      , route = Routes.fromUrl url
      , showIosInstall = flags.showIosInstall
      , iosShareIcon = flags.images.iosShareIconSrc
      , popup = Nothing
      , showSavedCheck = False
      , page = page
      }
    , cmd
    )



---- VIEW ----


viewApplication : Model -> Browser.Document Msg
viewApplication model =
    { title = Routes.tabTitle model.route
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    let
        pageContent =
            case model.page of
                Config subModel ->
                    Config.view model.sharedState subModel
                        |> Element.map ConfigMsg

                Workout subModel ->
                    Workout.view model.sharedState subModel
                        |> Element.map WorkoutMsg

                NotFound subModel ->
                    NotFound.view model.sharedState subModel
                        |> Element.map NotFoundMsg

        content =
            Element.el
                [ Element.width Element.fill
                , Element.height <| Element.maximum model.sharedState.windowSize.height Element.fill
                , Element.scrollbarY
                ]
            <|
                Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.paddingXY 0 16
                    , Element.inFront <| Element.el [ Element.centerX, Element.padding 8 ] (iosInstallPopup model)
                    ]
                    pageContent
    in
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.behindContent content
        , Element.inFront <|
            Popup.view model.popup
        ]
        Element.none
        |> Element.layout
            [ Font.family
                [ Font.typeface "Lato" ]
            , Element.inFront (githubLogo model.sharedState.device)
            ]


iosInstallPopup : Model -> Element Msg
iosInstallPopup model =
    if model.showIosInstall then
        let
            fontSize =
                model.sharedState.windowSize.width
                    // 24
                    |> clamp 13 30
        in
        Element.row
            [ Element.padding 12
            , Element.spacing 8
            , Font.size fontSize
            , Font.light
            , Background.color Colours.white
            , Border.color Colours.sunflower
            , Border.rounded 15
            , Border.width 1
            , Border.shadow
                { offset = ( 0, 0 )
                , size = 2
                , blur = 4
                , color = Colours.withAlpha 0.4 Colours.lightGray
                }
            ]
            [ Element.textColumn
                [ Element.width Element.fill
                , Element.spacing 4
                , Element.paddingXY 8 0
                ]
                [ Element.paragraph
                    [ Element.width Element.fill ]
                    [ Element.text "Install this webapp on your iOS device! " ]
                , Element.paragraph
                    [ Element.width Element.fill ]
                    [ Element.text "In Safari, tap "
                    , Element.image
                        [ Element.height (Element.px fontSize)
                        , Element.paddingXY 2 0
                        ]
                        { src = model.iosShareIcon
                        , description = "iOS Share button"
                        }
                    , Element.text ", then 'Add to the homescreen.'"
                    ]
                ]
            , Element.el
                [ Element.height Element.fill
                , Element.width <| Element.px 1
                , Background.color Colours.lightGray
                ]
                Element.none
            , Util.viewIcon
                { icon = Icon.x
                , color = Colours.sunset
                , size = toFloat fontSize * 2
                , msg = Just RemoveIosInstallPopup
                , withBorder = False
                }
                |> Element.el
                    [ Element.width Element.shrink
                    , Element.centerY
                    ]
            ]

    else
        Element.none


githubLogo : Element.Device -> Element Msg
githubLogo device =
    if device.class == Element.Desktop || device.class == Element.BigDesktop then
        GithubLogo.view
            { href = "https://github.com/joshuanianji/HIIT-Timer"
            , bgColor = "#000"
            , bodyColor = "#fff"
            }
            |> Element.el
                [ Element.alignRight
                , Element.alignTop
                ]

    else
        Element.none



---- UPDATE ----


type Msg
    = ChangedUrl Url
    | ClickedLink UrlRequest
    | NewWindowSize Int Int
    | RemoveIosInstallPopup -- ios user clicks the 'x'
    | ConfigMsg Config.Msg
    | WorkoutMsg Workout.Msg
    | NotFoundMsg NotFound.Msg



-- really should use lenses


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.page, msg ) of
        ( _, ClickedLink urlRequest ) ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.sharedState.key <| Url.toString url )

                External url ->
                    ( model, Nav.load url )

        ( _, ChangedUrl url ) ->
            let
                route =
                    Routes.fromUrl url

                ( page, pageCmd ) =
                    routeToPage route model.sharedState
            in
            ( { model
                | page = page
                , route = route
              }
            , pageCmd
            )

        ( _, NewWindowSize width height ) ->
            ( { model | sharedState = SharedState.update (SharedState.NewWindowSize width height) model.sharedState }
            , Cmd.none
            )

        ( _, RemoveIosInstallPopup ) ->
            ( { model | showIosInstall = False }
            , Cmd.none
            )

        ( Config subModel, ConfigMsg subMsg ) ->
            Config.update model.sharedState subMsg subModel
                |> updateWith Config ConfigMsg model

        ( Workout subModel, WorkoutMsg subMsg ) ->
            Workout.update model.sharedState subMsg subModel
                |> updateWith Workout WorkoutMsg model

        ( NotFound subModel, NotFoundMsg subMsg ) ->
            NotFound.update model.sharedState subMsg subModel
                |> updateWith NotFound NotFoundMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg, SharedStateUpdate subMsg )
    -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subMsg, ssUpdate ) =
    let
        ( newModel, newCmd ) =
            case ssUpdate of
                SharedState.PopupCmd popupCmd ->
                    ( { model | popup = PopupCmd.getPopup <| PopupCmd.map toMsg popupCmd }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )
    in
    ( { newModel
        | page = toPage subModel
        , sharedState = SharedState.update ssUpdate model.sharedState
      }
    , Cmd.map toMsg subMsg
    )


routeToPage : Route -> SharedState -> ( Page, Cmd Msg )
routeToPage route sharedState =
    case route of
        Routes.Config ->
            Config.init sharedState
                |> initWithPage Config ConfigMsg

        Routes.Workout ->
            Workout.init sharedState
                |> initWithPage Workout WorkoutMsg

        Routes.NotFound ->
            NotFound.init
                |> initWithPage NotFound NotFoundMsg


initWithPage : (subModel -> Page) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Page, Cmd Msg )
initWithPage toPage toMsg ( subModel, subMsg ) =
    ( toPage subModel, Cmd.map toMsg subMsg )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        specifics =
            case model.page of
                Config subModel ->
                    Config.subscriptions subModel
                        |> Sub.map ConfigMsg

                Workout subModel ->
                    Workout.subscriptions subModel
                        |> Sub.map WorkoutMsg

                NotFound subModel ->
                    NotFound.subscriptions subModel
                        |> Sub.map NotFoundMsg

        newWindowSub =
            if Util.isVerticalPhone model.sharedState.device then
                -- vertical phones call a new window size event when the keyboard pops up as well, messing up the view function.
                Sub.none

            else
                Browser.Events.onResize NewWindowSize
    in
    Sub.batch
        [ specifics
        , newWindowSub
        ]

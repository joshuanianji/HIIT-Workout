module Page.NotFound exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

{-| Splash Screen is shown then firbase is getting the user information
-}

import Browser.Navigation as Nav
import Colours
import Data.SharedState as SharedState exposing (SharedState, SharedStateUpdate)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Routes exposing (Route)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



---- VIEW ----


view : SharedState -> Model -> Element Msg
view sharedState model =
    Element.column
        [ Element.centerX
        , Element.centerY
        , Element.spacing 32
        ]
        [ Element.paragraph
            [ Font.center
            , Font.light
            , Font.size 32
            ]
            [ Element.text "Page not found!" ]
        , Input.button
            [ Element.centerX
            , Font.light
            , Font.size 20
            , Font.bold
            , Element.mouseOver
                [ Font.color Colours.lightGray ]
            ]
            { onPress = Just (NavigateTo Routes.Config)
            , label = Element.text "Go Back to Config"
            }
        ]



---- UPDATE ----


type Msg
    = NavigateTo Route


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate Msg )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, SharedState.navigateTo route sharedState, SharedState.NoUpdate )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

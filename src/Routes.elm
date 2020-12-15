module Routes exposing (Route(..), fromUrl, navigateTo, tabTitle)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


type Route
    = Config
    | Workout
    | NotFound


urlParser : Parser (Route -> a) a
urlParser =
    Parser.oneOf
        [ Parser.map Config Parser.top
        , Parser.map Workout (Parser.s "workout")
        ]



---- PUBLIC ----


navigateTo : Nav.Key -> Route -> Cmd msg
navigateTo key route =
    Nav.pushUrl key (toUrlString route)


fromUrl : Url -> Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse urlParser
        |> Maybe.withDefault NotFound


tabTitle : Route -> String
tabTitle route =
    case route of
        Config ->
            "Config"

        Workout ->
            "Workout"

        NotFound ->
            "Not Found"



---- INTERNAL ----


toUrlString : Route -> String
toUrlString route =
    let
        pieces =
            case route of
                Config ->
                    [ ]

                Workout ->
                    [ "workout" ]

                NotFound ->
                    [ "not-found" ]
    in
    "#/" ++ String.join "/" pieces

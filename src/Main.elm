module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Random



-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

type alias Model = Maybe String

init : () -> (Model, Cmd Msg)
init _ =
    ( Just ""
    , Http.get
        { url = "../static/Words.txt"
        , expect = Http.expectString GotText
        }
    )

-- UPDATE

type Msg
    = GotText (Result Http.Error String)
    | RandomInt Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok body -> (Just body, Random.generate RandomInt (Random.int 0 100))
                Err _ -> (Nothing, Cmd.none)
        RandomInt n ->
            case model of
                Just body ->
                    case String.split " " body of
                        [] -> (Nothing, Cmd.none)
                        (x::xs) -> (List.head (List.drop n (x::xs)), Cmd.none)
                Nothing -> (Nothing, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Html Msg
view model =
    pre [] [ text (case model of
                        Just body -> body
                        Nothing -> "Erreur de chargement !"
                    )]

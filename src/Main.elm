module Main exposing (..)

import Browser
import Html exposing (Html, text, pre)
import Http
import Random

-- FUNCTIONS

type Msg
  = Roll
  | NewFace Int

RandomInt msg = 
    case msg of
        Roll ->
            ( model
            , Random.generate NewFace (Random.int 1 6)
            )
        NewFace newFace ->
            ( Model newFace
            , Cmd.none
        )

cutAt lst n =
    if n > 0
    then case lst of
        [] -> []
        (x :: xs) -> cutAt xs (n-1)
    else case lst of
        [] -> []
        (x :: xs) -> (x :: xs)

getAt lst n = 
    List.head (cutAt lst n) 












-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Model
  = Failure
  | Loading
  | Success String

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , Http.get
      { url = "../static/Words.txt"
      , expect = Http.expectString GotText
      }
  )

-- UPDATE

type Msg
  = GotText (Result Http.Error (getAt (String.split " " String) 3))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to load your book."

    Loading ->
      text "Loading..."

    Success fullText ->
      pre [] [ text fullText ]

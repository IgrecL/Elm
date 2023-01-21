module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline as JP
import Debug



-- UTILS

clean : String -> String
clean str = String.slice 1 (String.length str - 1) str






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
  | Success (Result Error Word)

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getRandomWord)



-- UPDATE

type Msg
  = MorePlease
  | GotWord (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getRandomWord)

    GotWord result ->
      case result of
        Ok def ->
          (Success (decodeString wordDecoder (clean def)), Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Words" ]
    , viewWord model
    ]

viewWord : Model -> Html Msg
viewWord model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random word for some reason. "
        , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success def ->
      div []
        [ button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
        , blockquote [] [ text def ]
        , p [ style "text-align" "right" ]
            [ text "— "
            , cite [] [ ]
            , text (" by ")
            ]
        ]

-- HTTP

getRandomWord : Cmd Msg
getRandomWord =
  Http.get
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/anywhere"
    , expect = Http.expectString GotWord
    }



-- JSON PARSING

type alias Definitions =
    { definition : String
    , synonyms : List String
    , antonyms : List String
    , example : String
    }

type alias Meanings =
    { partsOfSpeech : String
    , definitions : List Definitions
    }

type alias Phonetics =
    { text : String
    , audio : String
    }

type alias Word =
    { word : String
    , phonetic : String
    , phonetics : List Phonetics
    , meanings : List Meanings
    }

definitionsDecoder =
    succeed Definitions
        |> JP.required "definition" string
        |> JP.required "synonyms" (list string)
        |> JP.required "antonyms" (list string)
        |> JP.required "example" string

meaningsDecoder =
    succeed Meanings
        |> JP.required "partOfSpeech" string
        |> JP.required "definitions" (list definitionsDecoder)

phoneticsDecoder =
    succeed Phonetics
        |> JP.required "text" string
        |> JP.required "audio" string

wordDecoder =
    succeed Word
        |> JP.required "word" string
        |> JP.required "phonetic" string
        |> JP.required "phonetics" (list phoneticsDecoder)
        |> JP.required "meanings" (list meaningsDecoder)

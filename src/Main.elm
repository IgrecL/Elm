module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Random
import Json.Decode exposing (..)
import Json.Decode.Pipeline as JP




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

type alias Model = Maybe String

init : () -> (Model, Cmd Msg)
init _ =
    ( Just ""
    , Http.get
        { url = "../static/Words.txt"
        , expect = Http.expectString GotWord
        }
    )



-- UPDATE

type Msg
    = GotWord (Result Http.Error String)
    | RandomInt Int
    | GotDefinition (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        -- On récupère la liste de mots et on génère un n
        GotWord result ->
            case result of
                Ok wordList -> (Just wordList, Random.generate RandomInt (Random.int 0 998))
                Err _ -> (Nothing, Cmd.none)
        RandomInt n ->
            case model of
                Just wordList ->
                    case String.split " " wordList of
                        [] -> (Nothing, Cmd.none)
                        (x::xs) -> (List.head (List.drop n (x::xs)), getDefinition)
                Nothing -> (Nothing, Cmd.none)
        GotDefinition result ->
            case result of
                Ok def -> (Just defString, decodeString wordDecoder (clean def))
                Err _ -> (Nothing, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

view : Model -> Html Msg
view model =
    pre [] [ text (case model of
                        Just wordList -> wordList
                        Nothing -> "Erreur de chargement !"
                    )]



-- HTTP

getDefinition : Cmd Msg
getDefinition =
    case model of
        Just answer ->
            Http.get
                { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ answer
                , expect = Http.expectJson GotDefinition 
                }
        Nothing -> (Nothing, Cmd.none)



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

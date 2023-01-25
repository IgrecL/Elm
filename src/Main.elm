module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (..)
import Http
import Random
import Json.Decode exposing (..)
import Json.Decode.Pipeline as JP



-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL

init : () -> (Model, Cmd Msg)
init _ =
    ( Loading
    , Http.get
        { url = "../static/Words.txt"
        , expect = Http.expectString GotWord
        }
    )



-- UPDATE

type Msg
    = GotWord (Result Http.Error String)
    | RandomInt Int
    | GotDefinition (Result Http.Error (List Word))
    | NewGuess Word String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        -- On récupère la liste de mots et on génère un nombre entier aléatoire
        GotWord result ->
            case result of
                Ok wordList -> (GotList wordList, Random.generate RandomInt (Random.int 0 998))
                Err _ -> (Error "Failed to generate a random integer.", Cmd.none)

        -- On récupère l'élément à l'index du nombre aléatoire dans la liste de mots
        RandomInt n ->
            case model of
                GotList wordList ->
                    case String.split " " wordList of
                        [] -> (Error "The word list is empty.", Cmd.none)
                        (x::xs) -> case (List.head (List.drop n (x::xs))) of
                            Just answer -> (Loading, getDefinition answer)
                            Nothing -> (Error "Failed to pick a random word.", Cmd.none)
                Error err -> (Error err, Cmd.none)
                Loading -> (Loading, Cmd.none)
                Success _ _ -> (Error "", Cmd.none)

        -- On récupère le premier élément de la liste de mots parsés
        GotDefinition result ->
            case result of
                Ok defList -> case (List.head defList) of
                    Just def -> (Success def "", Cmd.none)
                    Nothing -> (Error "The definitions list is empty.", Cmd.none)
                Err _ -> (Error "Couldn't parse the Json.", Cmd.none)
        
        NewGuess word newGuess ->
            (Success word newGuess, Cmd.none)
  


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW

-- Renvoie les synonymes d'un mot s'il y en a
ifSyn : Definitions -> String
ifSyn def = case def.synonyms of
    [] -> ""
    (x::xs) -> "\n-> synonyms: " ++ (String.join ", " def.synonyms)

-- Renvoie les antonymes d'un mot s'il y en a
ifAnt : Definitions -> String
ifAnt def = case def.antonyms of
    [] -> ""
    (x::xs) -> "\n-> antonyms: " ++ (String.join ", " def.antonyms)

-- Renvoie une liste d'élément html pour une défition du mot
stringDefs : Word -> List (Html msg)
stringDefs word = case List.head word.meanings of
    Just meaning -> case List.head meaning.definitions of
        Just def -> List.intersperse (br [] [])
            (List.map text
                (String.lines (def.definition ++ ifSyn def ++ ifAnt def))
            )
        Nothing -> []
    Nothing -> []

type Model
  = Loading
  | Error String
  | GotList String
  | Success Word String

view model =
  case model of
    Loading ->
        text "Loading..." 
    Error err ->
      div []
        [ b [] [ text "ERROR" ]
        , div [] [ text err ]
        ]
    GotList _ ->
        text ""
    Success word guess ->
        if word.word /= guess then
            div []
            [ b [] [ text "・Definition 1:" ]
            , blockquote [] (stringDefs word)
            , p [ style "text-align" "right" ]
            [ cite [] []
            , text word.word
            , text " |"
            ]
            , input [ placeholder "Write your guess", Html.Attributes.value guess, onInput (NewGuess word) ] []
            ]
        else
            div []
            [ b [] [ text "Congratulations!" ]
            , div [] [ text ("The word was '" ++ guess ++ "'") ]
            , button [ ] [ text "Play again" ]
            ]


-- HTTP

getDefinition : String -> Cmd Msg
getDefinition answer = Http.get
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ answer
    , expect = Http.expectJson GotDefinition (list wordDecoder)
    }



-- JSON PARSING

type alias Definitions =
    { definition : String
    , synonyms : List String
    , antonyms : List String
    }

type alias Meanings =
    { partsOfSpeech : String
    , definitions : List Definitions
    }

type alias Phonetics =
    { audio : String
    }

type alias Word =
    { word : String
    , phonetics : List Phonetics
    , meanings : List Meanings
    }

definitionsDecoder =
    succeed Definitions
        |> JP.required "definition" string
        |> JP.required "synonyms" (list string)
        |> JP.required "antonyms" (list string)

meaningsDecoder =
    succeed Meanings
        |> JP.required "partOfSpeech" string
        |> JP.required "definitions" (list definitionsDecoder)

phoneticsDecoder =
    succeed Phonetics
        |> JP.required "audio" string

wordDecoder =
    succeed Word
        |> JP.required "word" string
        |> JP.required "phonetics" (list phoneticsDecoder)
        |> JP.required "meanings" (list meaningsDecoder)

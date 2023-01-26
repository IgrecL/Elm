module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
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
    = Again
    | GotWord (Result Http.Error String)
    | RandomInt Int
    | GotDefinition (Result Http.Error (List Word))
    | NewGuess Word String
    | ShowAnswer Word

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        -- On recharge la page
        Again -> (Loading, Navigation.reload)

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
                Answer _ -> (Error "", Cmd.none)

        -- On récupère le premier élément de la liste de mots parsés
        GotDefinition result ->
            case result of
                Ok defList -> case (List.head defList) of
                    Just def -> (Success def "", Cmd.none)
                    Nothing -> (Error "The definitions list is empty.", Cmd.none)
                Err _ -> (Error "Couldn't parse the Json.", Navigation.reload)
        
        -- On met à jour la valeur de guess
        NewGuess word guess ->
            (Success word guess, Cmd.none)
  
        --
        ShowAnswer word ->
            (Answer word, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- FONCTIONS POUR VIEW

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

-- Renvoie un élément html <li> pour la n-ième définition du mot
stringDef : Int -> Meanings -> Html msg
stringDef n meaning = case List.head (List.drop n meaning.definitions) of
    Just def -> li [] (List.intersperse (br [] [])
        (List.map text
            (String.lines (def.definition ++ ifSyn def ++ ifAnt def))
        ))
    Nothing -> li [] []

-- Renvoie une liste d'éléments html <li> qui correspondent chacun à une définition pour ce 'meaning'
definitionHtml : Int -> Int -> Word -> List (Html msg)
definitionHtml m n word = case List.head (List.drop m word.meanings) of
    Just meaning -> case List.drop n meaning.definitions of
        (x :: xs) -> (stringDef n meaning) :: (definitionHtml m (n+1) word)
        [] -> []
    Nothing -> []

-- Renvoie un élément html text qui contient la classe grammatical du mot pour ce 'meaning' 
partsOfSpeech : Int -> Word -> Html msg
partsOfSpeech m word = case List.head (List.drop m word.meanings) of
    Just meaning -> b [] [text meaning.partsOfSpeech]
    Nothing -> text ""

-- Renvoie une liste d'éléments html <li> correspondant chacun à un 'meaning' du sens (classe grammaticale + <ol> des définitions)
senseHtml : Int -> Word -> List (Html msg)
senseHtml m word = case List.drop m word.meanings of
    (x :: xs) -> (li [] [ partsOfSpeech m word , ol [] (definitionHtml m 0 word) ]) :: (senseHtml (m+1) word) 
    [] -> []



-- VIEW

type Model
  = Loading
  | Error String
  | GotList String
  | Success Word String
  | Answer Word

view model =
  case model of

    Loading ->
        div []
        [ h1 [ style "text-align" "center", style "font-size" "50px" ] [ text "Guess it!" ]
        , div [ style "text-align" "center", style "font-size" "20px" ] [text "Loading..."]
        ]

    Error err ->
      div []
        [ b [ style "text-align" "center" ] [ text "ERROR" ]
        , div [] [ text err ]
        ]

    GotList _ ->
        text ""
    
    -- Page affichée quand le joueur est en train de chercher le mot ou qu'il l'a trouvé
    Success word guess ->

        -- Le joueur cherche le mot
        if word.word /= guess then
            div []
            [ h1 [ style "text-align" "center", style "font-size" "50px" ] [ text "Guess it!" ]
            , div [ style "text-align" "center" ] [
                input
                    [ style "text-align" "center"
                    , style "font-size" "20px"
                    , style "width" "193px"
                    , placeholder "Write your guess"
                    , Html.Attributes.value guess, onInput (NewGuess word) 
                    ] []
                , br [] []
                , button
                    [ style "text-align" "center"
                    , style "font-size" "20px"
                    , style "margin-top" "3px"
                    , style "width" "200px"
                    , onClick (ShowAnswer word) 
                    ] [ text "Show answer" ]
                ]
            , ul [ style "margin-left" "50px", style "margin-right" "80px" ] (senseHtml 0 word)
            ]

        -- Le joueur a trouvé le mot et on lui demande s'il veut rejouer
        else
            div []
            [ h1 [ style "text-align" "center", style "font-size" "50px" ] [ text "Guess it!" ]
            , div [ style "text-align" "center" ]
                [ div [ style "font-size" "20px" ] [ text ("The answer was '" ++ word.word ++ "'") ]
                , button
                    [ style "text-align" "center"
                    , style "font-size" "20px"
                    , style "margin-top" "5px"
                    , style "width" "200px"
                    , onClick Again 
                    ] [ text "Play again" ]
                , div [ style "margin-top" "100px", style "font-size" "70px" ] [ text "Congratulations!" ]
                ]
            ]
    
    -- Si le joueur appuie sur "Show answser" il est amenée sur cette page
    Answer word ->
        div []
        [ h1 [ style "text-align" "center", style "font-size" "50px" ] [ text "Guess it!" ]
        , div [ style "text-align" "center" ]
            [ div [ style "font-size" "20px" ] [ text ("The answer was '" ++ word.word ++ "'") ]
            , button
                [ style "text-align" "center"
                , style "font-size" "20px"
                , style "margin-top" "5px"
                , style "width" "200px"
                , onClick Again ] [ text "Play again" ]
            ]
        , ul [ style "margin-left" "50px", style "margin-right" "80px" ] (senseHtml 0 word)
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

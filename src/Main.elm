module Main exposing (..)

import Browser
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
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
    | NewGuess (List Word) String
    | ShowAnswer (List Word)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        -- On recharge la page
        Again -> (Loading, Navigation.reload)

        -- On récupère la liste de mots et on génère un nombre entier aléatoire
        GotWord result ->
            case result of
                Ok wordList -> (GotList wordList, Random.generate RandomInt (Random.int 0 998))
                Err _ -> (Error "Failed to get the word list. Maybe you launched elm reactor from /src (or another folder). If so, you should run in from the parent folder.", Cmd.none)

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
                Ok wordList -> (Success wordList "", Cmd.none)
                Err _ -> (Error "Unable to retreive the json. Dictionary API may be down. Try to refresh the page or check if you can access the API directly from here: https://api.dictionaryapi.dev/api/v2/entries/en/error", Cmd.none)
        
        -- On met à jour la valeur de guess
        NewGuess wordList guess ->
            (Success wordList guess, Cmd.none)
  
        -- On affiche la réponse
        ShowAnswer wordList ->
            (Answer wordList, Cmd.none)
        
-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- FONCTIONS POUR VIEW

-- Renvoie les synonymes d'un mot s'il y en a
ifSyn : Definition -> String
ifSyn def = case def.synonyms of
    (x::xs) -> "\n-> synonyms: " ++ (String.join ", " def.synonyms)
    [] -> ""

-- Renvoie les antonymes d'un mot s'il y en a
ifAnt : Definition -> String
ifAnt def = case def.antonyms of
    (x::xs) -> "\n-> antonyms: " ++ (String.join ", " def.antonyms)
    [] -> ""

-- Renvoie un élément html <li> pour la n-ième définition du mot
toStringDef : Definition -> Html msg
toStringDef def = li [] (List.intersperse (br [] []) (List.map text
        (String.lines (def.definition ++ ifSyn def ++ ifAnt def))
    ))

-- Renvoie une liste d'éléments html <li> correspondant chacun à une définition pour ce sens 
defHtml : List Definition -> List (Html msg)
defHtml defList = case defList of
    def :: defs -> toStringDef def :: defHtml defs
    [] -> []

-- Renvoie une liste d'éléments html <li> correspondant chacun à un 'meaning' pour ce sens (classe grammaticale + <ol> des définitions)
meaningHtml : List Meaning -> List (Html msg)
meaningHtml meaningList = case meaningList of
    meaning :: meanings -> li [] [ text meaning.partsOfSpeech, ol [] (defHtml meaning.definitions) ] :: meaningHtml meanings 
    [] -> []

-- Renvoie une liste d'éléments html <li> correspondant chacun à un élément du tableau Json, c'est-à-dire à un sens du mot (souvent 1 seul)
wordHtml : List Word -> List (Html msg)
wordHtml wordList = case wordList of
    word :: words -> li [] [ ul [] (meaningHtml word.meanings) ] :: wordHtml words
    [] -> []

-- Permet d'extraire les liens vers les prononciations du mot
audioHtml : List Word -> List (Html msg)
audioHtml wordList = case wordList of
    word :: words -> case word.phonetics of
        phonetic :: phonetics -> phoneticHtml word.phonetics
        [] -> []
    [] -> []
phoneticHtml : List Phonetics -> List (Html msg) 
phoneticHtml phonList = case phonList of
    phonetic :: phonetics -> if phonetic.audio == ""
        then phoneticHtml phonetics
        else br [] [] :: audio [ src phonetic.audio, controls True ] [] :: phoneticHtml phonetics
    [] -> []

-- VIEW

type Model
  = Loading
  | Error String
  | GotList String
  | Success (List Word) String
  | Answer (List Word)

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
    Success wordList guess -> case List.head wordList of
        Just word ->
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
                        , Html.Attributes.value guess, onInput (NewGuess wordList) 
                        ] []
                    , br [] []
                    , button
                        [ style "text-align" "center"
                        , style "font-size" "20px"
                        , style "margin-top" "3px"
                        , style "width" "200px"
                        , onClick (ShowAnswer wordList) 
                        ] [ text "Show answer" ]
                    ]
                    , ol [ style "margin-left" "30px", style "margin-right" "100px" ] (wordHtml wordList)
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
                    , div [] (audioHtml wordList)
                    , div [ style "margin-top" "90px", style "font-size" "70px" ] [ text "Congratulations!" ]
                    ]
                ]
        Nothing -> div []
            [ b [ style "text-align" "center" ] [ text "ERROR" ]
            , div [] [ text "The word list is empty." ]
            ]

    -- Si le joueur appuie sur "Show answser" il est amenée sur cette page
    Answer wordList -> case List.head wordList of
        Just word ->
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
                    , div [] (audioHtml wordList)
                ]
            , ol [ style "margin-left" "30px", style "margin-right" "100px" ] (wordHtml wordList)
            ]
        Nothing -> div []
            [ b [ style "text-align" "center" ] [ text "ERROR" ]
            , div [] [ text "The word list is empty." ]
            ]


-- HTTP

getDefinition : String -> Cmd Msg
getDefinition answer = Http.get
    { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ answer
    , expect = Http.expectJson GotDefinition (Json.Decode.list wordDecoder)
    }



-- JSON PARSING

type alias Definition =
    { definition : String
    , synonyms : List String
    , antonyms : List String
    }

type alias Meaning =
    { partsOfSpeech : String
    , definitions : List Definition
    }

type alias Phonetics =
    { audio : String
    }

type alias Word =
    { word : String
    , phonetics : List Phonetics
    , meanings : List Meaning
    }

defDecoder =
    succeed Definition
        |> JP.required "definition" string
        |> JP.required "synonyms" (Json.Decode.list string)
        |> JP.required "antonyms" (Json.Decode.list string)

meaningDecoder =
    succeed Meaning
        |> JP.required "partOfSpeech" string
        |> JP.required "definitions" (Json.Decode.list defDecoder)

phoneticsDecoder =
    succeed Phonetics
        |> JP.required "audio" string

wordDecoder =
    succeed Word
        |> JP.required "word" string
        |> JP.required "phonetics" (Json.Decode.list phoneticsDecoder)
        |> JP.required "meanings" (Json.Decode.list meaningDecoder)

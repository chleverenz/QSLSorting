module QSLSortingAssistant exposing (..)

import Html exposing (..)
import Browser exposing (document)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onClick, onInput)
import Json.Decode as JDEC exposing (..)
import Http

ceptlisturl : String
ceptlisturl =  "./configdata/qslsorting.json"


type alias Sorter = {
    name : String
    , regexp : String
    }

type alias Country = {
        name : String
        , id : String
        , sort : Maybe String
    }

type alias Combiner = 
    {
    callsignpattern : String
    , countryid : String
    , comment : String
    }

type alias SortSetup = {
    sorters : List Sorter
    , combinerlist : List Combiner
    , countries : List Country 
    }

type alias CallsignEntry = {
    callsign : String
    , group : String
    , sortvalue : Maybe String
    , isValid : Bool
    }

type alias QSLSortModel = { 
    callSignList : List CallsignEntry 
    , currentCallsign : String 
    , labels : List String 
    , sortSetup : Maybe SortSetup
    , setupError : Maybe String
    , csvalid : Int
    , version : String 
    }

countrydecoder : JDEC.Decoder Country
countrydecoder =
  map3
    (\a b c -> Country a b c)
    (field "name" string)
    (field "id" string)
    (maybe (field "sort" string))

combinerdecoder : JDEC.Decoder Combiner
combinerdecoder =
  map3
    (\a b c -> Combiner a b c)
    (field "callsignpattern" string)
    (field "countryid" string)
    (field "comment" string)

sorterdecoder : JDEC.Decoder Sorter
sorterdecoder =
  map2
    (\a b -> Sorter a b)
    (field "name" string)
    (field "regexp" string)

setupdecoder : JDEC.Decoder SortSetup
setupdecoder =
  JDEC.map3
    (\a b c -> SortSetup a b c)
    (field "sorters" (JDEC.list sorterdecoder))
    (field "combinerlist" (JDEC.list combinerdecoder))
    (field "countries" (JDEC.list countrydecoder))

type Msg =
    CALLSIGNENTERED String
    | KEYPRESSED Int
    | GETSORTSETUP (Result Http.Error SortSetup)
    | GETDATA

initialData : QSLSortModel

initialData = 
    {   
        currentCallsign = ""
        , callSignList = []
        , labels = [
        ]
        , sortSetup = Nothing
        , setupError = Nothing
        , csvalid = 0
        , version = "0.0.0.2"
    }
    
aSpceificClassDiv : String -> List (Html msg) -> Html msg
aSpceificClassDiv className htmlnodes =
    div [class className] htmlnodes

aRowDiv : List (Html msg) -> Html msg
aRowDiv = aSpceificClassDiv "row"

a6coldiv : List (Html msg) -> Html msg
a6coldiv = aSpceificClassDiv "col-sm-6"

a12coldiv : List (Html msg) -> Html msg
a12coldiv = aSpceificClassDiv "col-sm-12"

renderCallSign : CallsignEntry  -> Html msg
renderCallSign cs =
    div [class "callsign"] [ text cs.callsign ]

renderCallSignList : { a | callSignList : List CallsignEntry } -> List (Html msg)
renderCallSignList model =
    List.map renderCallSign model.callSignList

inputgenerator : { b | csvalid : Int, currentCallsign : String } -> List (Html Msg)
inputgenerator model =
    [
        div [] [
            input [onInput CALLSIGNENTERED, onKeyDown KEYPRESSED, Html.Attributes.value model.currentCallsign, placeholder "enter callsign", autofocus True] []
            , div [] [text (String.fromInt model.csvalid)]
        ]
    ]

onKeyDown : (Int -> value) -> Attribute value
onKeyDown tagger =
  on "keydown" (JDEC.map tagger keyCode)

initialCmd : Cmd Msg
initialCmd =
    setupdecoder
    |> Http.get ceptlisturl
    |> Http.send GETSORTSETUP

getCombinerdata : Maybe SortSetup  -> List Combiner
getCombinerdata setup =
    case setup of
        Nothing -> 
            []
        Just x -> 
            x.combinerlist

findCombinerEntry : (List Combiner)  -> String -> Maybe (List Combiner)
findCombinerEntry combinerlist cs =
    Just (List.filter (\s -> s.callsignpattern == cs) combinerlist)
   
update : Msg -> QSLSortModel  -> ( QSLSortModel  , Cmd Msg ) 
update msg model =
    case msg of
        CALLSIGNENTERED cs -> 
            ( { model | currentCallsign = cs }, Cmd.none)
        KEYPRESSED key ->
            if key == 13 then
                ({ model | 
                    callSignList = List.append model.callSignList [CallsignEntry model.currentCallsign "" Nothing False]
                    , currentCallsign = ""
                    , csvalid = (
                       case (findCombinerEntry (getCombinerdata model.sortSetup) model.currentCallsign) of
                            Nothing ->
                                0
                            Just aList ->
                                List.length aList
                        )
                    } , Cmd.none)
            else
                (model, Cmd.none)
        GETSORTSETUP (Ok setup) ->
            ({model | sortSetup = Just setup, setupError = Nothing } , Cmd.none)
        GETSORTSETUP (Err _) ->
            ({model | setupError = Just "unable to load ceptlist from server" }, Cmd.none)
        GETDATA ->
            (model, initialCmd)

getSetupView setup =
    div [] [
        case setup of 
            Nothing
                -> div [class "nosetupdata"] [text "No Setup Data"]
            Just x ->
                div [class "setupdata"] [
                    div [class "sorters"] [
                        div [] [text "Sorters"]
                        , table [class "table"] (List.map (\entry -> tr [] [td [] [text entry.name], td [] [text entry.regexp]])  x.sorters)
                    ]
                    , div [class "combiners"] [
                        div [] [text "Combiners"]
                        , table [class "table"] (List.map (
                            \entry -> 
                                tr [] 
                                    [
                                        td [] [text entry.callsignpattern]
                                        , td [] [text entry.countryid]
                                        , td [] [text entry.comment]
                                    ]
                            )
                            x.combinerlist )
                        ]
--                                         , td [] [text (case entry.sort of 
--                                                Nothing -> "no nort"
--                                                Just srot -> srot
--                                        )]

                ]
    ]

viewElement model =
    case model.setupError of
        Just errmesg -> 
                div [] [
                    span [] [text errmesg]
                    , span [] [text ceptlisturl]
                ]
        Nothing ->
            aSpceificClassDiv "container" [
                aRowDiv [
                    a12coldiv [
                        h1 [] [ text "CEPT List evaluation" ]
                        , button [onClick GETDATA] [text model.currentCallsign]
                    ]
                ]
                , aRowDiv [
                    a6coldiv (renderCallSignList model)
                    , a6coldiv (inputgenerator model)
                ]
                , getSetupView model.sortSetup
                , aRowDiv [ a12coldiv [
                        h1 [] [text model.version]
                    ]
                ]
            ]

viewDocument model  =
    {
        title  = "atitile"
        , body = [
            (viewElement model)
        ]
    }


init : Int -> (QSLSortModel, Cmd Msg)
init flags = 
    (initialData, initialCmd)

mainDocument = 
    Browser.document
        {
            init = init
            , view = viewDocument
            , update = update
            , subscriptions = (\model -> Sub.none)
        }
         
mainElement = 
    Browser.element
        {
            init = init
            , view = viewElement
            , update = update
            , subscriptions = (\model -> Sub.none)
        }
         

main = mainDocument


module Main exposing (..)

import Html exposing (Html, program, div, text, ul, li, table, thead, tr, td, th, h3, span, button, input)
import Html.Attributes exposing (class, id, type_, pattern, attribute)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Http
import Date exposing (Date)
import Date.Extra exposing (Interval(..))
import Task
import Util exposing (humanize, maybeHumanize)
import Time exposing (Time, second)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { events : List Event
    , message : String
    , currentDate : Maybe Date
    , shiftMins : Int
    }


type Event
    = Feeding FeedingContent
    | Medication MedicationContent


type alias FeedingContent =
    { time : String
    , who : String
    }


type alias MedicationContent =
    { time : String
    , name : String
    }


emptyModel : Model
emptyModel =
    { events = []
    , message = ""
    , currentDate = Nothing
    , shiftMins = 0
    }


init : ( Model, Cmd Msg )
init =
    emptyModel ! [ Task.perform Tick Time.now ]



-- UPDATE


type Msg
    = NewEvents (Result Http.Error (List Event))
    | FetchEvents
    | NoOp
    | Tick Time
    | ShiftMinsInput String
    | Medicate String
    | Feed
    | FeedOrMedicateResult (Result Http.Error Decode.Value)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Feed ->
            case model.currentDate of
                Just date ->
                    model ! [ (postFeed (Date.Extra.add Minute (model.shiftMins * -1) date)) ]

                Nothing ->
                    { model | message = "currentDate invalid, can't post feeding" } ! []

        Medicate name ->
            case model.currentDate of
                Just date ->
                    model ! [ (postMedication (Date.Extra.add Minute (model.shiftMins * -1) date) name) ]

                Nothing ->
                    { model | message = "currentDate invalid, can't post medication" } ! []

        ShiftMinsInput mins ->
            let
                shiftMins =
                    case String.toInt mins of
                        Ok i ->
                            i

                        Err _ ->
                            0
            in
                { model | shiftMins = shiftMins } ! []

        FetchEvents ->
            model ! [ getEvents ]

        Tick time ->
            let
                currentDate =
                    Date.fromTime time
            in
                if (Date.second currentDate) == 0 || model.currentDate == Nothing then
                    { model | currentDate = Just currentDate } ! [ getEvents ]
                else
                    { model | currentDate = Just currentDate } ! []

        NewEvents (Ok events) ->
            { model
                | message = ""
                , events = List.sortWith eventSort events
            }
                ! []

        NewEvents (Err _) ->
            { model | message = "error loading events!" } ! []

        FeedOrMedicateResult (Ok _) ->
            model ! [ getEvents ]

        FeedOrMedicateResult (Err _) ->
            { model | message = "error posting!" } ! []


eventSort : Event -> Event -> Order
eventSort e1 e2 =
    let
        extractTime event =
            case event of
                Medication m ->
                    m.time

                Feeding f ->
                    f.time

        t1 =
            extractTime (e1)

        t2 =
            extractTime (e2)
    in
        case compare t1 t2 of
            LT ->
                GT

            EQ ->
                EQ

            GT ->
                LT


eventListDecoder : Decode.Decoder (List Event)
eventListDecoder =
    Decode.map (List.map Tuple.second) (Decode.keyValuePairs eventDecoder)


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.oneOf [ medicationDecoder, feedingDecoder ]


medicationDecoder : Decode.Decoder Event
medicationDecoder =
    Decode.map Medication
        (Decode.map2 MedicationContent
            (Decode.field "time" Decode.string)
            (Decode.field "name" Decode.string)
        )


feedingDecoder : Decode.Decoder Event
feedingDecoder =
    Decode.map Feeding
        (Decode.map2 FeedingContent
            (Decode.field "time" Decode.string)
            (Decode.field "who" Decode.string)
        )


findLatestMedicationDate : String -> List Event -> Maybe Date
findLatestMedicationDate name events =
    case findLatestMedicationInner name events Nothing of
        Just m ->
            Date.Extra.fromIsoString m.time

        Nothing ->
            Nothing


{-| expects a sorted list of events
-}
findLatestMedication : String -> List Event -> Maybe MedicationContent
findLatestMedication name events =
    findLatestMedicationInner name events Nothing


findLatestMedicationInner : String -> List Event -> Maybe MedicationContent -> Maybe MedicationContent
findLatestMedicationInner name events maybeMedication =
    case ( events, maybeMedication ) of
        ( _, Just m ) ->
            maybeMedication

        ( [], m ) ->
            m

        ( (Feeding h) :: t, Nothing ) ->
            findLatestMedicationInner name t Nothing

        ( (Medication h) :: t, Nothing ) ->
            if h.name == name then
                Just h
            else
                findLatestMedicationInner name t Nothing


findLatestFeedingDate : List Event -> Maybe Date
findLatestFeedingDate events =
    case findLastestFeeding events of
        Just f ->
            Date.Extra.fromIsoString f.time

        Nothing ->
            Nothing


findLastestFeeding : List Event -> Maybe FeedingContent
findLastestFeeding events =
    findLastestFeedingInner events Nothing


findLastestFeedingInner : List Event -> Maybe FeedingContent -> Maybe FeedingContent
findLastestFeedingInner events maybeFeeding =
    case ( events, maybeFeeding ) of
        ( _, Just f ) ->
            maybeFeeding

        ( [], f ) ->
            f

        ( (Feeding h) :: t, Nothing ) ->
            Just h

        ( (Medication _) :: t, Nothing ) ->
            findLastestFeedingInner t Nothing



-- HTTP


getEvents : Cmd Msg
getEvents =
    Http.send NewEvents <|
        Http.get
            ("https://olive-the-things.firebaseio.com/events.json?"
                ++ "orderBy=\"time\""
                ++ "&startAt=\"2017-07-13T00:00:00.000Z\""
            )
            eventListDecoder


postFeed : Date -> Cmd Msg
postFeed date =
    Http.send FeedOrMedicateResult <|
        Http.post
            "https://olive-the-things.firebaseio.com/events.json"
            (Http.jsonBody
                (Encode.object
                    [ ( "type", Encode.string "feeding" )
                    , ( "time", Encode.string (Date.Extra.toUtcIsoString date) )
                    , ( "who", Encode.string "everett" )
                    ]
                )
            )
            Decode.value


postMedication : Date -> String -> Cmd Msg
postMedication date name =
    Http.send FeedOrMedicateResult <|
        Http.post
            "https://olive-the-things.firebaseio.com/events.json"
            (Http.jsonBody
                (Encode.object
                    [ ( "type", Encode.string "medication" )
                    , ( "time", Encode.string (Date.Extra.toUtcIsoString date) )
                    , ( "name", Encode.string name )
                    ]
                )
            )
            Decode.value



-- VIEW


view : Model -> Html Msg
view model =
    let
        feedingDate =
            findLatestFeedingDate model.events

        ironDate =
            findLatestMedicationDate "iron" model.events

        prenatalDate =
            findLatestMedicationDate "prenatal" model.events

        vitaminDDate =
            findLatestMedicationDate "vitamind" model.events

        currentDateHourMinute =
            case model.currentDate of
                Just d ->
                    Date.Extra.toFormattedString "HH:mm" d

                Nothing ->
                    "invalid currentDate"
    in
        div [ id "app" ]
            [ div [] [ text model.message ]
            , h3 [ class "right" ]
                [ span [] [ text ("Refreshed at " ++ currentDateHourMinute) ]
                , button [ class "refresh pure-button", onClick FetchEvents ] [ text "↻" ]
                ]
            , table [ class "highlights pure-table pure-table-horizontal" ]
                [ tr []
                    [ th [] [ text "Feeding" ]
                    , td [] [ text (maybeHumanize feedingDate model.currentDate) ]
                    ]
                , tr []
                    [ th [] [ text "Iron" ]
                    , td [] [ text (maybeHumanize ironDate model.currentDate) ]
                    ]
                , tr []
                    [ th [] [ text "Prenatal" ]
                    , td [] [ text (maybeHumanize prenatalDate model.currentDate) ]
                    ]
                , tr []
                    [ th [] [ text "Vitamin D" ]
                    , td [] [ text (maybeHumanize vitaminDDate model.currentDate) ]
                    ]
                ]
            , div [ class "center" ]
                [ span [] [ text "shift mins: " ]
                , input
                    [ type_ "number"
                    , attribute "inputmode" "numeric"
                    , pattern "[0-9]*"
                    , class "timeshift"
                    , onInput ShiftMinsInput
                    ]
                    []
                ]
            , div [ class "buttons right" ]
                [ div [] [ button [ class "pure-button pure-button-primary", onClick Feed ] [ text "Feed" ] ]
                , div [] [ button [ class "pure-button pure-button-primary", onClick (Medicate "iron") ] [ text "Iron" ] ]
                , div [] [ button [ class "pure-button pure-button-primary", onClick (Medicate "prenatal") ] [ text "Prenatal" ] ]
                , div [] [ button [ class "pure-button pure-button-primary", onClick (Medicate "vitamind") ] [ text "Vitamin D" ] ]
                ]
            , table [ class "pure-table" ]
                [ Html.thead []
                    [ tr []
                        [ th [] [ text "Type" ]
                        , th [] [ text "Date" ]
                        , th [] [ text "Medication Name" ]
                        ]
                    ]
                , Html.tbody []
                    (List.map (viewEvent model) model.events)
                ]
            ]


viewEvent : Model -> Event -> Html Msg
viewEvent model event =
    case event of
        Medication m ->
            tr []
                [ td [] [ text "Medication" ]
                , td [] [ text (maybeHumanize (Date.Extra.fromIsoString m.time) model.currentDate) ]
                , td [] [ text m.name ]
                ]

        Feeding f ->
            tr []
                [ td [] [ text "Feeding" ]
                , td [] [ text (maybeHumanize (Date.Extra.fromIsoString f.time) model.currentDate) ]
                , td [] []
                ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

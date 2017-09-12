module Main exposing (..)

import Html exposing (Html, program, div, text, ul, li, table, thead, tbody, tr, td, th, h3, span, button, input)
import Html.Attributes exposing (class, id, type_, pattern, attribute)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Http exposing (Error(..))
import Date exposing (Date)
import Date.Extra exposing (Interval(..))
import Task
import Util exposing (humanize, maybeHumanize, showEndDate, showDuration, showDate)
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
    { message : String
    , currentDate : Maybe Date
    , shiftMins : Int
    , feedings : List FeedingContent
    , medications : List MedicationContent
    , ounces : Float
    }


type alias FeedingContent =
    { id : String
    , time : String
    , who : String
    , endTime : Maybe String
    , ounces : Float
    }


type alias MedicationContent =
    { time : String
    , name : String
    }


emptyModel : Model
emptyModel =
    { message = ""
    , currentDate = Nothing
    , shiftMins = 0
    , feedings = []
    , medications = []
    , ounces = 0
    }


init : ( Model, Cmd Msg )
init =
    emptyModel ! [ Task.perform Tick Time.now ]



-- UPDATE


type Msg
    = FetchEvents
    | NoOp
    | Tick Time
    | ShiftMinsInput String
    | OuncesInput String
    | Medicate String
    | Feed
    | StopFeed
    | ApplyOunces
    | FeedResult (Result Http.Error Decode.Value)
    | MedicateResult (Result Http.Error Decode.Value)
    | NewFeedings (Result Http.Error (List FeedingContent))
    | NewMedications String (Result Http.Error (List MedicationContent))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        ApplyOunces ->
            let
                maybeLatestFeeding =
                    findLatestFeeding model.feedings
            in
                case maybeLatestFeeding of
                    Just feeding ->
                        model ! [ postOunces feeding.id model.ounces ]

                    Nothing ->
                        model ! []

        Feed ->
            case model.currentDate of
                Just date ->
                    model ! [ (postFeed (Date.Extra.add Minute (model.shiftMins * -1) date)) ]

                Nothing ->
                    { model | message = "currentDate invalid, can't post feeding" } ! []

        -- TODO: implement this
        StopFeed ->
            let
                maybeLatestFeeding =
                    findLatestFeeding model.feedings
            in
                case ( model.currentDate, maybeLatestFeeding ) of
                    ( Just date, Just latestFeeding ) ->
                        model ! [ (postFeedEnd latestFeeding.id (Date.Extra.add Minute (model.shiftMins * -1) date)) ]

                    ( _, _ ) ->
                        { model | message = "Invalid currentDate or latestFeeding" } ! []

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

        OuncesInput mins ->
            let
                o =
                    case String.toFloat mins of
                        Ok i ->
                            i

                        Err _ ->
                            0
            in
                { model | ounces = o } ! []

        FetchEvents ->
            model ! refresh

        Tick time ->
            let
                currentDate =
                    Date.fromTime time
            in
                if (Date.second currentDate) == 0 || model.currentDate == Nothing then
                    { model | currentDate = Just currentDate } ! refresh
                else
                    { model | currentDate = Just currentDate } ! []

        FeedResult (Ok _) ->
            model ! refresh

        FeedResult (Err _) ->
            model ! []

        MedicateResult (Ok _) ->
            model ! refresh

        MedicateResult (Err _) ->
            model ! []

        NewFeedings (Ok feedings) ->
            { model
                | feedings = feedings
                , message = ""
            }
                ! []

        NewFeedings (Err err) ->
            case err of
                BadUrl msg ->
                    { model | message = msg } ! []

                Timeout ->
                    { model | message = "timeout" } ! []

                NetworkError ->
                    { model | message = "network error" } ! []

                BadStatus _ ->
                    { model | message = "bad status" } ! []

                BadPayload msg _ ->
                    { model | message = msg } ! []

        NewMedications name (Ok medications) ->
            { model | medications = (filterMedications name model.medications) ++ medications } ! []

        NewMedications _ (Err _) ->
            model ! []


filterMedications : String -> List MedicationContent -> List MedicationContent
filterMedications name medications =
    (List.filter
        (\medication ->
            medication.name /= name
        )
        medications
    )


refresh : List (Cmd Msg)
refresh =
    [ getFeedings, getMedication "vitamind", getMedication "iron", getMedication "prenatal", getMedication "probiotic" ]


combine : ( String, FeedingContent ) -> FeedingContent
combine t =
    let
        id =
            Tuple.first (t)

        obj =
            Tuple.second (t)
    in
        { obj | id = id }


feedingListDecoder : Decode.Decoder (List FeedingContent)
feedingListDecoder =
    Decode.map (List.map combine) (Decode.keyValuePairs feedingDecoder)


medicationListDecoder : String -> Decode.Decoder (List MedicationContent)
medicationListDecoder name =
    Decode.map (List.map Tuple.second) (Decode.keyValuePairs (medicationDecoder name))


medicationDecoder : String -> Decode.Decoder MedicationContent
medicationDecoder name =
    Decode.map2 MedicationContent
        (Decode.field "time" Decode.string)
        (Decode.succeed name)


feedingDecoder : Decode.Decoder FeedingContent
feedingDecoder =
    (Decode.map4
        (\time who endTime ounces ->
            FeedingContent ""
                time
                who
                endTime
                (case ounces of
                    Just f ->
                        f

                    Nothing ->
                        0
                )
        )
        (Decode.field "time" Decode.string)
        (Decode.field "who" Decode.string)
        (Decode.maybe (Decode.field "endTime" Decode.string))
        (Decode.maybe (Decode.field "ounces" Decode.float))
    )


findLatestMedicationDate : String -> List MedicationContent -> Maybe Date
findLatestMedicationDate name medications =
    case findLatestMedicationInner name medications Nothing of
        Just m ->
            Date.Extra.fromIsoString m.time

        Nothing ->
            Nothing


{-| expects a sorted list of events
-}
findLatestMedication : String -> List MedicationContent -> Maybe MedicationContent
findLatestMedication name medications =
    findLatestMedicationInner name medications Nothing


findLatestMedicationInner : String -> List MedicationContent -> Maybe MedicationContent -> Maybe MedicationContent
findLatestMedicationInner name medications maybeMedication =
    case ( medications, maybeMedication ) of
        ( _, Just m ) ->
            maybeMedication

        ( [], m ) ->
            m

        ( h :: t, Nothing ) ->
            if h.name == name then
                Just h
            else
                findLatestMedicationInner name t Nothing


findLatestFeedingTime : List FeedingContent -> Maybe Date
findLatestFeedingTime feedings =
    case findLatestFeeding feedings of
        Just f ->
            Date.Extra.fromIsoString f.time

        Nothing ->
            Nothing


findLatestFeedingTimeEnd : List FeedingContent -> Maybe Date
findLatestFeedingTimeEnd feedings =
    case findLatestFeeding feedings of
        Just f ->
            case f.endTime of
                Just t ->
                    Date.Extra.fromIsoString t

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


findLatestFeeding : List FeedingContent -> Maybe FeedingContent
findLatestFeeding feedings =
    findLastestFeedingInner feedings Nothing


findLastestFeedingInner : List FeedingContent -> Maybe FeedingContent -> Maybe FeedingContent
findLastestFeedingInner feedings maybeFeeding =
    case ( feedings, maybeFeeding ) of
        ( _, Just f ) ->
            maybeFeeding

        ( [], f ) ->
            f

        ( h :: t, Nothing ) ->
            Just h



-- HTTP


getFeedings : Cmd Msg
getFeedings =
    Http.send NewFeedings <|
        Http.get
            ("https://olive-the-things.firebaseio.com/feedings.json?"
                ++ "orderBy=\"time\""
                ++ "&limitToLast=20"
            )
            feedingListDecoder


getMedication : String -> Cmd Msg
getMedication name =
    Http.send (NewMedications name) <|
        Http.get
            ("https://olive-the-things.firebaseio.com/"
                ++ name
                ++ ".json?"
                ++ "orderBy=\"time\""
                ++ "&limitToLast=1"
            )
            (medicationListDecoder name)


postFeed : Date -> Cmd Msg
postFeed date =
    Http.send FeedResult <|
        Http.post
            "https://olive-the-things.firebaseio.com/feedings.json"
            (Http.jsonBody
                (Encode.object
                    [ ( "time", Encode.string (Date.Extra.toUtcIsoString date) )
                    , ( "who", Encode.string "everett" )
                    ]
                )
            )
            Decode.value


postFeedEnd : String -> Date -> Cmd Msg
postFeedEnd id date =
    Http.send FeedResult <|
        Http.post
            ("https://olive-the-things.firebaseio.com/feedings/" ++ id ++ ".json?x-http-method-override=PATCH")
            (Http.jsonBody
                (Encode.object [ ( "endTime", Encode.string (Date.Extra.toUtcIsoString date) ) ])
            )
            Decode.value


postOunces : String -> Float -> Cmd Msg
postOunces id ounces =
    Http.send FeedResult <|
        Http.post
            ("https://olive-the-things.firebaseio.com/feedings/" ++ id ++ ".json?x-http-method-override=PATCH")
            (Http.jsonBody
                (Encode.object [ ( "ounces", Encode.float ounces ) ])
            )
            Decode.value


postMedication : Date -> String -> Cmd Msg
postMedication date name =
    Http.send MedicateResult <|
        Http.post
            ("https://olive-the-things.firebaseio.com/" ++ name ++ ".json")
            (Http.jsonBody
                (Encode.object
                    [ ( "time", Encode.string (Date.Extra.toUtcIsoString date) ) ]
                )
            )
            Decode.value



-- VIEW


view : Model -> Html Msg
view model =
    let
        latestFeeding =
            case findLatestFeeding model.feedings of
                Just feeding ->
                    feeding

                Nothing ->
                    FeedingContent "" "" "" Nothing 0

        latestFeedingTime =
            findLatestFeedingTime model.feedings

        vitaminDDate =
            findLatestMedicationDate "vitamind" model.medications

        ironDate =
            findLatestMedicationDate "iron" model.medications

        probioticDate =
            findLatestMedicationDate "probiotic" model.medications

        prenatalDate =
            findLatestMedicationDate "prenatal" model.medications

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
                    , td []
                        [ text
                            ((maybeHumanize latestFeedingTime model.currentDate)
                                ++ " "
                                ++ (showDuration latestFeeding.time latestFeeding.endTime)
                            )
                        ]
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
                , tr []
                    [ th [] [ text "Probiotic" ]
                    , td [] [ text (maybeHumanize probioticDate model.currentDate) ]
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
            , div [ class "center" ]
                [ span [] [ text "ounces: " ]
                , input
                    [ type_ "number"
                    , attribute "inputmode" "numeric"
                    , class "timeshift"
                    , onInput OuncesInput
                    ]
                    []
                ]
            , div [ class "buttons right" ]
                [ div [ class "pure-button-group" ]
                    [ button [ class "pure-button pure-button-primary", onClick Feed ] [ text "Feed" ]
                    , button [ class "pure-button button-error", attribute "role" "group", onClick StopFeed ] [ text "Stop" ]
                    , button [ class "pure-button button-warning", attribute "role" "group", onClick ApplyOunces ] [ text "Ounces" ]
                    ]
                , div [] [ button [ class "pure-button pure-button-primary", onClick (Medicate "iron") ] [ text "Iron" ] ]
                , div [] [ button [ class "pure-button pure-button-primary", onClick (Medicate "prenatal") ] [ text "Prenatal" ] ]
                , div [] [ button [ class "pure-button pure-button-primary", onClick (Medicate "vitamind") ] [ text "Vitamin D" ] ]
                , div [] [ button [ class "pure-button pure-button-primary", onClick (Medicate "probiotic") ] [ text "Probiotic" ] ]
                ]
            , table [ class "pure-table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Date" ]
                        , th [] [ text "Duration" ]
                        , th [] [ text "Ounces" ]
                        ]
                    ]
                , tbody [] (List.map (viewFeeding model) model.feedings)
                ]
            ]


viewFeeding : Model -> FeedingContent -> Html Msg
viewFeeding model feeding =
    tr []
        [ td [] [ text (showDate feeding.time) ]
        , td [] [ text (showDuration feeding.time feeding.endTime) ]
        , td [] [ text (toString feeding.ounces) ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick

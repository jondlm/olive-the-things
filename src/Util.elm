module Util exposing (..)

import Date exposing (Date)
import Date.Extra exposing (diff)


maybeHumanize : Maybe Date -> Maybe Date -> String
maybeHumanize mStartDate mEndDate =
    case ( mStartDate, mEndDate ) of
        ( Just startDate, Just endDate ) ->
            humanize startDate endDate

        ( _, _ ) ->
            "..."


humanize : Date -> Date -> String
humanize startDate endDate =
    let
        totalMinutes =
            diff Date.Extra.Minute startDate endDate

        totalHours =
            diff Date.Extra.Hour startDate endDate

        totalDays =
            diff Date.Extra.Day startDate endDate

        minutes =
            totalMinutes % 60

        minutesMessage =
            if totalMinutes == 0 then
                "just now"
            else if minutes > 0 then
                (toString minutes) ++ " min"
            else
                ""

        hours =
            floor ((toFloat totalMinutes) / 60)

        hoursMessage =
            if hours > 0 then
                (toString hours) ++ " hr"
            else
                ""

        hoursMinutes =
            Date.Extra.toFormattedString "HH:mm" startDate

        ago =
            if totalMinutes > 0 then
                " ago "
            else
                " "

        suffix =
            ago ++ "at " ++ hoursMinutes
    in
        hoursMessage ++ " " ++ minutesMessage ++ suffix

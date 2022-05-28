module Calendar exposing (Calendar, Selection(..), Status(..), advance, calendar, extendSelection, retreat)

import Date exposing (Date, Unit(..))
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Time exposing (Month(..), Weekday(..))
import Two exposing (Two)


type Selection
    = None
    | Single Date
    | Range (Two Date)


type Status
    = NotSelected
    | Selected
    | InRange
    | StartRange
    | EndRange


type alias Calendar msg =
    { page : ( Month, Int )
    , selection : Selection
    , viewWeekday : Weekday -> Html msg
    , viewDate : { date : Date, status : Status, inMonth : Bool } -> Html msg
    }


calendar : List (Html.Attribute msg) -> Calendar msg -> Html msg
calendar attrs model =
    let
        { page, selection, viewWeekday, viewDate } =
            model

        ( month, year ) =
            page

        monthStart =
            Date.fromCalendarDate year month 1

        monthEnd =
            Date.fromCalendarDate year month 31

        range =
            Date.range Date.Day 1 monthStart monthEnd

        before =
            List.head range
                |> Maybe.map
                    (\firstDate ->
                        List.range 1 (Date.weekdayNumber firstDate - 1)
                            |> List.map (\n -> Date.add Days -n firstDate)
                            |> List.reverse
                    )
                |> Maybe.withDefault []

        after =
            last range
                |> Maybe.map
                    (\lastDate ->
                        List.range 1 (7 * 6 - List.length (before ++ range))
                            |> List.map (\n -> Date.add Days n lastDate)
                    )
                |> Maybe.withDefault []
    in
    div (style "display" "grid" :: style "grid-template-columns" "repeat(7, 1fr)" :: style "grid-template-rows" "repeat(7, 1fr)" :: attrs) <|
        (viewDayHeaders viewWeekday
            ++ List.map (\date -> viewDate { date = date, inMonth = False, status = status selection date }) before
            ++ List.map (\date -> viewDate { date = date, inMonth = True, status = status selection date }) range
            ++ List.map (\date -> viewDate { date = date, inMonth = False, status = status selection date }) after
        )


last : List a -> Maybe a
last xs =
    case xs of
        [ x ] ->
            Just x

        _ :: tail ->
            last tail

        [] ->
            Nothing


days : List Weekday
days =
    [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]


viewDayHeaders : (Weekday -> Html msg) -> List (Html msg)
viewDayHeaders toString =
    days |> List.map (\day -> toString day)


status selection date =
    case selection of
        None ->
            NotSelected

        Single x ->
            if x == date then
                Selected

            else
                NotSelected

        Range dates ->
            if inRange dates date then
                let
                    min =
                        Two.extract Date.min dates

                    max =
                        Two.extract Date.max dates
                in
                if date == max then
                    EndRange

                else if date == min then
                    StartRange

                else
                    InRange

            else
                NotSelected


inRange : Two Date -> Date -> Bool
inRange dates date =
    Two.check Date.isBetween dates date


advance : ( Month, Int ) -> ( Month, Int )
advance ( month, year ) =
    case month of
        Jan ->
            ( Feb, year )

        Feb ->
            ( Mar, year )

        Mar ->
            ( Apr, year )

        Apr ->
            ( May, year )

        May ->
            ( Jun, year )

        Jun ->
            ( Jul, year )

        Jul ->
            ( Aug, year )

        Aug ->
            ( Sep, year )

        Sep ->
            ( Oct, year )

        Oct ->
            ( Nov, year )

        Nov ->
            ( Dec, year )

        Dec ->
            ( Jan, year + 1 )


retreat : ( Month, Int ) -> ( Month, Int )
retreat ( month, year ) =
    case month of
        Jan ->
            ( Dec, year - 1 )

        Feb ->
            ( Jan, year )

        Mar ->
            ( Feb, year )

        Apr ->
            ( Mar, year )

        May ->
            ( Apr, year )

        Jun ->
            ( May, year )

        Jul ->
            ( Jun, year )

        Aug ->
            ( Jul, year )

        Sep ->
            ( Aug, year )

        Oct ->
            ( Sep, year )

        Nov ->
            ( Oct, year )

        Dec ->
            ( Nov, year )


extendSelection selection d =
    case selection of
        None ->
            Single d

        Single date ->
            Range (Two.two date d)

        Range range ->
            Range (Two.replace Date.max range d)

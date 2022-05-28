module Main exposing (main)

import Browser
import Calendar exposing (..)
import Date exposing (Date)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Time exposing (Month(..), Weekday(..))
import Two


type alias Model =
    { page : ( Month, Int )
    , selection : Selection
    }


type Msg
    = NextMonth
    | PreviousMonth
    | DateClicked Date


init : Model
init =
    { page = ( May, 2022 )
    , selection = None
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PreviousMonth ->
            { model | page = advance model.page }

        NextMonth ->
            { model | page = advance model.page }

        DateClicked date ->
            { model | selection = extendSelection model.selection date }


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "header" ]
            [ viewPreviousMonthButton PreviousMonth
            , viewTitle model.page
            , viewNextMonthButton NextMonth
            ]
        , Calendar.calendar
            [ class "calendar" ]
            { onDateClicked = DateClicked
            , page = model.page
            , selection = model.selection
            , viewWeekday = viewWeekday
            , viewDate = viewDate
            }
        ]


viewTitle : ( Month, Int ) -> Html msg
viewTitle ( month, year ) =
    text <| monthToString month ++ " " ++ String.fromInt year


viewNextMonthButton onNextMonth =
    button [ onClick onNextMonth ] [ text "⮕" ]


viewPreviousMonthButton onPreviousMonth =
    button [ onClick onPreviousMonth ] [ text "⬅" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


viewWeekday day =
    span []
        [ text <|
            case day of
                Mon ->
                    "Mon"

                Tue ->
                    "Tue"

                Wed ->
                    "Wed"

                Thu ->
                    "Thu"

                Fri ->
                    "Fri"

                Sat ->
                    "Sat"

                Sun ->
                    "Sun"
        ]


viewDate { date, status, inMonth } =
    let
        attr =
            case status of
                NotSelected ->
                    style "" ""

                Selected ->
                    style "background-color" "grey"

                InRange ->
                    style "background-color" "indianred"

                StartRange ->
                    style "background-color" "cadetblue"

                EndRange ->
                    style "background-color" "palegreen"
    in
    button
        [ attr
        , onClick (DateClicked date)
        , disabled <| Date.weekday date == Sun || Date.weekday date == Sat
        , style "opacity"
            (if inMonth then
                "1"

             else
                "0.2"
            )
        ]
        [ text (String.fromInt (Date.day date))
        ]

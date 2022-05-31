module Main exposing (main)

import Browser
import Calendar exposing (..)
import Date exposing (Date)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Time exposing (Month(..), Weekday(..))



--import Two exposing (Two)
{-
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
-}


type alias Model =
    { page : ( Month, Int )
    , selection : Date
    }


type Msg
    = NextMonth
    | PreviousMonth
    | DateClicked Date


init : Model
init =
    { page = ( May, 2022 )
    , selection = Date.fromCalendarDate 2022 May 1
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PreviousMonth ->
            { model | page = previous model.page }

        NextMonth ->
            { model | page = next model.page }

        DateClicked date ->
            { model | selection = date }


view : Model -> Html Msg
view model =
    Calendar.calendar
        [ class "calendar" ]
        { page = model.page
        , viewHeader = viewHeader
        , viewDate = viewDate model.selection
        }



-- [ div
--     [ class "header" ]
--     [ viewPreviousMonthButton
--     , viewTitle model.page
--     , viewNextMonthButton
--     ]
-- viewTitle : ( Month, Int ) -> Html msg
-- viewTitle ( month, year ) =
--     text <| monthToString month ++ " " ++ String.fromInt year
-- viewNextMonthButton =
--     button [ onClick NextMonth ] [ text "⮕" ]
-- viewPreviousMonthButton =
--     button [ onClick PreviousMonth ] [ text "⬅" ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- monthToString : Month -> String
-- monthToString month =
--     case month of
--         Jan ->
--             "Jan"
--         Feb ->
--             "Feb"
--         Mar ->
--             "Mar"
--         Apr ->
--             "Apr"
--         May ->
--             "May"
--         Jun ->
--             "Jun"
--         Jul ->
--             "Jul"
--         Aug ->
--             "Aug"
--         Sep ->
--             "Sep"
--         Oct ->
--             "Oct"
--         Nov ->
--             "Nov"
--         Dec ->
--             "Dec"


viewHeader day =
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


viewDate selection { date, inMonth } =
    -- let
    --     -- attr =
    --     case status of
    --         NotSelected ->
    --             style "" ""
    --         Selected ->
    --             style "background-color" "grey"
    --         InRange ->
    --             style "background-color" "indianred"
    --         StartRange ->
    --             style "background-color" "cadetblue"
    --         EndRange ->
    --             style "background-color" "palegreen"
    -- in
    button
        [ onClick (DateClicked date)
        , disabled <| Date.weekday date == Sun || Date.weekday date == Sat
        , style "opacity"
            (if inMonth then
                "1"

             else
                "0.2"
            )
        , style "background-color"
            (if selection == date then
                "lightblue"

             else
                "inherit"
            )
        ]
        [ text (String.fromInt (Date.day date))
        ]

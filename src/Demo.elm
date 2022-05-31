module Demo exposing (main)

import Browser
import Calendar
import Date exposing (Date)
import Html exposing (Html, button, span, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Time exposing (Month(..), Weekday(..))


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
            { model | page = Calendar.previous model.page }

        NextMonth ->
            { model | page = Calendar.next model.page }

        DateClicked date ->
            { model | selection = date, page = ( Date.month date, Date.year date ) }


view : Model -> Html Msg
view model =
    Calendar.calendar
        [ class "calendar" ]
        { page = model.page
        , viewHeader = viewHeader
        , viewDate = viewDate model.selection
        }


viewHeader : Weekday -> Html msg
viewHeader day =
    span [] [ text <| Date.format "E" (Date.fromWeekDate 0 0 day) ]


viewDate : Date -> { date : Date, inMonth : Bool } -> Html Msg
viewDate selection { date, inMonth } =
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
        [ text <| Date.format "ddd" date
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

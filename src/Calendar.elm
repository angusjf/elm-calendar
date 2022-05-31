module Calendar exposing
    ( calendar
    , next
    , previous
    )

import Date exposing (Date, Unit(..))
import Html exposing (Html)
import Html.Attributes
import Time exposing (Month(..), Weekday(..))


calendar :
    List (Html.Attribute msg)
    ->
        { page : ( Month, Int )
        , viewHeader : Weekday -> Html msg
        , viewDate : { date : Date, inMonth : Bool } -> Html msg
        }
    -> Html msg
calendar attrs { page, viewHeader, viewDate } =
    let
        first =
            firstOnPage page

        last =
            firstOnPage (next page)

        headers =
            List.map viewHeader [ Mon, Tue, Wed, Thu, Fri, Sat, Sun ]

        underflow =
            List.range 1 (Date.weekdayNumber first - 1)
                |> List.reverse
                |> List.map (\n -> Date.add Days -n first)
                |> List.map (\date -> viewDate { date = date, inMonth = False })

        month =
            Date.range Date.Day 1 first last
                |> List.map (\date -> viewDate { date = date, inMonth = True })

        overflow =
            List.range 0 (7 * 6 - List.length underflow - List.length month - 1)
                |> List.map (\n -> Date.add Days n last)
                |> List.map (\date -> viewDate { date = date, inMonth = False })
    in
    grid attrs <|
        headers
            ++ underflow
            ++ month
            ++ overflow


grid : List (Html.Attribute msg) -> List (Html msg) -> Html msg
grid attrs children =
    Html.div
        (Html.Attributes.style "display" "grid"
            :: Html.Attributes.style "grid-template-columns" "repeat(7, 1fr)"
            :: Html.Attributes.style "grid-template-rows" "repeat(7, 1fr)"
            :: attrs
        )
        children


firstOnPage : ( Month, Int ) -> Date
firstOnPage ( month, year ) =
    Date.fromCalendarDate year month 1


next : ( Month, Int ) -> ( Month, Int )
next ( month, year ) =
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


previous : ( Month, Int ) -> ( Month, Int )
previous ( month, year ) =
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

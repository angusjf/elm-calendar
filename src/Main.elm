module Main exposing (main)

import Browser
import Calendar
import Html exposing (span, text)
import Time exposing (Month(..), Weekday(..))


main : Program () () ()
main =
    Browser.sandbox
        { init = ()
        , update = \_ _ -> ()
        , view =
            \_ ->
                Calendar.calendar []
                    { page = ( May, 2022 )
                    , viewHeader = \_ -> span [] [ text "header" ]
                    , viewDate = \_ -> span [] [ text "date" ]
                    }
        }

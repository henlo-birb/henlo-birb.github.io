module Time exposing (..)

import Parser exposing (..)
import Utilities exposing (..)


type AmPm
    = AM
    | PM


toString : Time -> String
toString time_ =
    case time_ of
        Time time ->
            String.fromInt time.hour
                ++ ":"
                ++ ternary (time.minute == 0) "00" (String.fromInt time.minute)
                ++ (case time.ampm of
                        AM ->
                            "AM"

                        PM ->
                            "PM"
                   )

        UnknownTime ->
            "Unknown"


timeParser : Parser Time
timeParser =
    let
        timeInt : Parser Int
        timeInt =
            oneOf
                [ succeed identity
                    |. token "0"
                    |= int
                , int
                ]
    in
    oneOf
        [ succeed Time
            |= (succeed TimeRecord
                    |= timeInt
                    |. symbol ":"
                    |= timeInt
                    |. spaces
                    |= oneOf
                        [ map (always AM) (keyword "AM")
                        , map (always PM) (keyword "PM")
                        ]
               )
        , succeed UnknownTime
        ]


type alias TimeRecord =
    { hour : Int
    , minute : Int
    , ampm : AmPm
    }


type Time
    = Time TimeRecord
    | UnknownTime


compareTime a b =
    let
        to24H : Time -> Int
        to24H time_ =
            case time_ of
                Time time ->
                    case time.ampm of
                        AM ->
                            ternary (time.hour == 12) 0 time.hour

                        PM ->
                            ternary (time.hour == 12) 12 (time.hour + 12)

                UnknownTime ->
                    100

        compareHours =
            compare (to24H a) (to24H b)

        getMinute : Time -> Int
        getMinute time_ =
            case time_ of
                Time time ->
                    time.minute

                UnknownTime ->
                    100

        compareMinutes =
            compare (getMinute a) (getMinute b)
    in
    ternary (compareHours == EQ) compareMinutes compareHours

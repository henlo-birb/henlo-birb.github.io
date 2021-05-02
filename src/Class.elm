module Class exposing (..)

import Json.Encode as E exposing (list, string)
import Parser exposing ((|.), (|=), Parser, backtrackable, chompIf, chompUntil, chompWhile, getChompedString, oneOf, spaces, succeed, symbol, token)
import Time exposing (Time)
import Utilities exposing (log, logChomp)


type alias Class =
    { hidden : Bool
    , name : String
    , remoteStatus : String
    , level : String
    , credits : String
    , days : List String
    , startTime : Time
    , endTime : Time
    , termLength : String
    , maxEnrollment : String
    , frequency : String
    , link : String
    }


termStrings =
    [ "Full-Term", "Full-term", "Full Term", "Full term" ]


encode : Class -> List E.Value
encode class =
    List.map
        (\day ->
            E.object
                [ ( "Class Name", string class.name )
                , ( "Remote Status", string class.remoteStatus )
                , ( "Level", string class.level )
                , ( "Credits", string class.credits )
                , ( "Day", string day )
                , ( "Start Time", string <| Time.toString class.startTime )
                , ( "End Time", string <| Time.toString class.endTime )
                , ( "Max Enrollment", string class.maxEnrollment )
                , ( "Frequency", string class.frequency )
                , ( "Link", string class.link )
                ]
        )
        class.days


generateClass : String -> String -> Result (List Parser.DeadEnd) Class
generateClass link html =
    let
        dayParser : Parser String
        dayParser =
            oneOf
                [ getChompedString <| token "M"
                , getChompedString <| token "TBA"
                , Parser.map (always "T") <| token "Tu"
                , getChompedString <| token "Th"
                , getChompedString <| token "T"
                , getChompedString <| token "W"
                , getChompedString <| token "F"
                ]

        chompThrough : String -> Parser ()
        chompThrough s =
            succeed ()
                |. chompUntil s
                |. token s

        makeList a b =
            [ a, b ]

        classParser : Parser Class
        classParser =
            succeed Class
                |= succeed False
                |. chompThrough "<h1 class=\"entry-title\">\n"
                |= (chompUntil "\n" |> getChompedString |> Parser.map String.trim)
                |. chompUntil "Delivery Method: "
                |= oneOf
                    [ succeed identity
                        |. chompUntil "ybrid"
                        |= succeed "Hybrid"
                    , succeed identity
                        |. chompUntil "emote"
                        |= succeed "Remotely Accessible"
                    , succeed identity
                        |. chompUntil "in-person"
                        |= succeed "In Person"
                    , succeed "Unknown"
                    ]
                |. chompThrough "Course Level: "
                |= Parser.map String.fromInt Parser.int
                |. chompThrough "Credits: "
                |= Parser.map String.fromInt Parser.int
                |. chompWhile (\c -> not (String.contains (String.fromChar c) "MTWF"))
                |= oneOf
                    [ succeed makeList
                        |= backtrackable dayParser
                        |. backtrackable (chompWhile (\c -> List.member c [ ',', '/', ' ' ]))
                        |= dayParser
                    , succeed List.singleton
                        |= dayParser
                    , succeed [ "Unknown" ]
                    ]
                |. chompIf (\c -> List.member c [ ',', '/', ' ' ])
                |. spaces
                |= Time.timeParser
                |. spaces
                |. oneOf [ symbol "-", succeed () ]
                |. spaces
                |= Time.timeParser
                |= oneOf
                    [ succeed "Full Term" |. oneOf (List.map chompUntil termStrings)
                    , succeed "1st 7 Weeks" |. backtrackable (chompUntil "1st") |. chompUntil "eeks"
                    , succeed "2nd 7 Weeks" |. backtrackable (chompUntil "2nd") |. chompUntil "eeks"
                    , succeed "1st Module Block" |. chompUntil "1st" |. chompUntil "odule"
                    , succeed "2nd Module Block" |. chompUntil "2nd" |. chompUntil "odule"
                    , succeed "3rd Module Block" |. chompUntil "3rd" |. chompUntil "odule"
                    , succeed "4th Module Block" |. chompUntil "4th" |. chompUntil "odule"
                    , succeed "Unknown"
                    ]
                |. chompThrough ")"
                |. chompThrough "Maximum Enrollment:"
                |. spaces
                |= Parser.map String.fromInt Parser.int
                |. chompThrough "Course Frequency:"
                |= (chompUntil "<br>" |> getChompedString |> Parser.map String.trim)
                |= succeed link
    in
    Parser.run classParser html

module Class exposing (..)

import Parser exposing ((|.), (|=), Parser, backtrackable, chompIf, chompUntil, chompWhile, getChompedString, map, oneOf, spaces, succeed, symbol, token)
import Time exposing (Time)
import Utilities exposing (log, logChomp)


type alias Class =
    { hidden : Bool
    , name : String
    , remoteStatus : String
    , level : Int
    , credits : Int
    , days : List String
    , startTime : Time
    , endTime : Time
    , termLength : String
    , frequency : String
    , link : String
    }


generateClass : String -> String -> Result (List Parser.DeadEnd) Class
generateClass link html =
    let
        --_ =
        --    log html
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
                        |. oneOf [ chompUntil "Hybrid", chompUntil "hybrid" ]
                        |= succeed "Hybrid"
                    , succeed identity
                        |. oneOf [ chompUntil "Remote", chompUntil "remote" ]
                        |. chompUntil "emote"
                        |= succeed "Remote"
                    , succeed identity
                        |. chompUntil "in-person"
                        |= succeed "In Person"
                    , succeed "Unknown"
                    ]
                |. chompThrough "Course Level: "
                |= Parser.int
                |. chompThrough "Credits: "
                |= Parser.int
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
                |. spaces
                |. chompUntil "("
                |= oneOf
                    [ succeed "Full Term" |. chompUntil "Full"
                    , succeed "1st 7 Weeks" |. chompUntil "1st"
                    , succeed "2nd 7 weeks" |. chompUntil "2nd"
                    ]
                |. chompUntil ")"
                |. chompThrough "Course Frequency: "
                |= (chompUntil "<br>" |> getChompedString |> Parser.map String.trim)
                |= succeed link
    in
    Parser.run classParser html

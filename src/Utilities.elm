module Utilities exposing (..)

import Maybe.Extra
import Parser exposing ((|=), Parser, getChompedString, succeed)


ternary : Bool -> a -> a -> a
ternary cond a b =
    if cond then
        a

    else
        b


log =
    Debug.log ""


logChomp : String -> Parser a -> Parser String
logChomp a chomp =
    succeed (Debug.log a)
        |= getChompedString chomp


unwrapMap =
    Maybe.Extra.unwrap


unwrap : a -> Maybe a -> a
unwrap a =
    unwrapMap a identity

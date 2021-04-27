module Utilities exposing (..)

import AllDict
import Maybe
import Maybe.Extra
import Parser exposing ((|=), Parser, getChompedString, succeed)


ternary : Bool -> a -> a -> a
ternary cond a b =
    if cond then
        a

    else
        b


log =
    identity



--Debug.log ""


logChomp : String -> Parser a -> Parser String
logChomp a chomp =
    succeed log
        |= getChompedString chomp


unwrapMap =
    Maybe.Extra.unwrap


unwrap : a -> Maybe a -> a
unwrap a =
    unwrapMap a identity


get : k -> AllDict.Dict k (List v) -> List v
get key dict =
    Maybe.withDefault [] <| AllDict.get key dict

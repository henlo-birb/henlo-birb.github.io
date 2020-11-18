module Utilities exposing (..)

import Maybe.Extra


ternary : Bool -> a -> a -> a
ternary cond a b =
    if cond then
        a

    else
        b


log =
    identity



--Debug.log ""


unwrapMap =
    Maybe.Extra.unwrap


unwrap : a -> Maybe a -> a
unwrap a =
    unwrapMap a identity

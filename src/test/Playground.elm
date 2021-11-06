module Playground exposing (..)

import Desugar exposing (desugar)
import Match as M
import Parse exposing (parse)
import RegExp as R
import Scan exposing (scan)
import Derivative as D

exp r =
    case Result.andThen (\x -> Ok <| desugar x) <| (Result.andThen parse <| scan r) of
        Ok v ->
            v

        _ ->
            R.Phi


match s r =
    M.match s (exp r)


loop f a n =
    if n == 0 then
        f a

    else
        let
            _ =
                f a
        in
        loop f a (n - 1)



{-

import Playground exposing(..)
import RegExp as R
import Scan as S
import Parse as P
import Derivative as D
Result.andThen P.parse <| S.scan "exp"

-}

nth r a n = 
    if n == 0 then
        r
    else 
        nth (D.derive r [a]) a (n-1)
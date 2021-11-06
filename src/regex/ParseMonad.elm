module ParseMonad exposing (ParseMonad(..), bind, result, return)

import Error as E
import RegExp as R


type ParseMonad a
    = Failure E.SyntaxError
    | Success a


return t =
    Success ( R.Phi, t )


bind : (a -> ParseMonad b) -> ParseMonad a -> ParseMonad b
bind f m =
    case m of
        Failure e ->
            Failure e

        Success v ->
            f v


result m =
    case m of
        Success v ->
            Ok v

        Failure e ->
            Err <| E.SyntaxError e

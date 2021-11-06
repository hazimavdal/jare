module Token exposing
    ( Token(..)
    , toChar
    , toString
    )

import Consts


type Token
    = Symbol Char
    | RParen
    | LParen
    | RCurly
    | LCurly
    | LBrack
    | RBrack
    | Caret
    | Comma
    | Plus
    | Amp
    | Pipe
    | Star
    | Epsilon
    | Phi
    | Dot
    | Query
    | Tilde
    | Slash
    | Dash


toChar t =
    case t of
        Symbol a ->
            a

        LParen ->
            '('

        RParen ->
            ')'

        LCurly ->
            '{'

        RCurly ->
            '}'

        LBrack ->
            '['

        RBrack ->
            ']'

        Caret ->
            '^'

        Comma ->
            ','

        Plus ->
            '+'

        Amp ->
            '&'

        Pipe ->
            '|'

        Star ->
            '*'

        Epsilon ->
            Consts.epsilon

        Phi ->
            Consts.phi

        Dot ->
            '.'

        Query ->
            '?'

        Tilde ->
            '~'

        Slash ->
            '\\'

        Dash ->
            '-'


toString tl =
    case tl of
        [] ->
            ""

        x :: xs ->
            (String.fromChar <| toChar x)
                ++ toString xs

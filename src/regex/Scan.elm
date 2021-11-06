module Scan exposing (scan)

import Consts
import Error as E
import Token as T


slashable a =
    List.member a Consts.reservedTokens
        || List.member a Consts.charClasses


next xl =
    case xl of
        [] ->
            Ok ( Nothing, [] )

        '\\' :: [] ->
            Err <| E.ScanError <| E.UnexpectedEOF

        '\\' :: a :: xs ->
            if slashable a then
                Ok ( Just T.Slash, a :: xs )

            else
                Err <| E.ScanError <| E.UnknownEscape a

        '(' :: xs ->
            Ok ( Just <| T.LParen, xs )

        ')' :: xs ->
            Ok ( Just <| T.RParen, xs )

        '{' :: xs ->
            Ok ( Just <| T.LCurly, xs )

        '}' :: xs ->
            Ok ( Just <| T.RCurly, xs )

        '[' :: xs ->
            Ok ( Just <| T.LBrack, xs )

        ']' :: xs ->
            Ok ( Just <| T.RBrack, xs )

        '^' :: xs ->
            Ok ( Just <| T.Caret, xs )

        ',' :: xs ->
            Ok ( Just <| T.Comma, xs )

        '?' :: xs ->
            Ok ( Just <| T.Query, xs )

        '~' :: xs ->
            Ok ( Just <| T.Tilde, xs )

        '+' :: xs ->
            Ok ( Just <| T.Plus, xs )

        '&' :: xs ->
            Ok ( Just <| T.Amp, xs )

        '|' :: xs ->
            Ok ( Just <| T.Pipe, xs )

        '*' :: xs ->
            Ok ( Just <| T.Star, xs )

        '.' :: xs ->
            Ok ( Just <| T.Dot, xs )

        'ε' :: xs ->
            Ok ( Just <| T.Epsilon, xs )

        '∅' :: xs ->
            Ok ( Just <| T.Phi, xs )

        '-' :: xs ->
            Ok ( Just <| T.Dash, xs )

        a :: xs ->
            Ok ( Just <| T.Symbol a, xs )


scan : String -> Result E.Error (List T.Token)
scan text =
    let
        lp xs =
            case xs of
                [] ->
                    Ok []

                _ ->
                    case next xs of
                        Ok ( Just tok, cs ) ->
                            Result.andThen (\x -> Ok <| tok :: x) (lp cs)

                        Err e ->
                            Err e

                        Ok ( Nothing, _ ) ->
                            Ok []
    in
    lp <| String.toList text

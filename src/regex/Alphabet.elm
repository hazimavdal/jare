module Alphabet exposing
    ( Class(..)
    , any
    , member
    , prune
    , toString
    )

import Set exposing (Set)


type Class
    = Any
    | Lits (Set Char)
    | Num
    | NotNum
    | Alpha
    | NotAlpha
    | AlphaNum
    | NotAlphaNum
    | Whitespace
    | NotWhitespace
    | Range Char Char


range min c max =
    let
        q =
            Char.toCode c
    in
    min <= q && q <= max


member a c =
    case c of
        Any ->
            True

        Lits bs ->
            Set.member a bs

        Num ->
            range 48 a 57

        NotNum ->
            not <| range 48 a 57

        Alpha ->
            range 65 a 122

        NotAlpha ->
            not <| range 65 a 122

        AlphaNum ->
            range 65 a 122 || range 48 a 57

        NotAlphaNum ->
            not <| range 65 a 122 || range 48 a 57

        Whitespace ->
            List.member (Char.toCode a) [ 9, 10, 32 ]

        NotWhitespace ->
            not <| List.member (Char.toCode a) [ 9, 10, 32 ]

        Range min max ->
            range (Char.toCode min) a (Char.toCode max)


any_ a xl res =
    if res then
        True

    else
        case xl of
            [] ->
                False

            x :: xs ->
                any_ a xs (member a x)


any a xl =
    any_ a xl False


toString a =
    case a of
        Any ->
            "."

        Lits cs ->
            case Set.toList cs of
                [] ->
                    ""

                [ c ] ->
                    String.fromChar c

                xl ->
                    "[" ++ String.fromList xl ++ "]"

        Num ->
            "\\d"

        NotNum ->
            "\\D"

        Alpha ->
            "TODO: this is not standard"

        NotAlpha ->
            "TODO: this is not standard"

        AlphaNum ->
            "\\w"

        NotAlphaNum ->
            "\\W"

        Whitespace ->
            "\\s"

        NotWhitespace ->
            "\\S"

        Range min max ->
            String.fromChar min
                ++ "-"
                ++ String.fromChar max


mem : List (List Class) -> List Class -> Bool
mem xl ys =
    case xl of
        [] ->
            False

        x :: xs ->
            List.all (\y -> List.member y ys) x
                || mem xs ys


isWild xl =
    mem
        [ [ Any ]
        , [ Num, NotNum ]
        , [ Alpha, NotAlpha ]
        , [ AlphaNum, NotAlphaNum ]
        , [ Whitespace, NotWhitespace ]
        ]
        xl


prune : List Class -> List Class
prune a =
    case a of
        [ _ ] ->
            a

        _ ->
            if isWild a then
                [ Any ]

            else
                a

module Util exposing
    ( brackWrap
    , curlyWrap
    , intToChar
    , parenWrap
    , parseDigit
    , intersperse
    )


intToChar n =
    case n of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        _ ->
            '�'


parseDigit n =
    if Char.isDigit n then
        Just <| Char.toCode n - 48

    else
        Nothing


parenWrap : String -> String
parenWrap x =
    "(" ++ x ++ ")"


curlyWrap : String -> String
curlyWrap x =
    "{" ++ x ++ "}"


brackWrap : String -> String
brackWrap x =
    "[" ++ x ++ "]"

intersperse xl yl = case (xl, yl) of
    ([], []) -> ""
    (x::xs, []) -> x ++ intersperse xs []
    ([], y::ys) -> y ++ intersperse [] ys
    (x::xs, y::ys) -> y ++ x ++ intersperse xs ys
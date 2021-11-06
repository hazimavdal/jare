module Error exposing
    ( Error(..)
    , ScanError(..)
    , SyntaxError(..)
    , toString
    )


type ScanError
    = UnknownEscape Char
    | UnexpectedEOF


type SyntaxError
    = MultipleEpressions String
    | UnexpectedTokens String
    | MissingToken Char
    | MissingCount
    | MissingOperand Char
    | MissingRangeStart
    | MissingRangeEnd
    | InvalidRange
    | InvalidUpperBound
    | InvalidSeq
    | ExpectedExpression Char


type Error
    = ScanError ScanError
    | SyntaxError SyntaxError


scanWrap s =
    "Scan Error: " ++ s


syntaxWrap s =
    "Syntax Error: " ++ s


escapeWrap c =
    scanWrap <|
        "'"
            ++ String.fromChar c
            ++ "' is not a valid escape sequence."


toString : Error -> String
toString e =
    case e of
        ScanError (UnknownEscape c) ->
            escapeWrap c

        ScanError UnexpectedEOF ->
            scanWrap <| "unexpected end of expression."

        SyntaxError (MultipleEpressions s) ->
            syntaxWrap <| "'" ++ s ++ "' inalid tokens after main expression."

        SyntaxError (UnexpectedTokens t) ->
            syntaxWrap <| "I was not expecting to see '" ++ t ++ "'."

        SyntaxError (MissingToken c) ->
            syntaxWrap <| "I was expecting to see a '" ++ String.fromChar c ++ "'."

        SyntaxError MissingCount ->
            syntaxWrap <| "This is an invalid range. Try something like {1,4}."

        SyntaxError (MissingOperand c) ->
            syntaxWrap <| "operator '" ++ String.fromChar c ++ "' requires a second operand."

        SyntaxError MissingRangeStart ->
            syntaxWrap <| "missing range head"

        SyntaxError MissingRangeEnd ->
            syntaxWrap <| "missing range tail"

        SyntaxError InvalidRange ->
            syntaxWrap <| "invalid range"

        SyntaxError InvalidSeq ->
            syntaxWrap <| "invalid character sequence"

        SyntaxError (ExpectedExpression c) ->
            syntaxWrap <| "I expected to see an expression after " ++ String.fromChar c

        SyntaxError InvalidUpperBound ->
            syntaxWrap <| "the upper bound must be greater than or equal to the lower bound"

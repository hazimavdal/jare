module Parse exposing (parse)

import Alphabet as A
import Error as E
import ParseMonad as M
import Set exposing (Set)
import SugarRegExp as R
import Token as T
import Util


type alias Step =
    ( R.SugarRegExp, List T.Token )


toInt : List Int -> Int -> Int -> Int
toInt xl i n =
    case xl of
        [] ->
            n

        x :: xs ->
            toInt xs (i + 1) (n + x * 10 ^ i)


parseDigits ts acc =
    case ts of
        (T.Symbol a) :: xs ->
            let
                n =
                    Util.parseDigit a
            in
            case n of
                Just n_ ->
                    parseDigits xs <| n_ :: acc

                _ ->
                    ( acc, ts )

        _ ->
            ( acc, ts )


parseSeq :
    List T.Token
    -> Set Char
    -> List A.Class
    -> M.ParseMonad ( Set Char, List A.Class, List T.Token )
parseSeq tl lits ms =
    case tl of
        [] ->
            M.Success ( lits, ms, [] )

        T.RBrack :: _ ->
            M.Success ( lits, ms, tl )

        (T.Symbol a) :: T.Dash :: (T.Symbol b) :: ts ->
            parseSeq ts lits <| A.Range a b :: ms

        T.Dash :: [] ->
            M.Failure E.InvalidRange

        T.Dash :: _ ->
            M.Failure E.MissingRangeStart

        _ :: T.Dash :: _ ->
            M.Failure E.MissingRangeEnd

        (T.Symbol a) :: ts ->
            parseSeq ts (Set.insert a lits) ms

        T.Slash :: (T.Symbol 'd') :: ts ->
            parseSeq ts lits <| A.Num :: ms

        T.Slash :: (T.Symbol 'D') :: ts ->
            parseSeq ts lits <| A.NotNum :: ms

        T.Slash :: (T.Symbol 'w') :: ts ->
            parseSeq ts lits <| A.AlphaNum :: ms

        T.Slash :: (T.Symbol 'W') :: ts ->
            parseSeq ts lits <| A.NotAlphaNum :: ms

        T.Slash :: (T.Symbol 's') :: ts ->
            parseSeq ts lits <| A.Whitespace :: ms

        T.Slash :: (T.Symbol 'S') :: ts ->
            parseSeq ts lits <| A.NotWhitespace :: ms

        t :: ts ->
            parseSeq ts (Set.insert (T.toChar t) lits) ms


seq : Step -> M.ParseMonad Step
seq ( pr, tl ) =
    M.bind
        (\( lits, cs, ts ) ->
            case ts of
                T.RBrack :: ts1 ->
                    case pr of
                        R.Not _ ->
                            M.Success
                                ( R.Not <| R.Atoms <| A.Lits lits :: cs, ts1 )

                        _ ->
                            M.Success
                                ( R.Atoms <| A.Lits lits :: cs, ts1 )

                _ ->
                    M.Failure <| E.MissingToken ']'
        )
    <|
        parseSeq tl Set.empty []


parseSlash : Step -> M.ParseMonad Step
parseSlash ( _, tl ) =
    case tl of
        [] ->
            M.Failure <| E.UnexpectedTokens "\\"

        (T.Symbol 'd') :: ts ->
            M.Success <| ( R.Atoms [ A.Num ], ts )

        (T.Symbol 'D') :: ts ->
            M.Success <| ( R.Atoms [ A.NotNum ], ts )

        (T.Symbol 'w') :: ts ->
            M.Success <| ( R.Atoms [ A.AlphaNum ], ts )

        (T.Symbol 'W') :: ts ->
            M.Success <| ( R.Atoms [ A.NotAlphaNum ], ts )

        (T.Symbol 's') :: ts ->
            M.Success <| ( R.Atoms [ A.Whitespace ], ts )

        (T.Symbol 'S') :: ts ->
            M.Success <| ( R.Atoms [ A.NotWhitespace ], ts )

        t :: ts ->
            M.Success ( R.Atoms [ A.Lits <| Set.fromList [ T.toChar t ] ], ts )


validBound a b =
    case ( a, b ) of
        ( Just a_, Just b_ ) ->
            a_ <= b_

        _ ->
            True


parseRange : Step -> M.ParseMonad Step
parseRange ( r, ts ) =
    case ts of
        T.LCurly :: T.RCurly :: _ ->
            M.Failure <| E.MissingCount

        T.LCurly :: ts1 ->
            let
                ( md, ts2 ) =
                    parseDigits ts1 []

                min =
                    if md == [] then
                        Nothing

                    else
                        Just <| toInt md 0 0
            in
            case ts2 of
                T.Comma :: ts3 ->
                    let
                        ( md2, ts4 ) =
                            parseDigits ts3 []

                        max =
                            if md2 == [] then
                                Nothing

                            else
                                Just <| toInt md2 0 0
                    in
                    if not (validBound min max) then
                        M.Failure E.InvalidUpperBound

                    else
                        case ts4 of
                            T.RCurly :: ts5 ->
                                M.Success ( R.Repeat r min max, ts5 )

                            a :: _ ->
                                M.Failure <| E.UnexpectedTokens (T.toString [ a ])

                            _ ->
                                M.Failure <| E.MissingToken '}'

                T.RCurly :: ts4 ->
                    M.Success ( R.Repeat r min min, ts4 )

                a :: _ ->
                    M.Failure <| E.UnexpectedTokens (T.toString [ a ])

                _ ->
                    M.Failure <| E.MissingToken '}'

        _ ->
            M.Failure <| E.MissingToken '{'


parseBi : Step -> M.ParseMonad Step
parseBi ( r, ts1 ) =
    case ts1 of
        T.Pipe :: [] ->
            M.Failure <| E.MissingOperand '|'

        T.Amp :: [] ->
            M.Failure <| E.MissingOperand '&'

        T.Pipe :: ts2 ->
            M.bind
                (\( r1, ts3 ) -> M.Success ( R.Or r r1, ts3 ))
                (doBi ( r, ts2 ))

        T.Amp :: ts2 ->
            M.bind
                (\( r1, ts3 ) -> M.Success ( R.And r r1, ts3 ))
                (doBi ( r, ts2 ))

        _ ->
            M.Success ( r, ts1 )


parseKleene : Step -> M.ParseMonad Step
parseKleene ( r, ts1 ) =
    case ts1 of
        T.Star :: T.Star :: ts2 ->
            M.Success ( R.Star r, ts2 )

        T.Star :: T.Query :: ts2 ->
            M.Success ( R.Star r, ts2 )

        T.Star :: ts2 ->
            M.Success ( R.Star r, ts2 )

        T.Plus :: T.Query :: ts2 ->
            M.Success ( R.Star r, ts2 )

        T.Plus :: ts2 ->
            M.Success ( R.Must r, ts2 )

        T.Query :: T.Plus :: ts2 ->
            M.Success ( R.Star r, ts2 )

        T.Query :: T.Star :: ts2 ->
            M.Success ( R.Star r, ts2 )

        T.Query :: ts2 ->
            M.Success ( R.Optional r, ts2 )

        T.LCurly :: _ ->
            parseRange ( r, ts1 )

        _ ->
            M.Success ( r, ts1 )


parseTerm : Step -> M.ParseMonad Step
parseTerm ( r, ts1 ) =
    case ts1 of
        [] ->
            M.Success ( r, ts1 )

        T.Amp :: ts2 ->
            M.Success ( r, T.Amp :: ts2 )

        T.Pipe :: ts2 ->
            M.Success ( r, T.Pipe :: ts2 )

        T.RParen :: ts2 ->
            M.Success ( r, T.RParen :: ts2 )

        _ ->
            M.bind
                (\( r1, ts2 ) -> M.Success ( R.Concat r r1, ts2 ))
            <|
                term ( r, ts1 )


symb a =
    R.Atoms [ A.Lits <| Set.singleton a ]


parseAtomic : Step -> M.ParseMonad Step
parseAtomic ( prevR, tl ) =
    case tl of
        [] ->
            M.Success ( prevR, tl )

        T.Phi :: ts ->
            M.Success ( R.Phi, ts )

        T.Epsilon :: ts ->
            M.Success ( R.Epsilon, ts )

        T.Dot :: ts ->
            M.Success ( R.Atoms [ A.Any ], ts )

        (T.Symbol a) :: ts ->
            M.Success ( symb a, ts )

        T.LCurly :: _ ->
            parseRange ( prevR, tl )

        T.LBrack :: T.RBrack :: ts ->
            M.Success ( R.Epsilon, ts )

        T.LBrack :: T.Caret :: ts ->
            seq ( R.Not R.Epsilon, ts )

        T.LBrack :: ts ->
            seq ( R.Phi, ts )

        T.LParen :: T.RParen :: ts ->
            M.Success ( R.Phi, ts )

        T.LParen :: ts ->
            M.bind
                (\( r, ts1 ) ->
                    case ts1 of
                        T.RParen :: ts2 ->
                            M.Success ( r, ts2 )

                        _ ->
                            M.Failure <| E.MissingToken ')'
                )
                (doBi ( prevR, ts ))

        T.Tilde :: [] ->
            M.Failure <| E.ExpectedExpression '~'

        T.Tilde :: ts ->
            M.bind
                (\( r, ts1 ) -> M.Success ( R.Not r, ts1 ))
                (doBi ( prevR, ts ))

        T.Slash :: ts ->
            parseSlash ( prevR, ts )

        _ ->
            M.Failure <| E.UnexpectedTokens (T.toString tl)


doBi : Step -> M.ParseMonad Step
doBi ts =
    M.bind parseBi (term ts)


term : Step -> M.ParseMonad Step
term ts =
    M.bind parseTerm (kleene ts)


kleene : Step -> M.ParseMonad Step
kleene ts =
    M.bind parseKleene (parseAtomic ts)


doParse ( r, ts ) =
    case ts of
        [] ->
            M.Success r

        ts1 ->
            M.Failure <| E.MultipleEpressions (T.toString ts1)


parse : List T.Token -> Result E.Error R.SugarRegExp
parse r =
    M.result <| M.bind doParse (doBi ( R.Phi, r ))

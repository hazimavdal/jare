module RegExp exposing
    ( RegExp(..)
    , delta
    , nullable
    , prune
    , reverse
    , toString
    )

import Alphabet as A
import Consts
import Util as U


type RegExp
    = Epsilon
    | Phi
    | Atoms (List A.Class)
    | Or RegExp RegExp
    | And RegExp RegExp
    | Concat RegExp RegExp
    | Not RegExp
    | Star RegExp
    | Repeat RegExp (Maybe Int) (Maybe Int)


nullable : RegExp -> Bool
nullable r =
    case r of
        Epsilon ->
            True

        Star _ ->
            True

        And r1 r2 ->
            nullable r1 && nullable r2

        Or r1 r2 ->
            nullable r1 || nullable r2

        Concat r1 r2 ->
            nullable r1 && nullable r2

        Not r1 ->
            not <| nullable r1

        Repeat _ Nothing (Just n) ->
            n >= 0

        Repeat _ (Just 0) _ ->
            True

        _ ->
            False


delta r =
    if nullable r then
        Epsilon

    else
        Phi


toString r =
    case r of
        Epsilon ->
            String.fromChar Consts.epsilon

        Phi ->
            String.fromChar Consts.phi

        Atoms (cs :: []) ->
            A.toString cs

        Atoms cs ->
            U.brackWrap <|
                String.join "" <|
                    List.map A.toString cs

        And r1 r2 ->
            U.intersperse
                [ toString r1, toString r2 ]
                [ "(", "&", ")" ]

        Or r1 r2 ->
            U.intersperse
                [ toString r1, toString r2 ]
                [ "(", "|", ")" ]

        Concat r1 r2 ->
            toString r1 ++ toString r2

        Not r1 ->
            "~" ++ (U.parenWrap <| toString r1)

        Star (Atoms [ A.Any ]) ->
            ".*"

        Star (Atoms cs) ->
            toString (Atoms cs) ++ "*"

        Star r1 ->
            U.intersperse
                [ toString r1 ]
                [ "(", ")", "*" ]

        Repeat r1 min max ->
            case ( min, max ) of
                ( Nothing, Nothing ) ->
                    (U.parenWrap <| toString r1) ++ "*"

                ( Just m, Nothing ) ->
                    (U.parenWrap <| toString r1)
                        ++ (U.curlyWrap <| String.fromInt m ++ ",")

                ( Nothing, Just n ) ->
                    (U.parenWrap <| toString r1)
                        ++ (U.curlyWrap <| "," ++ String.fromInt n)

                ( Just m, Just n ) ->
                    (U.parenWrap <| toString r1)
                        ++ (U.curlyWrap <| String.fromInt m ++ "," ++ String.fromInt n)


combine :
    (RegExp -> RegExp -> RegExp)
    -> RegExp
    -> RegExp
    -> RegExp
    -> RegExp
combine op r1 r2 r3 =
    let
        pr1 =
            prune r1

        pr2 =
            prune r2

        pr3 =
            prune r3
    in
    case ( pr1 == pr2, pr2 == pr3 ) of
        ( True, True ) ->
            pr1

        ( True, False ) ->
            op pr1 pr3

        ( False, True ) ->
            op pr1 pr2

        ( False, False ) ->
            if pr1 == pr3 then
                op pr1 pr2

            else
                op pr1 (op pr2 pr3)


tailEliminate r =
    case r of
        Or (Or r1 r2) r3 ->
            combine Or r1 r2 r3

        Or r1 (Or r2 r3) ->
            combine Or r1 r2 r3

        And r1 (And r2 r3) ->
            combine And r1 r2 r3

        And (And r1 r2) r3 ->
            combine And r1 r2 r3

        _ ->
            r


pruneAnd r1 r2 =
    case ( r1, r2 ) of
        ( _, Phi ) ->
            Phi

        ( Phi, _ ) ->
            Phi

        ( Epsilon, _ ) ->
            delta r2

        ( _, Epsilon ) ->
            delta r1

        _ ->
            tailEliminate <| And r1 r2


pruneOr r1 r2 =
    case ( r1, r2 ) of
        ( Epsilon, Phi ) ->
            Epsilon

        ( Phi, Epsilon ) ->
            Epsilon

        ( Phi, _ ) ->
            r2

        ( _, Phi ) ->
            r1

        ( Atoms [ A.Any ], Atoms _ ) ->
            Atoms [ A.Any ]

        ( Atoms _, Atoms [ A.Any ] ) ->
            Atoms [ A.Any ]

        ( Star (Atoms [ A.Any ]), _ ) ->
            Star (Atoms [ A.Any ])

        ( _, Star (Atoms [ A.Any ]) ) ->
            Star (Atoms [ A.Any ])

        ( _, Star r3 ) ->
            if r1 == r3 then
                r2

            else
                Or r1 r2

        ( Star r3, _ ) ->
            if r2 == r3 then
                r1

            else
                Or r1 r2

        _ ->
            tailEliminate <| Or r1 r2


pruneConcat r1 r2 =
    case ( r1, r2 ) of
        ( Phi, _ ) ->
            Phi

        ( _, Phi ) ->
            Phi

        ( Epsilon, Epsilon ) ->
            Epsilon

        ( Epsilon, _ ) ->
            r2

        ( _, Epsilon ) ->
            r1

        ( Star (Atoms [ A.Any ]), Concat (Star (Atoms [ A.Any ])) r2_ ) ->
            Concat (Star <| Atoms [ A.Any ]) r2_

        ( Star r1_, Star r2_ ) ->
            let
                pr1 =
                    prune r1_

                pr2 =
                    prune r2_
            in
            if pr1 == pr2 then
                Star pr1

            else
                Concat (Star pr1) (Star pr2)

        ( Star r1_, Repeat (Star r2_) min max ) ->
            let
                pr1 =
                    prune <| Star r1_

                pr2 =
                    prune <| Star r2_
            in
            if pr1 == pr2 then
                Star pr1

            else
                Concat pr1 (Repeat pr2 min max)

        _ ->
            Concat r1 r2


pruneStar r =
    case r of
        Epsilon ->
            Epsilon

        Phi ->
            Epsilon

        Star r1 ->
            Star <| prune r1

        _ ->
            Star r


pruneNot r =
    case r of
        Not (Not r2) ->
            prune r2

        Phi ->
            Star <| Atoms [ A.Any ]

        Epsilon ->
            Concat (Atoms [ A.Any ])
                (Star <| Atoms [ A.Any ])

        Star (Atoms [ A.Any ]) ->
            Phi

        _ ->
            Not r


pruneRepeat r min max =
    case ( min, max ) of
        ( Nothing, Nothing ) ->
            prune <| Star r

        ( Just m, Just n ) ->
            if m > n then
                Phi

            else if m == 0 && n == 0 then
                Epsilon

            else if m == 1 && n == 1 then
                prune r

            else
                Repeat (prune r) min max

        _ ->
            Repeat (prune r) min max


prune r =
    case r of
        And r1 r2 ->
            pruneAnd (prune r1) (prune r2)

        Or r1 r2 ->
            pruneOr (prune r1) (prune r2)

        Concat r1 r2 ->
            pruneConcat (prune r1) (prune r2)

        Not r1 ->
            pruneNot (prune r1)

        Star r1 ->
            pruneStar (prune r1)

        Atoms cs ->
            Atoms <| A.prune cs

        Repeat r1 min max ->
            pruneRepeat r1 min max

        _ ->
            r


reverse r =
    case r of
        And r1 r2 ->
            And (reverse r1) (reverse r2)

        Or r1 r2 ->
            Or (reverse r1) (reverse r2)

        Concat r1 r2 ->
            Concat (reverse r2) (reverse r1)

        Not r1 ->
            Not (reverse r1)

        Star r1 ->
            Star (reverse r1)

        Repeat r1 min max ->
            Repeat (reverse r1) min max

        _ ->
            r

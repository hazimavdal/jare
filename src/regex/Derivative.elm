module Derivative exposing (derive)

import Alphabet as A
import RegExp as R


relu x =
    if x < 1 then
        0

    else
        x - 1


drep x r min max =
    case ( min, max ) of
        ( Nothing, Nothing ) ->
            brzozowski x <| R.Star r

        ( Just m, Nothing ) ->
            R.Concat (brzozowski x r) (R.Repeat r (Just <| relu m) Nothing)

        ( Nothing, Just n ) ->
            R.Concat (brzozowski x r) (R.Repeat r Nothing (Just <| n - 1))

        ( Just m, Just n ) ->
            R.Concat (brzozowski x r) (R.Repeat r (Just <| relu m) (Just <| n - 1))


brzozowski x r =
    case ( x, r ) of
        ( a, R.Atoms cs ) ->
            if A.any a cs then
                R.Epsilon

            else
                R.Phi

        ( _, R.Epsilon ) ->
            R.Phi

        ( _, R.Phi ) ->
            R.Phi

        ( a, R.And r1 r2 ) ->
            R.And (brzozowski a r1) (brzozowski a r2)

        ( a, R.Or r1 r2 ) ->
            R.Or (brzozowski a r1) (brzozowski a r2)

        ( a, R.Concat r1 r2 ) ->
            let
                right =
                    if R.nullable r1 then
                        brzozowski a r2

                    else
                        R.Phi
            in
            R.Or (R.Concat (brzozowski a r1) r2) right

        ( a, R.Not r1 ) ->
            R.Not (brzozowski a r1)

        ( a, R.Star r1 ) ->
            R.Concat (brzozowski a r1) (R.Star r1)

        ( a, R.Repeat r1 min max ) ->
            drep a r1 min max


derive : R.RegExp -> List Char -> R.RegExp
derive r u =
    case ( r, u ) of
        ( R.Phi, _ ) ->
            R.Phi

        ( _, [] ) ->
            r

        ( R.Epsilon, _ ) ->
            R.Phi

        ( _, x :: xs ) ->
            derive (R.prune <| brzozowski x r) xs

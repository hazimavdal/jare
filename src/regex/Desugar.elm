module Desugar exposing (desugar)

import RegExp as R
import SugarRegExp as S


desugar s =
    case s of
        S.Epsilon ->
            R.Epsilon

        S.Phi ->
            R.Phi

        S.And r1 r2 ->
            R.And (desugar r1) (desugar r2)

        S.Or r1 r2 ->
            R.Or (desugar r1) (desugar r2)

        S.Concat r1 r2 ->
            R.Concat (desugar r1) (desugar r2)

        S.Not r1 ->
            R.Not (desugar r1)

        S.Star r1 ->
            R.Star (desugar r1)

        S.Optional r1 ->
            R.Or (desugar r1) R.Epsilon

        S.Must r1 ->
            let
                r =
                    desugar r1
            in
            R.Concat r (R.Star r)

        S.Repeat r1 min max ->
            R.Repeat (desugar r1) min max

        S.Atoms cs ->
            R.Atoms cs

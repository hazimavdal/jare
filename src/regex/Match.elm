module Match exposing (match)

import Derivative as D
import RegExp as R


match s r =
    String.toList s
        |> D.derive r
        |> R.delta
        |> R.prune
        |> (\x -> x == R.Epsilon)

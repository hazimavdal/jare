module SugarRegExp exposing (SugarRegExp(..))

import Alphabet as A


type SugarRegExp
    = Epsilon
    | Phi
    | Atoms (List A.Class)
    | And SugarRegExp SugarRegExp
    | Or SugarRegExp SugarRegExp
    | Concat SugarRegExp SugarRegExp
    | Star SugarRegExp
    | Not SugarRegExp
    | Optional SugarRegExp
    | Must SugarRegExp
    | Repeat SugarRegExp (Maybe Int) (Maybe Int)

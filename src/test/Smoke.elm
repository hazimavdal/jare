module Smoke exposing (..)

import Playground as P

true f a = 
    if not (f a) then
        Debug.todo <| Debug.toString a
    else 
        ()

false f a = 
    if (f a) then
        Debug.todo <| Debug.toString a
    else 
        ()

run = 
    let
        _ = true (P.match "") "[]"
        _ = true (P.match "a") "a"
        _ = true (P.match ".") "."
        _ = true (P.match ".") "\\."
        _ = true (P.match "aabadfadf") ".*"
        _ = false (P.match "a") "aa"
        _ = false (P.match "a") "a&[]"

        _ = true (P.match "b") "[^a]"
        _ = true (P.match "the") "[Tt]he"
        _ = true (P.match "a5c4") "[a-d3-6]*"
        _ = false (P.match "a5c4") "[a-d3-6]"
        _ = false (P.match "a") "[^a]"

        _ = true (P.match "quick ") "quick(ly)?\\s"
        _ = true (P.match "quickly ") "quick(ly)?\\s"
        _ = false (P.match "quick") "quick(ly)?\\s"

        _ = true (P.match "aa") "a{,}"
        _ = false (P.match "aa") "a{,1}"
        _ = true (P.match "aa") "a{,2}"
        _ = true (P.match "aa") "a{,3}"

        _ = true (P.match "aa") "a{0,}"
        _ = true (P.match "aa") "a{1,}"
        _ = true (P.match "aa") "a{2,}"
        _ = false (P.match "aa") "a{3,}"
        
        _ = true (P.match "aa") "a{2,1000}"
        _ = true (P.match "aa") "a{1,1000}"
        _ = true (P.match "aa") "a{0,1000}"
        _ = false (P.match "aa") "a{0,0}"
        _ = true (P.match "aa") "a{2,2}"
        _ = false (P.match "aa") "a{3,3}"
        _ = false (P.match "aa") "a{0,1}"

        _ = true (P.match "aa") "(a*){1,10}"
        _ = true (P.match "aa") "(a*){10000000,2000000000}"
        _ = true (P.match "aa") "(a|b){1,10}"
        _ = true (P.match "aa") "(a|b){,2000000000}"
    in
    "yeet!"
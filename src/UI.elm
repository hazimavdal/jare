port module UI exposing (main)

import Browser
import Desugar as D
import Error as E
import Html exposing (Html)
import Match as M
import Parse as P
import RegExp as R
import Scan as S


type alias Query a =
    ( List ( String, ( Int, Int ) ), a )


type alias StrQuery =
    List ( Bool, ( Int, Int ) )


type alias QueryResult a =
    { ok : Bool
    , errors : List String
    , result : a
    }


port matchQuery : (Query String -> msg) -> Sub msg


port matchResult : QueryResult StrQuery -> Cmd msg


port reverseQuery : (String -> msg) -> Sub msg


port reverseResult : QueryResult String -> Cmd msg


getReverse s =
    case exp s of
        Ok r ->
            { ok = True
            , result = R.toString <| R.reverse r
            , errors = []
            }

        Err e ->
            { ok = False
            , result = ""
            , errors = [ E.toString e ]
            }


exp : String -> Result E.Error R.RegExp
exp s =
    Result.andThen
        (\x -> Ok <| D.desugar x)
    <|
        (Result.andThen P.parse <| S.scan s)


matchMany : Query R.RegExp -> StrQuery -> StrQuery
matchMany xl res =
    case xl of
        ( [], _ ) ->
            res

        ( ( x, ( i, j ) ) :: xs, r ) ->
            matchMany ( xs, r ) 
                (( M.match x r, ( i, j ) )::res)


doMatch : Query String -> QueryResult StrQuery
doMatch xl =
    case xl of
        ( xs, r ) ->
            case exp r of
                Ok r1 ->
                    { ok = True
                    , result = matchMany ( xs, r1 ) []
                    , errors = []
                    }

                Err e ->
                    { ok = False
                    , result = []
                    , errors = [ E.toString e ]
                    }


type alias Model =
    ()


type Msg
    = MatchQuery (Query String)
    | ReverseQuery String


type alias Flags =
    ()


initModel =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ matchQuery MatchQuery
        , reverseQuery ReverseQuery
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MatchQuery q ->
            ( model
            , matchResult <| doMatch q
            )

        ReverseQuery s ->
            ( model, reverseResult <| getReverse s )


view : Model -> Html Msg
view _ =
    Html.div
        []
        []


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

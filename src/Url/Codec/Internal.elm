module Url.Codec.Internal exposing
    ( Parser, ParseError(..)
    , parse
    , urlToInput, pathToInput, constructPath
    , succeed, s, string, int
    , queryInt, queryString, queryInts, queryStrings
    , queryFlag, allQueryFlags
    , fragment
    , listTraverse
    )

{-|

@docs Parser, ParseError
@docs parse
@docs urlToInput, pathToInput, constructPath
@docs succeed, s, string, int
@docs queryInt, queryString, queryInts, queryStrings
@docs queryFlag, allQueryFlags
@docs fragment
@docs listTraverse

-}

import Dict exposing (Dict)
import Set exposing (Set)
import Url exposing (Url)


type alias ParserInput =
    { segments : List String
    , queryParameters : Dict String (List String)
    , queryFlags : Set String
    , fragment : Maybe String
    }


type alias Parser parseResult =
    ParserInput -> Result ( ParseError, Int ) ( parseResult, List String, Int )


type ParseError
    = SegmentMismatch
        { expected : String
        , available : String
        }
    | SegmentNotAvailable
    | WasNotInt String
    | DidNotConsumeEverything (List String)
    | NeededSingleQueryParameterValueGotMultiple
        { key : String
        , got : List String
        }
    | NotAllQueryParameterValuesWereInts
        { key : String
        , got : List String
        }
    | NoParsers


parse_ : Parser a -> ParserInput -> Result ( ParseError, Int ) a
parse_ parser input =
    parser input
        |> Result.andThen checkEmptyLeftovers


{-| Mostly delegates to `parse_` for the actual handling, and deals with the
depth+error juggling (trying all parsers if necessary, reporting the first
success or reporting the deepest error).
-}
parse : List (Parser a) -> ParserInput -> Result ParseError a
parse parsers input =
    case parsers of
        [] ->
            Err NoParsers

        first :: rest ->
            case parse_ first input of
                Ok value ->
                    Ok value

                Err ( firstErr, firstDepth ) ->
                    let
                        go : ( ParseError, Int ) -> List (Parser a) -> Result ParseError a
                        go ( lastErr, lastDepth ) accParsers =
                            case accParsers of
                                [] ->
                                    Err lastErr

                                currentParser :: restParsers ->
                                    case parse_ currentParser input of
                                        Ok value ->
                                            Ok value

                                        Err ( currentErr, currentDepth ) ->
                                            go
                                                (if lastDepth > currentDepth then
                                                    ( lastErr, lastDepth )

                                                 else
                                                    ( currentErr, currentDepth )
                                                )
                                                restParsers
                    in
                    go ( firstErr, firstDepth ) rest


constructPath :
    { a
        | path : String
        , query : Maybe String
        , fragment : Maybe String
    }
    -> String
constructPath url =
    url.path
        ++ (url.query
                |> Maybe.map (\x -> "?" ++ x)
                |> Maybe.withDefault ""
           )
        ++ (url.fragment
                |> Maybe.map (\x -> "#" ++ x)
                |> Maybe.withDefault ""
           )


urlToInput : Url -> ParserInput
urlToInput url =
    url
        |> constructPath
        |> pathToInput


pathToInput : String -> ParserInput
pathToInput path =
    let
        firstSplitBy : String -> String -> String
        firstSplitBy separator input =
            case String.split separator input of
                first :: _ ->
                    first

                [] ->
                    -- never happens
                    input

        pathOnly : String
        pathOnly =
            path
                |> firstSplitBy "?"
                |> firstSplitBy "#"

        ( flags, params ) =
            case String.split "?" path of
                _ :: search :: _ ->
                    search
                        |> String.split "&"
                        |> List.foldl
                            (\pair ( accFlags, accParams ) ->
                                case String.split "=" pair of
                                    -- "x"
                                    [ flag ] ->
                                        ( percentDecode flag :: accFlags
                                        , accParams
                                        )

                                    -- "x="
                                    -- "x=1"
                                    [ key, value ] ->
                                        ( accFlags
                                        , ( percentDecode key
                                          , percentDecode value
                                          )
                                            :: accParams
                                        )

                                    _ ->
                                        -- [] never happens, "x=y=z" etc. is outside spec
                                        ( accFlags, accParams )
                            )
                            ( [], [] )

                _ ->
                    ( [], [] )
    in
    { segments = pathToSegments pathOnly
    , queryParameters =
        params
            |> List.foldl
                (\( key, value ) acc ->
                    Dict.update
                        key
                        (\maybeValues ->
                            case maybeValues of
                                Nothing ->
                                    Just [ value ]

                                Just values ->
                                    Just (value :: values)
                        )
                        acc
                )
                Dict.empty
    , queryFlags = Set.fromList flags
    , fragment =
        case String.split "#" path of
            [ _, fragment_ ] ->
                Just (percentDecode fragment_)

            _ ->
                Nothing
    }


pathToSegments : String -> List String
pathToSegments path =
    path
        |> String.split "/"
        |> removeLeadingEmpty
        |> removeTrailingEmpty
        |> List.map percentDecode


percentDecode : String -> String
percentDecode str =
    str
        |> Url.percentDecode
        |> Maybe.withDefault str


removeLeadingEmpty : List String -> List String
removeLeadingEmpty segments =
    case segments of
        "" :: rest ->
            removeLeadingEmpty rest

        _ ->
            segments


removeTrailingEmpty : List String -> List String
removeTrailingEmpty segments =
    case segments of
        [] ->
            []

        [ "" ] ->
            []

        "" :: rest ->
            if List.all String.isEmpty rest then
                []

            else
                "" :: removeTrailingEmpty rest

        segment :: rest ->
            segment :: removeTrailingEmpty rest


checkEmptyLeftovers : ( parseResult, List String, Int ) -> Result ( ParseError, Int ) parseResult
checkEmptyLeftovers ( value, leftoverSegments, depth ) =
    if List.isEmpty leftoverSegments then
        Ok value

    else
        Err ( DidNotConsumeEverything leftoverSegments, depth )



-- COMBINATORS


succeed : a -> Parser a
succeed thing =
    \{ segments } -> Ok ( thing, segments, 0 )


s : String -> Parser a -> Parser a
s expected innerParser =
    \input ->
        innerParser input
            |> Result.andThen
                (\( thing, segments2, depth ) ->
                    case segments2 of
                        first :: rest ->
                            if first == expected then
                                Ok ( thing, rest, depth + 1 )

                            else
                                Err
                                    ( SegmentMismatch
                                        { expected = expected
                                        , available = first
                                        }
                                    , depth
                                    )

                        [] ->
                            Err ( SegmentNotAvailable, depth )
                )


string : Parser (String -> a) -> Parser a
string innerParser =
    \input ->
        innerParser input
            |> Result.andThen
                (\( stringToThing, segments2, depth ) ->
                    case segments2 of
                        first :: rest ->
                            Ok ( stringToThing first, rest, depth + 1 )

                        [] ->
                            Err ( SegmentNotAvailable, depth )
                )


int : Parser (Int -> a) -> Parser a
int innerParser =
    \input ->
        innerParser input
            |> Result.andThen
                (\( intToThing, segments2, depth ) ->
                    case segments2 of
                        first :: rest ->
                            case String.toInt first of
                                Nothing ->
                                    Err ( WasNotInt first, depth )

                                Just int_ ->
                                    Ok ( intToThing int_, rest, depth + 1 )

                        [] ->
                            Err ( SegmentNotAvailable, depth )
                )


queryInt : String -> Parser (Maybe Int -> a) -> Parser a
queryInt key innerParser =
    \input ->
        innerParser input
            |> Result.andThen
                (\( maybeIntToThing, segments2, depth ) ->
                    case
                        Dict.get key input.queryParameters
                            |> Maybe.withDefault []
                    of
                        [] ->
                            Ok ( maybeIntToThing Nothing, segments2, depth + 1 )

                        [ single ] ->
                            Ok ( maybeIntToThing (String.toInt single), segments2, depth + 1 )

                        many ->
                            Err
                                ( NeededSingleQueryParameterValueGotMultiple
                                    { key = key
                                    , got = many
                                    }
                                , depth
                                )
                )


queryString : String -> Parser (Maybe String -> a) -> Parser a
queryString key innerParser =
    \input ->
        innerParser input
            |> Result.andThen
                (\( maybeStringToThing, segments2, depth ) ->
                    case
                        Dict.get key input.queryParameters
                            |> Maybe.withDefault []
                    of
                        [] ->
                            Ok ( maybeStringToThing Nothing, segments2, depth + 1 )

                        [ single ] ->
                            Ok ( maybeStringToThing (Just single), segments2, depth + 1 )

                        many ->
                            Err
                                ( NeededSingleQueryParameterValueGotMultiple
                                    { key = key
                                    , got = many
                                    }
                                , depth
                                )
                )


queryInts : String -> Parser (List Int -> a) -> Parser a
queryInts key innerParser =
    \input ->
        innerParser input
            |> Result.andThen
                (\( intsToThing, segments2, depth ) ->
                    let
                        strings : List String
                        strings =
                            Dict.get key input.queryParameters
                                |> Maybe.withDefault []

                        ints : Maybe (List Int)
                        ints =
                            listTraverse String.toInt strings
                    in
                    case ints of
                        Nothing ->
                            Err
                                ( NotAllQueryParameterValuesWereInts
                                    { key = key
                                    , got = strings
                                    }
                                , depth
                                )

                        Just ints_ ->
                            Ok ( intsToThing (List.reverse ints_), segments2, depth + 1 )
                )


queryStrings : String -> Parser (List String -> a) -> Parser a
queryStrings key innerParser =
    \input ->
        innerParser input
            |> Result.map
                (\( stringsToThing, segments2, depth ) ->
                    let
                        strings : List String
                        strings =
                            Dict.get key input.queryParameters
                                |> Maybe.withDefault []
                    in
                    ( stringsToThing strings, segments2, depth + 1 )
                )


queryFlag : String -> Parser (Bool -> a) -> Parser a
queryFlag flag innerParser =
    \input ->
        innerParser input
            |> Result.map
                (\( queryFlagToThing, segments2, depth ) ->
                    ( queryFlagToThing (Set.member flag input.queryFlags)
                    , segments2
                    , depth + 1
                    )
                )


allQueryFlags : Parser (List String -> a) -> Parser a
allQueryFlags innerParser =
    \input ->
        innerParser input
            |> Result.map
                (\( queryFlagsToThing, segments2, depth ) ->
                    ( queryFlagsToThing (Set.toList input.queryFlags)
                    , segments2
                    , depth + 1
                    )
                )


fragment : Parser (Maybe String -> a) -> Parser a
fragment innerParser =
    \input ->
        innerParser input
            |> Result.map
                (\( fragmentToThing, segments2, depth ) ->
                    ( fragmentToThing input.fragment
                    , segments2
                    , depth + 1
                    )
                )


listTraverse : (a -> Maybe b) -> List a -> Maybe (List b)
listTraverse fn list =
    List.foldl
        (\x acc -> Maybe.map2 (::) (fn x) acc)
        (Just [])
        list

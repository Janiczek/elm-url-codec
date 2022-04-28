module Url.Codec.Internal exposing
    ( Parser, ParseError(..)
    , parse, parseOneOf
    , succeed, s, string, int
    )

{-|

@docs Parser, ParseError
@docs parse, parseOneOf
@docs succeed, s, string, int

-}


type alias Parser parseResult =
    List String -> Result ( ParseError, Int ) ( parseResult, List String, Int )


type ParseError
    = SegmentMismatch
        { expected : String
        , available : String
        }
    | SegmentNotAvailable
    | WasNotInt String
    | DidNotConsumeEverything (List String)
    | NoParsers


parse : Parser a -> String -> Result ParseError a
parse parser path =
    parse_ parser (pathToSegments path)
        |> Result.mapError Tuple.first


parse_ : Parser a -> List String -> Result ( ParseError, Int ) a
parse_ parser segments =
    parser segments
        |> Result.andThen checkEmptyLeftovers


parseOneOf : List (Parser a) -> String -> Result ParseError a
parseOneOf parsers path =
    case parsers of
        [] ->
            Err NoParsers

        first :: rest ->
            let
                segments : List String
                segments =
                    pathToSegments path
            in
            case parse_ first segments of
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
                                    case parse_ currentParser segments of
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


pathToSegments : String -> List String
pathToSegments path =
    path
        |> String.split "/"
        |> removeLeadingEmpty
        |> removeTrailingEmpty


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
succeed thing segments =
    Ok ( thing, segments, 0 )


s : String -> Parser a -> Parser a
s expected innerParser segments =
    innerParser segments
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
string innerParser segments =
    innerParser segments
        |> Result.andThen
            (\( stringToThing, segments2, depth ) ->
                case segments2 of
                    first :: rest ->
                        Ok ( stringToThing first, rest, depth + 1 )

                    [] ->
                        Err ( SegmentNotAvailable, depth )
            )


int : Parser (Int -> a) -> Parser a
int innerParser segments =
    innerParser segments
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

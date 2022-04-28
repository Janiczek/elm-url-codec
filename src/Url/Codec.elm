module Url.Codec exposing
    ( Codec
    , parse, ParseError(..)
    , succeed
    , s
    , int, string
    , parseOneOf
    )

{-|

@docs Codec
@docs parse, ParseError
@docs toRelativeUrl, toAbsoluteUrl
@docs succeed
@docs s
@docs int, string

TODO note it will return the deepest error (parser that dove the deepest)

-}


type ParseError
    = SegmentMismatch
        { expected : String
        , available : String
        }
    | SegmentNotAvailable
    | WasNotInt String
    | DidNotConsumeEverything (List String)
    | NoCodecs


type Codec a
    = Codec
        { parser : Parser a
        }


type alias Parser a =
    List String -> Result ( ParseError, Int ) ( a, List String, Int )


parse : Codec a -> String -> Result ParseError a
parse codec path =
    parse_ codec (toSegments path)
        |> Result.mapError Tuple.first


parse_ : Codec a -> List String -> Result ( ParseError, Int ) a
parse_ (Codec { parser }) segments =
    parser segments
        |> Result.andThen checkEmptyLeftovers


parseOneOf : List (Codec a) -> String -> Result ParseError a
parseOneOf codecs path =
    case codecs of
        [] ->
            Err NoCodecs

        first :: rest ->
            let
                segments : List String
                segments =
                    toSegments path
            in
            case parse_ first segments of
                Ok value ->
                    Ok value

                Err ( firstErr, firstDepth ) ->
                    let
                        go : ( ParseError, Int ) -> List (Codec a) -> Result ParseError a
                        go ( lastErr, lastDepth ) accCodecs =
                            case accCodecs of
                                [] ->
                                    Err lastErr

                                currentCodec :: restCodecs ->
                                    case parse_ currentCodec segments of
                                        Ok value ->
                                            Ok value

                                        Err ( currentErr, currentDepth ) ->
                                            go
                                                (if lastDepth > currentDepth then
                                                    ( lastErr, lastDepth )

                                                 else
                                                    ( currentErr, currentDepth )
                                                )
                                                restCodecs
                    in
                    go ( firstErr, firstDepth ) rest


toSegments : String -> List String
toSegments path =
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

        segment :: rest ->
            segment :: removeTrailingEmpty rest


checkEmptyLeftovers : ( a, List String, Int ) -> Result ( ParseError, Int ) a
checkEmptyLeftovers ( value, leftoverSegments, depth ) =
    if List.isEmpty leftoverSegments then
        Ok value

    else
        Err ( DidNotConsumeEverything leftoverSegments, depth )



-- COMBINATORS


succeed : a -> Codec a
succeed thing =
    Codec
        { parser = \segments -> Ok ( thing, segments, 0 )
        }


s : String -> Codec a -> Codec a
s expected (Codec inner) =
    Codec
        { parser =
            \segments ->
                inner.parser segments
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
        }


string : Codec (String -> a) -> Codec a
string (Codec inner) =
    Codec
        { parser =
            \segments ->
                inner.parser segments
                    |> Result.andThen
                        (\( stringToThing, segments2, depth ) ->
                            case segments2 of
                                first :: rest ->
                                    Ok ( stringToThing first, rest, depth + 1 )

                                [] ->
                                    Err ( SegmentNotAvailable, depth )
                        )
        }


int : Codec (Int -> a) -> Codec a
int (Codec inner) =
    Codec
        { parser =
            \segments ->
                inner.parser segments
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
        }

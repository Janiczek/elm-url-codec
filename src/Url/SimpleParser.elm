module Url.SimpleParser exposing
    ( Parser, ParseError(..), parse, parseOneOf
    , succeed, s, int, string
    )

{-| A simpler alternative to [`Url.Parser`](https://package.elm-lang.org/packages/elm/url/latest/Url-Parser)
module from the [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/)
package.

Note that if you want to build both URL parsers and builders at the same time,
you should use the [`Url.Codec`](Url-Codec) module instead. The API is very similar!


## URL parsing

@docs Parser, ParseError, parse, parseOneOf


## Combinators

@docs succeed, s, int, string

-}

import Url.Codec.Internal as Internal exposing (Parser)


{-| Parser knows how to parse an URL string into Elm data.

Create it with the combinators [`succeed`](#succeed), [`s`](#s), [`int`](#int)
and [`string`](#string).

Use it to parse URLs with the functions [`parse`](#parse) and
[`parseOneOf`](#parseOneOf).

Leading and trailing slashes don't matter: don't feel an obligation to sanitize
your input!

-}
type Parser a
    = Parser (Internal.Parser a)


{-| All the ways the parsing can fail.
-}
type ParseError
    = SegmentMismatch
        { expected : String
        , available : String
        }
    | SegmentNotAvailable
    | WasNotInt String
    | DidNotConsumeEverything (List String)
    | NoParsers


internalErrorToOurError : Internal.ParseError -> ParseError
internalErrorToOurError err =
    case err of
        Internal.SegmentMismatch r ->
            SegmentMismatch r

        Internal.SegmentNotAvailable ->
            SegmentNotAvailable

        Internal.WasNotInt str ->
            WasNotInt str

        Internal.DidNotConsumeEverything list ->
            DidNotConsumeEverything list

        Internal.NoParsers ->
            NoParsers


{-| Parse the URL path string using the provided parser.

    Url.SimpleParser.parse helloParser "hello/123"
    --> Ok (HelloPage 123)

    Url.SimpleParser.parse helloParser "hello/123whoops"
    --> Err (WasNotInt "123whoops")

-}
parse : Parser a -> String -> Result ParseError a
parse (Parser parser) path =
    Internal.parse parser path
        |> Result.mapError internalErrorToOurError


getInternalParser : Parser a -> Internal.Parser a
getInternalParser (Parser parser) =
    parser


{-| Parse the URL path string, trying out multiple parsers if necessary.

Will stop at the first success.

In case of errors will present an error from the parser that had the most success
before failing.

In case two parsers failed at the same depth, will prefer the later one.

    allParsers =
        [ helloParser, homeParser ]

    Url.SimpleParser.parseOneOf allParsers "hello/123"
    --> Ok (HelloPage 123)

    Url.SimpleParser.parseOneOf allParsers ""
    --> Ok HomePage

    Url.SimpleParser.parseOneOf [] ""
    --> Err NoParsers

-}
parseOneOf : List (Parser a) -> String -> Result ParseError a
parseOneOf parsers path =
    Internal.parseOneOf (List.map getInternalParser parsers) path
        |> Result.mapError internalErrorToOurError


{-| A way to start your Parser definition.

Can also work standalone for URLs without path segments:

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed HomeRoute

    Url.SimpleParser.parse parser ""
    --> Ok HomeRoute

-}
succeed : a -> Parser a
succeed thing =
    Parser <| Internal.succeed thing


{-| A hardcoded path segment.

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed HomeRoute
            |> Url.SimpleParser.s "home"

    Url.SimpleParser.parse parser "home"
    --> Ok HomeRoute

-}
s : String -> Parser a -> Parser a
s segment (Parser inner) =
    Parser <| Internal.s segment inner


{-| A string path segment.

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed PostRoute
            |> Url.SimpleParser.s "post"
            |> Url.SimpleParser.string

    Url.SimpleParser.parse parser "post/hello"
    --> Ok (PostRoute "hello")

-}
string : Parser (String -> a) -> Parser a
string (Parser inner) =
    Parser <| Internal.string inner


{-| An integer path segment.

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed UserRoute
            |> Url.SimpleParser.s "user"
            |> Url.SimpleParser.int

    Url.SimpleParser.parse parser "user/123"
    --> Ok (UserRoute 123)

-}
int : Parser (Int -> a) -> Parser a
int (Parser inner) =
    Parser <| Internal.int inner

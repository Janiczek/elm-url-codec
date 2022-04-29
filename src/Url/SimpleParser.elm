module Url.SimpleParser exposing
    ( Parser, ParseError(..), parsePath, parseUrl
    , succeed, s, int, string
    , intQuery, stringQuery, intsQuery, stringsQuery
    , queryFlag, allQueryFlags
    , fragment
    )

{-| A simpler alternative to [`Url.Parser`](https://package.elm-lang.org/packages/elm/url/latest/Url-Parser)
module from the [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/)
package.

Note that if you want to build both URL parsers and builders at the same time,
you should use the [`Url.Codec`](Url-Codec) module instead. The API is very similar!


## URL parsing

@docs Parser, ParseError, parsePath, parseUrl


## Combinators

@docs succeed, s, int, string


## Query parameters

@docs intQuery, stringQuery, intsQuery, stringsQuery
@docs queryFlag, allQueryFlags


## Fragment

@docs fragment

-}

import Url exposing (Url)
import Url.Codec.Internal as Internal exposing (Parser)


{-| Parser knows how to parse an URL string into Elm data.

Create it with the combinators [`succeed`](#succeed), [`s`](#s), [`int`](#int)
and [`string`](#string).

Parse query parameters with [] TODO
TODO fragment

Use it to parse URLs with the functions [`parsePath`](#parsePath) and [`parseUrl`](#parseUrl).

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
    | NeededSingleQueryParameterValueGotMultiple
        { key : String
        , got : List String
        }
    | NotAllQueryParameterValuesWereInts
        { key : String
        , got : List String
        }
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

        Internal.NeededSingleQueryParameterValueGotMultiple r ->
            NeededSingleQueryParameterValueGotMultiple r

        Internal.NotAllQueryParameterValuesWereInts r ->
            NotAllQueryParameterValuesWereInts r

        Internal.NoParsers ->
            NoParsers


getInternalParser : Parser a -> Internal.Parser a
getInternalParser (Parser parser) =
    parser


{-| Parse the URL path string, trying out multiple parsers if necessary.

Will stop at the first success.

Will prefer to report error from the parser that had most success parsing.

    allParsers =
        [ helloParser, homeParser ]

    Url.SimpleParser.parse allParsers "hello/123"
    --> Ok (HelloPage 123)

    Url.SimpleParser.parse allParsers "/hello/123?comments=1"
    --> Ok (HelloPage 123)

    Url.SimpleParser.parse allParsers "hello/123whoops"
    --> Err (WasNotInt "123whoops")

    Url.SimpleParser.parse allParsers ""
    --> Ok HomePage

    Url.SimpleParser.parse [] ""
    --> Err NoParsers

-}
parsePath : List (Parser a) -> String -> Result ParseError a
parsePath parsers path =
    path
        |> Internal.pathToInput
        |> Internal.parse (List.map getInternalParser parsers)
        |> Result.mapError internalErrorToOurError


{-| A variant of [`parsePath`](#parsePath) that accepts an
[`Url`](https://package.elm-lang.org/packages/elm/url/latest/Url#Url).
-}
parseUrl : List (Parser parseResult) -> Url -> Result ParseError parseResult
parseUrl codecs url =
    url
        |> Internal.urlToInput
        |> Internal.parse (List.map getInternalParser codecs)
        |> Result.mapError internalErrorToOurError


{-| A way to start your Parser definition.

Can also work standalone for URLs without path segments:

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed HomeRoute

    Url.SimpleParser.parse [parser] ""
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

    Url.SimpleParser.parse [parser] "home"
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

    Url.SimpleParser.parse [parser] "post/hello"
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

    Url.SimpleParser.parse [parser] "user/123"
    --> Ok (UserRoute 123)

-}
int : Parser (Int -> a) -> Parser a
int (Parser inner) =
    Parser <| Internal.int inner


intQuery : String -> Parser (Maybe Int -> a) -> Parser a
intQuery key (Parser inner) =
    Parser <| Internal.intQuery key inner


stringQuery : String -> Parser (Maybe String -> a) -> Parser a
stringQuery key (Parser inner) =
    Parser <| Internal.stringQuery key inner


intsQuery : String -> Parser (List Int -> a) -> Parser a
intsQuery key (Parser inner) =
    Parser <| Internal.intsQuery key inner


stringsQuery : String -> Parser (List String -> a) -> Parser a
stringsQuery key (Parser inner) =
    Parser <| Internal.stringsQuery key inner


queryFlag : String -> Parser (Bool -> a) -> Parser a
queryFlag flag (Parser inner) =
    Parser <| Internal.queryFlag flag inner


allQueryFlags : Parser (List String -> a) -> Parser a
allQueryFlags (Parser inner) =
    Parser <| Internal.allQueryFlags inner


fragment : Parser (Maybe String -> a) -> Parser a
fragment (Parser inner) =
    Parser <| Internal.fragment inner

module Url.SimpleParser exposing
    ( Parser, ParseError(..), parsePath, parseUrl
    , succeed, s, int, string
    , queryInt, queryString, queryInts, queryStrings
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

@docs queryInt, queryString, queryInts, queryStrings
@docs queryFlag, allQueryFlags


## Fragment

@docs fragment

-}

import Url exposing (Url)
import Url.Codec.Internal as Internal exposing (Parser)


{-| Parser knows how to parse an URL string into Elm data.

Create it with the combinators:

  - [`succeed`](#succeed), [`s`](#s), [`int`](#int), [`string`](#string)
  - [`queryInt`](#queryInt), [`queryString`](#queryString), [`queryInts`](#queryInts), [`queryStrings`](#queryStrings),
  - [`queryFlag`](#queryFlag), [`allQueryFlags`](#allQueryFlags)
  - [`fragment`](#fragment)

Use it to parse URLs with the functions [`parsePath`](#parsePath) and [`parseUrl`](#parseUrl).

Leading and trailing slashes don't matter.

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

    Url.SimpleParser.parsePath allParsers "hello/123"
    --> Ok (HelloPage 123)

    Url.SimpleParser.parsePath allParsers "/hello/123?comments=1"
    --> Ok (HelloPage 123)

    Url.SimpleParser.parsePath allParsers "hello/123whoops"
    --> Err (WasNotInt "123whoops")

    Url.SimpleParser.parsePath allParsers ""
    --> Ok HomePage

    Url.SimpleParser.parsePath [] ""
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



-- COMBINATORS


{-| A way to start your Parser definition.

    unfinishedParser : Parser (String -> Route)
    unfinishedParser =
        -- needs a string provided via a combinator like `Url.SimpleParser.string`
        Url.SimpleParser.succeed UserRoute

Can also work standalone for URLs without path segments:

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed HomeRoute

    Url.SimpleParser.parsePath [parser] ""
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

    Url.SimpleParser.parsePath [parser] "home"
    --> Ok HomeRoute

-}
s : String -> Parser a -> Parser a
s segment (Parser inner) =
    Parser <| Internal.s segment inner


{-| A string path segment.

    type Route
        = PostRoute String
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed PostRoute
            |> Url.SimpleParser.s "post"
            |> Url.SimpleParser.string

    Url.SimpleParser.parsePath [parser] "post/hello"
    --> Ok (PostRoute "hello")

    Url.SimpleParser.parsePath [parser] "post"
    --> Err SegmentNotAvailable

-}
string : Parser (String -> a) -> Parser a
string (Parser inner) =
    Parser <| Internal.string inner


{-| An integer path segment.

    type Route
        = UserRoute Int
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed UserRoute
            |> Url.SimpleParser.s "user"
            |> Url.SimpleParser.int

    Url.SimpleParser.parsePath [parser] "user/123"
    --> Ok (UserRoute 123)

    Url.SimpleParser.parsePath [parser] "user"
    --> Err SegmentNotAvailable

-}
int : Parser (Int -> a) -> Parser a
int (Parser inner) =
    Parser <| Internal.int inner


{-| An integer query parameter.

    type Route
        = UserRoute (Maybe Int)
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed UserRoute
            |> Url.SimpleParser.s "user"
            |> Url.SimpleParser.queryInt "id"

    Url.SimpleParser.parsePath [parser] "user?id=123"
    --> Ok (UserRoute (Just 123))

    Url.SimpleParser.parsePath [parser] "user"
    --> Ok (UserRoute Nothing)

Will fail if there are multiple query parameters with the same key:

    Url.SimpleParser.parsePath [parser] "user?id=1&id=2"
    --> Err (NeededSingleQueryParameterValueGotMultiple { got = ["1","2"], key = "id" })

Will succeed with Nothing if the query parameter contains a non-integer string:

    Url.SimpleParser.parsePath [parser] "user?id=martin"
    --> Ok (UserRoute Nothing)

-}
queryInt : String -> Parser (Maybe Int -> a) -> Parser a
queryInt key (Parser inner) =
    Parser <| Internal.queryInt key inner


{-| A string query parameter.

    type Route
        = UserRoute (Maybe String)
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed UserRoute
            |> Url.SimpleParser.s "user"
            |> Url.SimpleParser.queryString "name"

    Url.SimpleParser.parsePath [parser] "user?name=martin"
    --> Ok (UserRoute (Just "martin"))

Will fail if there are multiple query parameters with the same key:

    Url.SimpleParser.parsePath [parser] "user?name=a&name=b"
    --> Err (NeededSingleQueryParameterValueGotMultiple { got = ["a","b"], key = "name" })

-}
queryString : String -> Parser (Maybe String -> a) -> Parser a
queryString key (Parser inner) =
    Parser <| Internal.queryString key inner


{-| A repeated integer query parameter.

    type Route
        = UserListingRoute (List Int)
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed UserListingRoute
            |> Url.SimpleParser.s "users"
            |> Url.SimpleParser.queryInts "id"

    Url.SimpleParser.parsePath [parser] "users?id=1"
    --> Ok (UserListingRoute [1])

    Url.SimpleParser.parsePath [parser] "users?id=1&id=2&id=3"
    --> Ok (UserListingRoute [1,2,3])

    Url.SimpleParser.parsePath [parser] "users"
    --> Ok (UserListingRoute [])

Will fail if given a query parameter with an empty value:

    Url.SimpleParser.parsePath [parser] "users?id="
    --> Err (NotAllQueryParameterValuesWereInts { got = [ "" ] , key = "id" })

Will fail if any of the query parameters has a non-integer value:

    Url.SimpleParser.parsePath [parser] "users?id=1&id=hello"
    --> Err (NotAllQueryParameterValuesWereInts { got = [ "1", "hello" ] , key = "id" })

-}
queryInts : String -> Parser (List Int -> a) -> Parser a
queryInts key (Parser inner) =
    Parser <| Internal.queryInts key inner


{-| A repeated string query parameter.

    type Route
        = UserListingRoute (List String)
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed UserListingRoute
            |> Url.SimpleParser.s "users"
            |> Url.SimpleParser.queryInts "tags"

    Url.SimpleParser.parsePath [parser] "users?tags=Foo"
    --> Ok (UserListingRoute ["Foo"])

    Url.SimpleParser.parsePath [parser] "users?tags=Foo&tags=Bar&tags=999"
    --> Ok (UserListingRoute ["Foo", "Bar", "999"])

    Url.SimpleParser.parsePath [parser] "users"
    --> Ok (UserListingRoute [])

Will succeed with an empty string if given a query parameter with an empty value:

    Url.SimpleParser.parsePath [parser] "users?tags="
    --> Ok (UserListingRoute [""])

-}
queryStrings : String -> Parser (List String -> a) -> Parser a
queryStrings key (Parser inner) =
    Parser <| Internal.queryStrings key inner


{-| A query flag (parameter without `=` and a value), like eg. `/settings?admin`.

    type Route
        = SettingsRoute { admin : Bool }
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed (\admin -> SettingsRoute { admin = admin })
            |> Url.SimpleParser.s "settings"
            |> Url.SimpleParser.queryFlag "admin"

    Url.SimpleParser.parsePath [parser] "settings?admin"
    --> Ok (SettingsRoute { admin = True })

    Url.SimpleParser.parsePath [parser] "settings"
    --> Ok (SettingsRoute { admin = False })

-}
queryFlag : String -> Parser (Bool -> a) -> Parser a
queryFlag flag (Parser inner) =
    Parser <| Internal.queryFlag flag inner


{-| All query flags, like eg. `/settings?admin&no-exports`.

    type Route
        = SettingsRoute (List String)
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed SettingsRoute
            |> Url.SimpleParser.s "settings"
            |> Url.SimpleParser.allQueryFlags

    Url.SimpleParser.parsePath [parser] "settings?admin"
    --> Ok (SettingsRoute ["admin"])

    Url.SimpleParser.parsePath [parser] "settings"
    --> Ok (SettingsRoute [])

    Url.SimpleParser.parsePath [parser] "settings?admin&no-exports"
    --> Ok (SettingsRoute ["admin", "no-exports"])

-}
allQueryFlags : Parser (List String -> a) -> Parser a
allQueryFlags (Parser inner) =
    Parser <| Internal.allQueryFlags inner


{-| Fragment part of the URL, eg. `/settings#HelloThereWorld`.

    type Route
        = SettingsRoute (Maybe String)
        | ...

    parser : Parser Route
    parser =
        Url.SimpleParser.succeed SettingsRoute
            |> Url.SimpleParser.s "settings"
            |> Url.SimpleParser.fragment

    Url.SimpleParser.parsePath [parser] "settings#abc"
    --> Ok (SettingsRoute (Just "abc"))

    Url.SimpleParser.parsePath [parser] "settings"
    --> Ok (SettingsRoute Nothing)

    Url.SimpleParser.parsePath [parser] "settings#"
    --> Ok (SettingsRoute (Just ""))

-}
fragment : Parser (Maybe String -> a) -> Parser a
fragment (Parser inner) =
    Parser <| Internal.fragment inner

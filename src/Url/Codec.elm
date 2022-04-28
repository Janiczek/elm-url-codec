module Url.Codec exposing
    ( Codec, CodecInProgress
    , ParseError(..), parse, parseOneOf
    , toString, toStringOneOf
    , succeed, s, int, string
    )

{-| An alternative to [`Url.Parser`](https://package.elm-lang.org/packages/elm/url/latest/Url-Parser)
and [`Url.Builder`](https://package.elm-lang.org/packages/elm/url/latest/Url-Builder)
modules from the [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/)
package.

**Allows you to define both the URL parser and the URL builder at the same time.**

Note that if you only need an URL parser, the [`Url.SimpleParser`](Url-SimpleParser)
module will be nicer to use while providing the same functionality.

@docs Codec, CodecInProgress


## URL parsing

@docs ParseError, parse, parseOneOf


## URL building

@docs toString, toStringOneOf


## Combinators

@docs succeed, s, int, string

-}

import Url.Codec.Internal as Internal


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
    | NoCodecs


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
            NoCodecs


{-| Codec knows both:

  - how to parse an URL string into Elm data
  - how to build an URL from Elm data.

Create it with the combinators [`succeed`](#succeed), [`s`](#s), [`int`](#int)
and [`string`](#string).

Use it to **parse** URLs with the functions [`parse`](#parse) and
[`parseOneOf`](#parseOneOf).

Use it to **build** URLs with the functions [`toString`](#toString) and
[`toStringOneOf`](#toStringOneOf).

Leading and trailing slashes don't matter: don't feel an obligation to sanitize
your input!

-}
type alias Codec target =
    CodecInProgress target target


{-| CodecInProgress is an unfinished codec that needs some more steps to be able
to fully parse and build URLs.

Whenever you see `CodecInProgress` with two unequal types, you're likely missing
an argument to your final value somewhere.

You'll typically start with something like

    type Route
        = CommentPage String Int
        | PostPage String

    myCodec =
        Url.Codec.succeed CommentPage

At this point, the codec is of type:

    myCodec : CodecInProgress a (String -> Int -> Route)

_(After the first usage of [`int`](#int) or [`string`](#string) this will become
more precise: the `a` will become a `Route`.)_

Your goal here is to provide both arguments to `CommentPage`: the `String` and
the `Int`. Do that with the [`int`](#int) / [`string`](#string) functions. You
can also use the [`s`](#s) (as in "segment") function to provide hardcoded
segments in the URL path:

    myCodec =
        Url.Codec.succeed CommentPage
            |> Url.Codec.s "post"
            |> Url.Codec.string getCommentPageSlug
            |> Url.Codec.s "page"
            |> Url.Codec.int getCommentPageNumber

Now that we've used both [`string`](#string) and [`int`](#int), this codec will
be able to both parse and build URLs:

    Url.Codec.parse myCodec "post/hello-world/page/1"
    --> Ok (CommentPage "hello-world" 1)

    Url.Codec.toString myCodec (CommentPage "you-too" 222)
    --> Just "post/you-too/page/222"

Note that the [`int`](#int) and [`string`](#string) functions need you to provide
a getter. Here is a typical implementation of one:

    getCommentPageSlug : Route -> Maybe String
    getCommentPageSlug route =
        case route of
            CommentPage slug _ ->
                Just slug

            _ ->
                Nothing

_Sidenote: In principle it should be possible to make an `elm-review` rule that
would generate these getters for you. The more noise you'll make (ping
`@janiczek` on the Elm Slack or on Twitter), the better chance it will come into
existence :)_

-}
type CodecInProgress target parseResult
    = C
        { parser : Internal.Parser parseResult
        , toSegments : List (target -> Maybe String)
        }


{-| Parse the URL path string using the provided codec.

    Url.Codec.parse helloCodec "hello/123"
    --> Ok (HelloPage 123)

    Url.Codec.parse helloCodec "hello/123whoops"
    --> Err (WasNotInt "123whoops")

-}
parse : Codec parseResult -> String -> Result ParseError parseResult
parse (C codec) path =
    Internal.parse codec.parser path
        |> Result.mapError internalErrorToOurError


getParser : Codec a -> Internal.Parser a
getParser (C codec) =
    codec.parser


{-| Parse the URL path string, trying out multiple codecs if necessary.

Will stop at the first success.

In case of errors will present an error from the codec that had the most success
before failing.

In case two codec failed at the same depth, will prefer the later one.

    allCodecs =
        [ helloCodec, homeCodec ]

    Url.Codec.parseOneOf allCodecs "hello/123"
    --> Ok (HelloPage 123)

    Url.Codec.parseOneOf allCodecs ""
    --> Ok HomePage

    Url.Codec.parseOneOf [] ""
    --> Err NoCodecs

-}
parseOneOf : List (Codec parseResult) -> String -> Result ParseError parseResult
parseOneOf codecs path =
    Internal.parseOneOf (List.map getParser codecs) path
        |> Result.mapError internalErrorToOurError



-- URL BUILDING


listTraverse : (a -> Maybe b) -> List a -> Maybe (List b)
listTraverse fn list =
    List.foldl
        (\x acc -> Maybe.map2 (::) (fn x) acc)
        (Just [])
        list


{-| Convert the given value into an URL string.

Can fail (eg. if you use a codec for one route with a string belonging to a
different route, such that the getters will return Nothing).

    Url.Codec.toString helloCodec (HelloPage 123)
    --> "hello/123"

-}
toString : Codec target -> target -> Maybe String
toString (C codec) thing =
    codec.toSegments
        |> listTraverse (\fn -> fn thing)
        |> Maybe.map (String.join "/")


{-| Convert the given value into an URL string, trying out multiple codecs if
necessary.

Will stop at the first success.

Can fail (eg. if you use a codec for one route with a string belonging to a
different route, such that the used getters will return Nothing).

    allCodecs =
        [ helloCodec, postCodec ]

    Url.Codec.toString allCodecs (HelloPage 123)
    --> "hello/123"

    Url.Codec.toString allCodecs (PostPage "goto-bad")
    --> "post/goto-bad"

-}
toStringOneOf : List (Codec target) -> target -> Maybe String
toStringOneOf codecs thing =
    let
        go : List (Codec target) -> Maybe String
        go accCodecs =
            case accCodecs of
                [] ->
                    Nothing

                codec :: rest ->
                    case toString codec thing of
                        Nothing ->
                            go rest

                        Just string_ ->
                            Just string_
    in
    go codecs



-- COMBINATORS


{-| A way to start your Codec definition.

    unfinishedCodec : CodecInProgress Route (String -> Route)
    unfinishedCodec =
        -- needs a string provided via Url.Codec.string
        Url.Codec.succeed UserRoute

Can also work standalone for URLs without path segments:

    codec : Codec Route
    codec =
        Url.Codec.succeed HomeRoute

    Url.Codec.parse codec ""
    --> Ok HomeRoute

    Url.Codec.toString codec HomeRoute
    --> Just ""

-}
succeed : parseResult -> CodecInProgress target parseResult
succeed thing =
    C
        { parser = Internal.succeed thing
        , toSegments = []
        }


{-| A hardcoded path segment.

    codec : Codec Route
    codec =
        Url.Codec.succeed HomeRoute
            |> Url.Codec.s "home"

    Url.Codec.parse coded "home"
    --> Ok HomeRoute

    Url.Codec.toString codec HomeRoute
    --> Just "home"

-}
s : String -> CodecInProgress target parseResult -> CodecInProgress target parseResult
s expected (C inner) =
    C
        { parser = Internal.s expected inner.parser
        , toSegments = (\_ -> Just expected) :: inner.toSegments
        }


{-| A string path segment.

    codec : Codec Route
    codec =
        Url.Codec.succeed PostRoute
            |> Url.Codec.s "post"
            |> Url.Codec.string getPostRouteSlug

    getPostRouteSlug : Route -> Maybe String
    getPostRouteSlug route =
        case route of
            PostRoute slug ->
                Just slug

            _ ->
                Nothing

    Url.Codec.parse codec "post/hello"
    --> Ok (PostRoute "hello")

    Url.Codec.toString codec (PostRoute "hiya")
    --> Just "post/hiya"

-}
string :
    (target -> Maybe String)
    -> CodecInProgress target (String -> parseResult)
    -> CodecInProgress target parseResult
string getter (C inner) =
    C
        { parser = Internal.string inner.parser
        , toSegments = getter :: inner.toSegments
        }


{-| An integer path segment.

    codec : Codec Route
    codec =
        Url.Codec.succeed UserRoute
            |> Url.Codec.s "user"
            |> Url.Codec.int getUserRouteId

    getUserRouteId : Route -> Maybe Int
    getUserRouteId route =
        case route of
            UserRoute id ->
                Just id

            _ ->
                Nothing

    Url.Codec.parse codec "user/123"
    --> Ok (UserRoute 123)

    Url.Codec.toString codec (UserRoute 999)
    --> Just "user/999"

-}
int :
    (target -> Maybe Int)
    -> CodecInProgress target (Int -> parseResult)
    -> CodecInProgress target parseResult
int getter (C inner) =
    C
        { parser = Internal.int inner.parser
        , toSegments = (getter >> Maybe.map String.fromInt) :: inner.toSegments
        }

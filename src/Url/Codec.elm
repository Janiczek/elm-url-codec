module Url.Codec exposing
    ( Codec, CodecInProgress
    , ParseError(..), parsePath, parseUrl
    , toString
    , succeed, s, int, string
    , intQuery, stringQuery, intsQuery, stringsQuery
    , queryFlag, allQueryFlags
    , fragment
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

@docs ParseError, parsePath, parseUrl


## URL building

@docs toString


## Combinators

@docs succeed, s, int, string


## Query parameters

@docs intQuery, stringQuery, intsQuery, stringsQuery
@docs queryFlag, allQueryFlags


## Fragment

@docs fragment

-}

import Url exposing (Url)
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
    | NeededSingleQueryParameterValueGotMultiple
        { key : String
        , got : List String
        }
    | NotAllQueryParameterValuesWereInts
        { key : String
        , got : List String
        }
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

        Internal.NeededSingleQueryParameterValueGotMultiple r ->
            NeededSingleQueryParameterValueGotMultiple r

        Internal.NotAllQueryParameterValuesWereInts r ->
            NotAllQueryParameterValuesWereInts r

        Internal.NoParsers ->
            NoCodecs


{-| Codec knows both:

  - how to parse an URL string into Elm data
  - how to build an URL from Elm data.

Create it with the combinators [`succeed`](#succeed), [`s`](#s), [`int`](#int)
and [`string`](#string).

Use it to **parse** URLs with the functions [`parsePath`](#parsePath) and [`parseUrl`](#parseUrl).

Use it to **build** URLs with the function [`toString`](#toString).

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

    Url.Codec.parsePath [myCodec] "/post/hello-world/page/1"
    --> Ok (CommentPage "hello-world" 1)

    Url.Codec.toString [myCodec] (CommentPage "you-too" 222)
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
        , toQueryParams : List (target -> Maybe ( String, List String ))
        , toQueryFlags : List (target -> List String)
        , toFragment : Maybe (target -> Maybe String)
        }


getParser : Codec a -> Internal.Parser a
getParser (C codec) =
    codec.parser


{-| Parse the URL path string, trying out multiple codecs if necessary.

Will stop at the first success.

Will prefer to report error from the parser that had most success parsing.

    allCodecs =
        [ helloCodec, homeCodec ]

    Url.Codec.parsePath allCodecs "hello/123"
    --> Ok (HelloPage 123)

    Url.Codec.parsePath allCodecs "/hello/123?comments=1"
    --> Ok (HelloPage 123)

    Url.Codec.parsePath allCodecs "hello/123whoops"
    --> Err (WasNotInt "123whoops")

    Url.Codec.parsePath allCodecs ""
    --> Ok HomePage

    Url.Codec.parsePath [] ""
    --> Err NoCodecs

-}
parsePath : List (Codec parseResult) -> String -> Result ParseError parseResult
parsePath codecs path =
    path
        |> Internal.pathToInput
        |> Internal.parse (List.map getParser codecs)
        |> Result.mapError internalErrorToOurError


{-| A variant of [`parsePath`](#parsePath) that accepts an
[`Url`](https://package.elm-lang.org/packages/elm/url/latest/Url#Url).
-}
parseUrl : List (Codec parseResult) -> Url -> Result ParseError parseResult
parseUrl codecs url =
    url
        |> Internal.urlToInput
        |> Internal.parse (List.map getParser codecs)
        |> Result.mapError internalErrorToOurError



-- URL BUILDING


{-| Convert the given value into an URL string.

Can fail (eg. if you use a codec for one route with a string belonging to a
different route, such that the getters will return Nothing).

    Url.Codec.toString [helloCodec] (HelloPage 123)
    --> Just "hello/123"

-}
toStringSingle : Codec target -> target -> Maybe String
toStringSingle (C codec) thing =
    codec.toSegments
        |> Internal.listTraverse (\fn -> fn thing)
        |> Maybe.map
            (\pathParts ->
                let
                    params : List String
                    params =
                        codec.toQueryParams
                            |> List.filterMap (\fn -> fn thing)
                            |> List.reverse
                            |> List.concatMap
                                (\( key, values ) ->
                                    let
                                        pctKey : String
                                        pctKey =
                                            Url.percentEncode key
                                    in
                                    values
                                        |> List.map
                                            (\value ->
                                                pctKey
                                                    ++ "="
                                                    ++ Url.percentEncode value
                                            )
                                )

                    flags : List String
                    flags =
                        codec.toQueryFlags
                            |> List.concatMap (\fn -> fn thing)
                            |> List.map Url.percentEncode

                    allQueries : List String
                    allQueries =
                        params ++ flags
                in
                Internal.constructPath
                    { path =
                        pathParts
                            |> List.map Url.percentEncode
                            |> String.join "/"
                    , query =
                        if List.isEmpty allQueries then
                            Nothing

                        else
                            Just (String.join "&" (params ++ flags))
                    , fragment =
                        codec.toFragment
                            |> Maybe.andThen (\fn -> fn thing)
                            |> Maybe.map Url.percentEncode
                    }
            )


{-| Convert the given value into an URL string, trying out multiple codecs if
necessary.

Will stop at the first success.

Can fail (eg. if you use a codec for one route with a string belonging to a
different route, such that the used getters will return Nothing).

    allCodecs =
        [ helloCodec, postCodec ]

    Url.Codec.toString allCodecs (HelloPage 123)
    --> Just "hello/123"

    Url.Codec.toString allCodecs (PostPage "goto-bad")
    --> Just "post/goto-bad"

-}
toString : List (Codec target) -> target -> Maybe String
toString codecs thing =
    case codecs of
        [] ->
            Nothing

        c :: cs ->
            case toStringSingle c thing of
                Nothing ->
                    toString cs thing

                Just string_ ->
                    Just string_



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

    Url.Codec.parse [codec] ""
    --> Ok HomeRoute

    Url.Codec.toString [codec] HomeRoute
    --> Just ""

-}
succeed : parseResult -> CodecInProgress target parseResult
succeed thing =
    C
        { parser = Internal.succeed thing
        , toSegments = []
        , toQueryParams = []
        , toQueryFlags = []
        , toFragment = Nothing
        }


{-| A hardcoded path segment.

    codec : Codec Route
    codec =
        Url.Codec.succeed HomeRoute
            |> Url.Codec.s "home"

    Url.Codec.parse [codec] "home"
    --> Ok HomeRoute

    Url.Codec.toString [codec] HomeRoute
    --> Just "home"

-}
s : String -> CodecInProgress target parseResult -> CodecInProgress target parseResult
s expected (C inner) =
    C
        { parser = Internal.s expected inner.parser
        , toSegments = (\_ -> Just expected) :: inner.toSegments
        , toQueryParams = inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
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

    Url.Codec.parse [codec] "post/hello"
    --> Ok (PostRoute "hello")

    Url.Codec.toString [codec] (PostRoute "hiya")
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
        , toQueryParams = inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
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

    Url.Codec.parse [codec] "user/123"
    --> Ok (UserRoute 123)

    Url.Codec.toString [codec] (UserRoute 999)
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
        , toQueryParams = inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        }


intQuery :
    String
    -> (target -> Maybe Int)
    -> CodecInProgress target (Maybe Int -> parseResult)
    -> CodecInProgress target parseResult
intQuery key getter (C inner) =
    C
        { parser = Internal.intQuery key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (getter >> Maybe.map (\int_ -> ( key, [ String.fromInt int_ ] )))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        }


stringQuery :
    String
    -> (target -> Maybe String)
    -> CodecInProgress target (Maybe String -> parseResult)
    -> CodecInProgress target parseResult
stringQuery key getter (C inner) =
    C
        { parser = Internal.stringQuery key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (getter >> Maybe.map (\str -> ( key, [ str ] )))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        }


intsQuery :
    String
    -> (target -> List Int)
    -> CodecInProgress target (List Int -> parseResult)
    -> CodecInProgress target parseResult
intsQuery key getter (C inner) =
    C
        { parser = Internal.intsQuery key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (\item -> Just ( key, List.map String.fromInt (getter item) ))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        }


stringsQuery :
    String
    -> (target -> List String)
    -> CodecInProgress target (List String -> parseResult)
    -> CodecInProgress target parseResult
stringsQuery key getter (C inner) =
    C
        { parser = Internal.stringsQuery key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (\item -> Just ( key, getter item ))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        }


queryFlag :
    String
    -> (target -> Bool)
    -> CodecInProgress target (Bool -> parseResult)
    -> CodecInProgress target parseResult
queryFlag flag getter (C inner) =
    C
        { parser = Internal.queryFlag flag inner.parser
        , toSegments = inner.toSegments
        , toQueryParams = inner.toQueryParams
        , toQueryFlags =
            (\target ->
                if getter target then
                    [ flag ]

                else
                    []
            )
                :: inner.toQueryFlags
        , toFragment = inner.toFragment
        }


allQueryFlags :
    (target -> List String)
    -> CodecInProgress target (List String -> parseResult)
    -> CodecInProgress target parseResult
allQueryFlags getter (C inner) =
    C
        { parser = Internal.allQueryFlags inner.parser
        , toSegments = inner.toSegments
        , toQueryParams = inner.toQueryParams
        , toQueryFlags = getter :: inner.toQueryFlags
        , toFragment = inner.toFragment
        }


fragment :
    (target -> Maybe String)
    -> CodecInProgress target (Maybe String -> parseResult)
    -> CodecInProgress target parseResult
fragment getter (C inner) =
    C
        { parser = Internal.fragment inner.parser
        , toSegments = inner.toSegments
        , toQueryParams = inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = Just getter
        }

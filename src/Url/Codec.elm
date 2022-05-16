module Url.Codec exposing
    ( Codec, CodecInProgress
    , ParseError(..), parsePath, parseUrl
    , toString
    , succeed, s, int, string
    , queryInt, queryString, queryInts, queryStrings
    , queryFlag, allQueryFlags
    , fragment
    )

{-| An alternative to [`Url.Parser`](https://package.elm-lang.org/packages/elm/url/latest/Url-Parser)
and [`Url.Builder`](https://package.elm-lang.org/packages/elm/url/latest/Url-Builder)
modules from the [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/)
package.

**Allows you to define both the URL parser and the URL builder at the same time.**

Note that if you only need an URL parser, the [`Url.SimpleParser`](Url-SimpleParser)
module will be nicer to use (require less boilerplate) while providing the same
functionality.

@docs Codec, CodecInProgress


## URL parsing

@docs ParseError, parsePath, parseUrl


## URL building

@docs toString


## Combinators

@docs succeed, s, int, string


## Query parameters

@docs queryInt, queryString, queryInts, queryStrings
@docs queryFlag, allQueryFlags


## Fragment

@docs fragment

-}

import Url exposing (Url)
import Url.Codec.Internal as Internal


{-| Codec knows both:

  - how to parse an URL string into Elm data
  - how to build an URL from Elm data.

Create it with the combinators:

  - [`succeed`](#succeed), [`s`](#s), [`int`](#int), [`string`](#string)
  - [`queryInt`](#queryInt), [`queryString`](#queryString), [`queryInts`](#queryInts), [`queryStrings`](#queryStrings),
  - [`queryFlag`](#queryFlag), [`allQueryFlags`](#allQueryFlags)
  - [`fragment`](#fragment)

Use it to **parse** URLs with the functions [`parsePath`](#parsePath) and [`parseUrl`](#parseUrl).

Use it to **build** URLs with the function [`toString`](#toString).

Leading and trailing slashes don't matter.

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

    isComment : Route -> Bool
    isComment route =
        case route of
            CommentPage _ _ ->
                True

            _ ->
                False

    myCodec =
        Url.Codec.succeed CommentPage isComment

At this point, the codec is of type:

    myCodec : CodecInProgress Route (String -> Int -> Route)

Your goal here is to provide both arguments to `CommentPage`: the `String` and
the `Int`. Do that with the various combinators ([`int`](#int),
[`string`](#string), [`fragment`](#fragment) and all the `*query*` functions).

You can also use the [`s`](#s) (as in "segment") function to provide hardcoded
segments in the URL path:

    myCodec =
        Url.Codec.succeed CommentPage isComment
            |> Url.Codec.s "post"
            |> Url.Codec.string getCommentPageSlug
            |> Url.Codec.s "page"
            |> Url.Codec.int getCommentPageNumber

Now that we've used both [`string`](#string) and [`int`](#int), the second part
of the type signature has changed from `String -> Int -> Route` to `Route`,
meaning both parts are the same (`CodecInProgress Route Route`, also expressible
as `Codec Route`) and the codec is ready to use! It will now be able to both
parse and build URLs:

    Url.Codec.parsePath [myCodec] "/post/hello-world/page/1"
    --> Ok (CommentPage "hello-world" 1)

    Url.Codec.toString [myCodec] (CommentPage "you-too" 222)
    --> Just "post/you-too/page/222"

Note that the functions that provide you with data will need you to provide
a getter, used by the `toString` function to get the data from your `Route` type.
Here is a typical implementation of one:

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
        , isThing : target -> Bool
        }


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


toStringSingle : Codec target -> target -> Maybe String
toStringSingle (C codec) thing =
    if codec.isThing thing then
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

    else
        Nothing


{-| Convert the given value into an URL string, trying out multiple codecs if
necessary.

Will stop at the first success.

Can fail (eg. if you use a codec for one route with a string belonging to a
different route, such that the predicate given to `success` will return `False`
or the getters return `Nothing`).

    allCodecs =
        [ helloCodec, postCodec ]

    Url.Codec.toString allCodecs (HelloPage 123)
    --> Just "hello/123"

    Url.Codec.toString allCodecs (PostPage "goto-bad")
    --> Just "post/goto-bad"

    Url.Codec.toString [nonHelloPageCodec] (HelloPage 123)
    --> Nothing

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
        -- needs a string provided via a combinator like `Url.Codec.string`
        Url.Codec.succeed UserRoute isUserRoute

    isUserRoute : Route -> Bool
    isUserRoute route =
        case route of
            UserRoute _ ->
                True

            _ ->
                False

You'll then need to continue with some other combinators to provide the data to
your route constructor.

Can also work standalone for URLs without path segments:

    -- same as CodecInProgress Route Route
    codec : Codec Route
    codec =
        Url.Codec.succeed HomeRoute isHomeRoute

    isHomeRoute : Route -> Bool
    isHomeRoute route =
        route == HomeRoute

    Url.Codec.parsePath [codec] ""
    --> Ok HomeRoute

    Url.Codec.toString [codec] HomeRoute
    --> Just ""

-}
succeed : parseResult -> (target -> Bool) -> CodecInProgress target parseResult
succeed thing isThing =
    C
        { parser = Internal.succeed thing
        , toSegments = []
        , toQueryParams = []
        , toQueryFlags = []
        , toFragment = Nothing
        , isThing = isThing
        }


{-| A hardcoded path segment.

    type Route
        = HomeRoute

    codec : Codec Route
    codec =
        Url.Codec.succeed HomeRoute isHomeRoute
            |> Url.Codec.s "home"

    isHomeRoute : Route -> Bool
    isHomeRoute route =
        route == HomeRoute

    Url.Codec.parsePath [codec] "home"
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
        , isThing = inner.isThing
        }


{-| A string path segment.

    type Route
        = PostRoute String
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed PostRoute isPostRoute
            |> Url.Codec.s "post"
            |> Url.Codec.string getPostRouteSlug

    isPostRoute : Route -> Bool
    isPostRoute route =
        case route of
            PostRoute _ ->
                True

            _ ->
                False

    getPostRouteSlug : Route -> Maybe String
    getPostRouteSlug route =
        case route of
            PostRoute slug ->
                Just slug

            _ ->
                Nothing

    Url.Codec.parsePath [codec] "post/hello"
    --> Ok (PostRoute "hello")

    Url.Codec.parsePath [codec] "post"
    --> Err SegmentNotAvailable

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
        , isThing = inner.isThing
        }


{-| An integer path segment.

    type Route
        = UserRoute Int
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed UserRoute isUserRoute
            |> Url.Codec.s "user"
            |> Url.Codec.int getUserRouteId

    isUserRoute : Route -> Bool
    isUserRoute route =
        case route of
            UserRoute _ ->
                True

            _ ->
                False

    getUserRouteId : Route -> Maybe Int
    getUserRouteId route =
        case route of
            UserRoute id ->
                Just id

            _ ->
                Nothing

    Url.Codec.parsePath [codec] "user/123"
    --> Ok (UserRoute 123)

    Url.Codec.parsePath [codec] "user"
    --> Err SegmentNotAvailable

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
        , isThing = inner.isThing
        }


{-| An integer query parameter.

    type Route
        = UserRoute (Maybe Int)
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed UserRoute isUserRoute
            |> Url.Codec.s "user"
            |> Url.Codec.queryInt "id" getUserRouteId

    isUserRoute : Route -> Bool
    isUserRoute route =
        case route of
            UserRoute _ ->
                True

            _ ->
                False

    getUserRouteId : Route -> Maybe Int
    getUserRouteId route =
        case route of
            UserRoute maybeId ->
                maybeId

            _ ->
                Nothing

    Url.Codec.parsePath [codec] "user?id=123"
    --> Ok (UserRoute (Just 123))

    Url.Codec.parsePath [codec] "user"
    --> Ok (UserRoute Nothing)

    Url.Codec.toString [codec] (UserRoute (Just 999))
    --> Just "user?id=999"

    Url.Codec.toString [codec] (UserRoute Nothing)
    --> Just "user"

Will fail if there are multiple query parameters with the same key:

    Url.Codec.parsePath [codec] "user?id=1&id=2"
    --> Err (NeededSingleQueryParameterValueGotMultiple { got = ["1","2"], key = "id" })

Will succeed with Nothing if the query parameter contains a non-integer string:

    Url.Codec.parsePath [codec] "user?id=martin"
    --> Ok (UserRoute Nothing)

-}
queryInt :
    String
    -> (target -> Maybe Int)
    -> CodecInProgress target (Maybe Int -> parseResult)
    -> CodecInProgress target parseResult
queryInt key getter (C inner) =
    C
        { parser = Internal.queryInt key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (getter >> Maybe.map (\int_ -> ( key, [ String.fromInt int_ ] )))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        , isThing = inner.isThing
        }


{-| A string query parameter.

    type Route
        = UserRoute (Maybe String)
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed UserRoute isUserRoute
            |> Url.Codec.s "user"
            |> Url.Codec.queryString "name" getUserRouteName

    isUserRoute : Route -> Bool
    isUserRoute route =
        case route of
            UserRoute _ ->
                True

            _ ->
                False

    getUserRouteName : Route -> Maybe String
    getUserRouteName route =
        case route of
            UserRoute name ->
                name

            _ ->
                Nothing

    Url.Codec.parsePath [codec] "user?name=martin"
    --> Ok (UserRoute (Just "martin"))

    Url.Codec.toString [codec] (UserRoute (Just "john")
    --> Just "user?name=john"

Will fail if there are multiple query parameters with the same key:

    Url.Codec.parsePath [codec] "user?name=a&name=b"
    --> Err (NeededSingleQueryParameterValueGotMultiple { got = ["a","b"], key = "name" })

-}
queryString :
    String
    -> (target -> Maybe String)
    -> CodecInProgress target (Maybe String -> parseResult)
    -> CodecInProgress target parseResult
queryString key getter (C inner) =
    C
        { parser = Internal.queryString key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (getter >> Maybe.map (\str -> ( key, [ str ] )))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        , isThing = inner.isThing
        }


{-| A repeated integer query parameter.

    type Route
        = UserListingRoute (List Int)
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed UserListingRoute isUserListingRoute
            |> Url.Codec.s "users"
            |> Url.Codec.queryInts "id" getUserListingRouteIds

    isUserListingRoute : Route -> Bool
    isUserListingRoute route =
        case route of
            UserListingRoute _ ->
                True

            _ ->
                False

    getUserListingRouteIds : Route -> List Int
    getUserListingRouteIds route =
        case route of
            UserListingRoute ids ->
                ids

            _ ->
                []

    Url.Codec.parsePath [codec] "users?id=1"
    --> Ok (UserListingRoute [1])

    Url.Codec.parsePath [codec] "users?id=1&id=2&id=3"
    --> Ok (UserListingRoute [1,2,3])

    Url.Codec.parsePath [codec] "users"
    --> Ok (UserListingRoute [])

    Url.Codec.toString [codec] (UserListingRoute [])
    --> Just "user"

    Url.Codec.toString [codec] (UserListingRoute [1])
    --> Just "user?id=1"

    Url.Codec.toString [codec] (UserListingRoute [1,2])
    --> Just "user?id=1&id=2"

Will fail if given a query parameter with an empty value:

    Url.Codec.parsePath [codec] "users?id="
    --> Err (NotAllQueryParameterValuesWereInts { got = [ "" ] , key = "id" })

Will fail if any of the query parameters has a non-integer value:

    Url.Codec.parsePath [codec] "users?id=1&id=hello"
    --> Err (NotAllQueryParameterValuesWereInts { got = [ "1", "hello" ] , key = "id" })

-}
queryInts :
    String
    -> (target -> List Int)
    -> CodecInProgress target (List Int -> parseResult)
    -> CodecInProgress target parseResult
queryInts key getter (C inner) =
    C
        { parser = Internal.queryInts key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (\item -> Just ( key, List.map String.fromInt (getter item) ))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        , isThing = inner.isThing
        }


{-| A repeated string query parameter.

    type Route
        = UserListingRoute (List String)
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed UserListingRoute isUserListingRoute
            |> Url.Codec.s "users"
            |> Url.Codec.queryInts "tags" getUserListingRouteTags

    isUserListingRoute : Route -> Bool
    isUserListingRoute route =
        case route of
            UserListingRoute _ ->
                True

            _ ->
                False

    getUserListingRouteTags : Route -> List String
    getUserListingRouteTags route =
        case route of
            UserListingRoute tags ->
                tags

            _ ->
                []

    Url.Codec.parsePath [codec] "users?tags=Foo"
    --> Ok (UserListingRoute ["Foo"])

    Url.Codec.parsePath [codec] "users?tags=Foo&tags=Bar&tags=999"
    --> Ok (UserListingRoute ["Foo", "Bar", "999"])

    Url.Codec.parsePath [codec] "users"
    --> Ok (UserListingRoute [])

    Url.Codec.toString [codec] (UserListingRoute [])
    --> Just "user"

    Url.Codec.toString [codec] (UserListingRoute ["hello"])
    --> Just "user?tags=hello

    Url.Codec.toString [codec] (UserListingRoute ["hello", "111"])
    --> Just "user?tags=hello&tags=111"

Will succeed with an empty string if given a query parameter with an empty value:

    Url.Codec.parsePath [codec] "users?tags="
    --> Ok (UserListingRoute [""])

-}
queryStrings :
    String
    -> (target -> List String)
    -> CodecInProgress target (List String -> parseResult)
    -> CodecInProgress target parseResult
queryStrings key getter (C inner) =
    C
        { parser = Internal.queryStrings key inner.parser
        , toSegments = inner.toSegments
        , toQueryParams =
            (\item -> Just ( key, getter item ))
                :: inner.toQueryParams
        , toQueryFlags = inner.toQueryFlags
        , toFragment = inner.toFragment
        , isThing = inner.isThing
        }


{-| A query flag (parameter without `=` and a value), like eg. `/settings?admin`.

    type Route
        = SettingsRoute { admin : Bool }
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed (\admin -> SettingsRoute { admin = admin }) isSettingsRoute
            |> Url.Codec.s "settings"
            |> Url.Codec.queryFlag "admin" getSettingsAdminFlag

    isSettingsRoute : Route -> Bool
    isSettingsRoute route =
        case route of
            SettingsRoute _ ->
                True

            _ ->
                False

    getSettingsAdminFlag : Route -> Bool
    getSettingsAdminFlag route =
        case route of
            SettingsRoute {admin} ->
                admin

            _ ->
                False

    Url.Codec.parsePath [codec] "settings?admin"
    --> Ok (SettingsRoute { admin = True })

    Url.Codec.parsePath [codec] "settings"
    --> Ok (SettingsRoute { admin = False })

    Url.Codec.toString [codec] (SettingsRoute { admin = False })
    --> Just "settings"

    Url.Codec.toString [codec] (SettingsRoute { admin = True })
    --> Just "settings?admin"

-}
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
        , isThing = inner.isThing
        }


{-| All query flags, like eg. `/settings?admin&no-exports`.

    type Route
        = SettingsRoute (List String)
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed SettingsRoute isSettingsRoute
            |> Url.Codec.s "settings"
            |> Url.Codec.allQueryFlags getSettingsFlags

    isSettingsRoute : Route -> Bool
    isSettingsRoute route =
        case route of
            SettingsRoute _ ->
                True

            _ ->
                False

    getSettingsFlags : Route -> List String
    getSettingsFlags route =
        case route of
            SettingsRoute flags ->
                flags

            _ ->
                []

    Url.Codec.parsePath [codec] "settings?admin"
    --> Ok (SettingsRoute ["admin"])

    Url.Codec.parsePath [codec] "settings"
    --> Ok (SettingsRoute [])

    Url.Codec.parsePath [codec] "settings?admin&no-exports"
    --> Ok (SettingsRoute ["admin", "no-exports"])

    Url.Codec.toString [codec] (SettingsRoute [])
    --> Just "settings"

    Url.Codec.toString [codec] (SettingsRoute ["foo"])
    --> Just "settings?foo"

    Url.Codec.toString [codec] (SettingsRoute ["foo", "bar"])
    --> Just "settings?foo&bar"

-}
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
        , isThing = inner.isThing
        }


{-| Fragment part of the URL, eg. `/settings#HelloThereWorld`.

    type Route
        = SettingsRoute (Maybe String)
        | ...

    codec : Codec Route
    codec =
        Url.Codec.succeed SettingsRoute isSettingsRoute
            |> Url.Codec.s "settings"
            |> Url.Codec.fragment getSettingsFragment

    isSettingsRoute : Route -> Bool
    isSettingsRoute route =
        case route of
            SettingsRoute _ ->
                True

            _ ->
                False

    getSettingsFragment : Route -> Maybe String
    getSettingsFragment route =
        case route of
            SettingsRoute fragment ->
                fragment

            _ ->
                []

    Url.Codec.parsePath [codec] "settings#abc"
    --> Ok (SettingsRoute (Just "abc"))

    Url.Codec.parsePath [codec] "settings"
    --> Ok (SettingsRoute Nothing)

    Url.Codec.parsePath [codec] "settings#"
    --> Ok (SettingsRoute (Just ""))

    Url.Codec.toString [codec] (SettingsRoute (Just "abc"))
    --> Just "settings#abc"

    Url.Codec.toString [codec] (SettingsRoute Nothing)
    --> Just "settings"

    Url.Codec.toString [codec] (SettingsRoute (Just ""))
    --> Just "settings#"

-}
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
        , isThing = inner.isThing
        }

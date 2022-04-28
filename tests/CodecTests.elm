module CodecTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Url.Codec exposing (Codec, ParseError(..))



----------- EXAMPLE START -----------
-- ROUTE TYPE


type Route
    = Topic String
    | Blog Int
    | User String
    | Comment String Int



-- GETTERS (for `Url.Codec.toString*`)


getTopicSlug : Route -> Maybe String
getTopicSlug route =
    case route of
        Topic slug ->
            Just slug

        _ ->
            Nothing


getBlogPostId : Route -> Maybe Int
getBlogPostId route =
    case route of
        Blog postId ->
            Just postId

        _ ->
            Nothing


getUserName : Route -> Maybe String
getUserName route =
    case route of
        User name ->
            Just name

        _ ->
            Nothing


getCommentTopicSlug : Route -> Maybe String
getCommentTopicSlug route =
    case route of
        Comment slug _ ->
            Just slug

        _ ->
            Nothing


getCommentIndex : Route -> Maybe Int
getCommentIndex route =
    case route of
        Comment _ index ->
            Just index

        _ ->
            Nothing



-- CODECS


topicCodec : Codec Route
topicCodec =
    Url.Codec.succeed Topic
        |> Url.Codec.s "topic"
        |> Url.Codec.string getTopicSlug


blogCodec : Codec Route
blogCodec =
    Url.Codec.succeed Blog
        |> Url.Codec.s "blog"
        |> Url.Codec.int getBlogPostId


userCodec : Codec Route
userCodec =
    Url.Codec.succeed User
        |> Url.Codec.s "user"
        |> Url.Codec.string getUserName


commentCodec : Codec Route
commentCodec =
    Url.Codec.succeed Comment
        |> Url.Codec.s "topic"
        |> Url.Codec.string getCommentTopicSlug
        |> Url.Codec.s "comment"
        |> Url.Codec.int getCommentIndex


allCodecs : List (Codec Route)
allCodecs =
    [ topicCodec
    , blogCodec
    , userCodec
    , commentCodec
    ]



----------- EXAMPLE END -----------
-- TESTS


runParseCase : ( String, Result ParseError Route ) -> Test
runParseCase ( input, expectedOutput ) =
    Test.test ("\"" ++ input ++ "\" -> " ++ Debug.toString expectedOutput) <|
        \() ->
            input
                |> Url.Codec.parseOneOf allCodecs
                |> Expect.equal expectedOutput


runToStringCase : ( Route, String ) -> Test
runToStringCase ( input, expectedOutput ) =
    Test.test (Debug.toString input ++ " -> \"" ++ expectedOutput ++ "\"") <|
        \() ->
            input
                |> Url.Codec.toStringOneOf allCodecs
                |> Expect.equal (Just expectedOutput)


parseCases : List ( String, Result ParseError Route )
parseCases =
    [ ( "", Err SegmentNotAvailable )
    , ( "topic", Err SegmentNotAvailable )
    , ( "topic/hello", Ok (Topic "hello") )
    , ( "topic/123", Ok (Topic "123") )
    , ( "blog", Err SegmentNotAvailable )
    , ( "blog/hello", Err (WasNotInt "hello") )
    , ( "blog/123", Ok (Blog 123) )
    , ( "blog/-999", Ok (Blog -999) )
    , ( "user", Err SegmentNotAvailable )
    , ( "user/hello", Ok (User "hello") )
    , ( "user/123", Ok (User "123") )
    , ( "topic/comment", Ok (Topic "comment") )
    , ( "topic/123/123"
      , Err
            (SegmentMismatch
                { expected = "comment"
                , available = "123"
                }
            )
      )
    , ( "topic/123/comment", Err SegmentNotAvailable )
    , ( "topic/123/comment/hello", Err (WasNotInt "hello") )
    , ( "topic/123/comment/123", Ok (Comment "123" 123) )
    , ( "topic/hello/comment", Err SegmentNotAvailable )
    , ( "topic/hello/comment/hello", Err (WasNotInt "hello") )
    , ( "topic/hello/comment/123", Ok (Comment "hello" 123) )
    , ( "something-else"
      , Err
            (SegmentMismatch
                { expected = "topic"
                , available = "something-else"
                }
            )
      )
    ]


parseCaseInputs : List String
parseCaseInputs =
    List.map Tuple.first parseCases


toStringCases : List ( Route, String )
toStringCases =
    [ ( Topic "abc", "topic/abc" )
    , ( Topic "def", "topic/def" )
    , ( Topic "123", "topic/123" )
    , ( Blog 1, "blog/1" )
    , ( Blog 42, "blog/42" )
    , ( Blog 999, "blog/999" )
    , ( Blog -999, "blog/-999" )
    , ( User "abc", "user/abc" )
    , ( User "def", "user/def" )
    , ( User "123", "user/123" )
    , ( Comment "abc" 1, "topic/abc/comment/1" )
    , ( Comment "def" 42, "topic/def/comment/42" )
    , ( Comment "123" 999, "topic/123/comment/999" )

    -- TODO edge cases: empty strings etc.
    ]


routeFuzzer : Fuzzer Route
routeFuzzer =
    Fuzz.oneOf
        [ Fuzz.map Topic safeStringFuzzer
        , Fuzz.map Blog Fuzz.int
        , Fuzz.map User safeStringFuzzer
        , Fuzz.map2 Comment safeStringFuzzer Fuzz.int
        ]


safeCharFuzzer : Fuzzer Char
safeCharFuzzer =
    Fuzz.char
        |> Fuzz.map
            (\c ->
                if c == '/' then
                    'x'

                else
                    c
            )


nonemptyListFuzzer : Fuzzer a -> Fuzzer (List a)
nonemptyListFuzzer itemFuzzer =
    Fuzz.map2 (::) itemFuzzer (Fuzz.list itemFuzzer)


safeStringFuzzer : Fuzzer String
safeStringFuzzer =
    Fuzz.map String.fromList (nonemptyListFuzzer safeCharFuzzer)


segmentFuzzer : Fuzzer String
segmentFuzzer =
    Fuzz.oneOf
        [ Fuzz.map String.fromInt Fuzz.int
        , safeStringFuzzer
        ]


simplePathFuzzer : Fuzzer String
simplePathFuzzer =
    Fuzz.map (String.join "/") (Fuzz.list segmentFuzzer)


pathFuzzer : Fuzzer String
pathFuzzer =
    Fuzz.oneOf
        [ simplePathFuzzer
        , simplePathFuzzer |> Fuzz.map prependSlash
        , simplePathFuzzer |> Fuzz.map appendSlash
        ]


routePathFuzzer : Fuzzer String
routePathFuzzer =
    Fuzz.map (String.join "/") (Fuzz.list routeSegmentFuzzer)


routeSegmentFuzzer : Fuzzer String
routeSegmentFuzzer =
    Fuzz.oneOf
        [ Fuzz.int |> Fuzz.map String.fromInt
        , safeStringFuzzer
        , Fuzz.constant "topic"
        , Fuzz.constant "blog"
        , Fuzz.constant "user"
        , Fuzz.constant "comment"
        ]


codecFuzzer : Fuzzer (Codec Route)
codecFuzzer =
    [ topicCodec
    , blogCodec
    , userCodec
    , commentCodec
    ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


prependSlash : String -> String
prependSlash s =
    "/" ++ s


appendSlash : String -> String
appendSlash s =
    s ++ "/"


suite : Test
suite =
    Test.describe "Url.Codec"
        [ Test.describe "parseOneOf"
            [ Test.describe "Hardcoded cases" <|
                List.map runParseCase parseCases
            , Test.test "Doesn't care about leading /" <|
                \() ->
                    let
                        actual : List (Result ParseError Route)
                        actual =
                            parseCaseInputs
                                |> List.map (prependSlash >> Url.Codec.parseOneOf allCodecs)

                        expected : List (Result ParseError Route)
                        expected =
                            parseCaseInputs
                                |> List.map (Url.Codec.parseOneOf allCodecs)
                    in
                    actual
                        |> Expect.equalLists expected
            , Test.test "doesn't care about trailing /" <|
                \() ->
                    let
                        actual : List (Result ParseError Route)
                        actual =
                            parseCaseInputs
                                |> List.map (appendSlash >> Url.Codec.parseOneOf allCodecs)

                        expected : List (Result ParseError Route)
                        expected =
                            parseCaseInputs
                                |> List.map (Url.Codec.parseOneOf allCodecs)
                    in
                    actual
                        |> Expect.equalLists expected
            , Test.test "order matters - oneOf [a->b,a->c] vs oneOf [a->c,a->b]" <|
                \() ->
                    let
                        plus : Codec Int
                        plus =
                            Url.Codec.succeed (\n -> n + 1)
                                |> Url.Codec.s "id"
                                |> Url.Codec.int Just

                        times : Codec Int
                        times =
                            Url.Codec.succeed (\n -> n * 500)
                                |> Url.Codec.s "id"
                                |> Url.Codec.int Just

                        path : String
                        path =
                            "id/123"

                        plusFirst : Result ParseError Int
                        plusFirst =
                            Url.Codec.parseOneOf [ plus, times ] path

                        timesFirst : Result ParseError Int
                        timesFirst =
                            Url.Codec.parseOneOf [ times, plus ] path
                    in
                    plusFirst
                        |> Expect.notEqual timesFirst
            , Test.test "order doesn't matter - oneOf [a,a/b] vs oneOf [a/b,a]" <|
                \() ->
                    let
                        shallow : Codec ( Int, Maybe Int )
                        shallow =
                            Url.Codec.succeed (\n -> ( n, Nothing ))
                                |> Url.Codec.int (Tuple.first >> Just)

                        deep : Codec ( Int, Maybe Int )
                        deep =
                            Url.Codec.succeed (\n m -> ( n, Just m ))
                                |> Url.Codec.int (Tuple.first >> Just)
                                |> Url.Codec.int Tuple.second

                        path : String
                        path =
                            "123/456"

                        shallowFirst : Result ParseError ( Int, Maybe Int )
                        shallowFirst =
                            Url.Codec.parseOneOf [ shallow, deep ] path

                        deepFirst : Result ParseError ( Int, Maybe Int )
                        deepFirst =
                            Url.Codec.parseOneOf [ deep, shallow ] path
                    in
                    shallowFirst
                        |> Expect.equal deepFirst
            , Test.fuzz pathFuzzer "parseOneOf [] -> error" <|
                \path ->
                    Url.Codec.parseOneOf [] path
                        |> Expect.equal (Err NoCodecs)
            ]
        , Test.describe "parse"
            [ Test.fuzz2 codecFuzzer routePathFuzzer "parse x == parseOneOf [x]" <|
                \codec path ->
                    Url.Codec.parse codec path
                        |> Expect.equal (Url.Codec.parseOneOf [ codec ] path)
            ]
        , Test.describe "toStringOneOf"
            [ Test.describe "Hardcoded cases" <|
                List.map runToStringCase toStringCases
            ]
        , Test.fuzz routeFuzzer "Roundtrip" <|
            \route ->
                route
                    |> Url.Codec.toStringOneOf allCodecs
                    |> Maybe.andThen (Url.Codec.parseOneOf allCodecs >> Result.toMaybe)
                    |> Expect.equal (Just route)
        ]

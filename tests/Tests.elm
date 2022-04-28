module Tests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Url.Codec exposing (Codec, ParseError(..))


type Route
    = Topic String
    | Blog Int
    | User String
    | Comment String Int


topicCodec : Codec Route
topicCodec =
    Url.Codec.succeed Topic
        |> Url.Codec.s "topic"
        |> Url.Codec.string


blogCodec : Codec Route
blogCodec =
    Url.Codec.succeed Blog
        |> Url.Codec.s "blog"
        |> Url.Codec.int


userCodec : Codec Route
userCodec =
    Url.Codec.succeed User
        |> Url.Codec.s "user"
        |> Url.Codec.string


commentCodec : Codec Route
commentCodec =
    Url.Codec.succeed Comment
        |> Url.Codec.s "topic"
        |> Url.Codec.string
        |> Url.Codec.s "comment"
        |> Url.Codec.int


allCodecs : List (Codec Route)
allCodecs =
    [ topicCodec
    , blogCodec
    , userCodec
    , commentCodec
    ]


runCase : ( String, Result ParseError Route ) -> Test
runCase ( input, expectedOutput ) =
    Test.test ("\"" ++ input ++ "\" -> " ++ Debug.toString expectedOutput) <|
        \() ->
            input
                |> Url.Codec.parseOneOf allCodecs
                |> Expect.equal expectedOutput


cases : List ( String, Result ParseError Route )
cases =
    [ ( "", Err SegmentNotAvailable )
    , ( "topic", Err SegmentNotAvailable )
    , ( "topic/hello", Ok (Topic "hello") )
    , ( "topic/123", Ok (Topic "123") )
    , ( "blog", Err SegmentNotAvailable )
    , ( "blog/hello", Err (WasNotInt "hello") )
    , ( "blog/123", Ok (Blog 123) )
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


caseInputs : List String
caseInputs =
    List.map Tuple.first cases


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


segmentFuzzer : Fuzzer String
segmentFuzzer =
    Fuzz.oneOf
        [ Fuzz.map String.fromInt Fuzz.int
        , Fuzz.map String.fromList (nonemptyListFuzzer safeCharFuzzer)
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
                List.map runCase cases
            , Test.test "Doesn't care about leading /" <|
                \() ->
                    let
                        actual =
                            caseInputs
                                |> List.map (prependSlash >> Url.Codec.parseOneOf allCodecs)

                        expected =
                            caseInputs
                                |> List.map (Url.Codec.parseOneOf allCodecs)
                    in
                    actual
                        |> Expect.equalLists expected
            , Test.test "doesn't care about trailing /" <|
                \() ->
                    let
                        actual =
                            caseInputs
                                |> List.map (appendSlash >> Url.Codec.parseOneOf allCodecs)

                        expected =
                            caseInputs
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
                                |> Url.Codec.int

                        times : Codec Int
                        times =
                            Url.Codec.succeed (\n -> n * 500)
                                |> Url.Codec.s "id"
                                |> Url.Codec.int

                        path : String
                        path =
                            "id/123"

                        plusFirst =
                            Url.Codec.parseOneOf [ plus, times ] path

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
                                |> Url.Codec.int

                        deep : Codec ( Int, Maybe Int )
                        deep =
                            Url.Codec.succeed (\n m -> ( n, Just m ))
                                |> Url.Codec.int
                                |> Url.Codec.int

                        path : String
                        path =
                            "123/456"

                        shallowFirst =
                            Url.Codec.parseOneOf [ shallow, deep ] path

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
        ]

module SimpleParserTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)
import Url.SimpleParser exposing (ParseError(..), Parser)



----------- EXAMPLE START -----------
-- ROUTE TYPE


type Route
    = Topic String
    | Blog Int
    | User String
    | Comment String Int



-- PARSERS


topicParser : Parser Route
topicParser =
    Url.SimpleParser.succeed Topic
        |> Url.SimpleParser.s "topic"
        |> Url.SimpleParser.string


blogParser : Parser Route
blogParser =
    Url.SimpleParser.succeed Blog
        |> Url.SimpleParser.s "blog"
        |> Url.SimpleParser.int


userParser : Parser Route
userParser =
    Url.SimpleParser.succeed User
        |> Url.SimpleParser.s "user"
        |> Url.SimpleParser.string


commentParser : Parser Route
commentParser =
    Url.SimpleParser.succeed Comment
        |> Url.SimpleParser.s "topic"
        |> Url.SimpleParser.string
        |> Url.SimpleParser.s "comment"
        |> Url.SimpleParser.int


allParsers : List (Parser Route)
allParsers =
    [ topicParser
    , blogParser
    , userParser
    , commentParser
    ]


runParseCase : ( String, Result ParseError Route ) -> Test
runParseCase ( input, expectedOutput ) =
    Test.test ("\"" ++ input ++ "\" -> " ++ Debug.toString expectedOutput) <|
        \() ->
            input
                |> Url.SimpleParser.parseOneOf allParsers
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


parserFuzzer : Fuzzer (Parser Route)
parserFuzzer =
    [ topicParser
    , blogParser
    , userParser
    , commentParser
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
    Test.describe "Url.SimpleParser"
        [ Test.describe "parseOneOf"
            [ Test.describe "Hardcoded cases" <|
                List.map runParseCase cases
            , Test.test "Doesn't care about leading /" <|
                \() ->
                    let
                        actual : List (Result ParseError Route)
                        actual =
                            caseInputs
                                |> List.map (prependSlash >> Url.SimpleParser.parseOneOf allParsers)

                        expected : List (Result ParseError Route)
                        expected =
                            caseInputs
                                |> List.map (Url.SimpleParser.parseOneOf allParsers)
                    in
                    actual
                        |> Expect.equalLists expected
            , Test.test "doesn't care about trailing /" <|
                \() ->
                    let
                        actual : List (Result ParseError Route)
                        actual =
                            caseInputs
                                |> List.map (appendSlash >> Url.SimpleParser.parseOneOf allParsers)

                        expected : List (Result ParseError Route)
                        expected =
                            caseInputs
                                |> List.map (Url.SimpleParser.parseOneOf allParsers)
                    in
                    actual
                        |> Expect.equalLists expected
            , Test.test "order matters - oneOf [a->b,a->c] vs oneOf [a->c,a->b]" <|
                \() ->
                    let
                        plus : Parser Int
                        plus =
                            Url.SimpleParser.succeed (\n -> n + 1)
                                |> Url.SimpleParser.s "id"
                                |> Url.SimpleParser.int

                        times : Parser Int
                        times =
                            Url.SimpleParser.succeed (\n -> n * 500)
                                |> Url.SimpleParser.s "id"
                                |> Url.SimpleParser.int

                        path : String
                        path =
                            "id/123"

                        plusFirst : Result ParseError Int
                        plusFirst =
                            Url.SimpleParser.parseOneOf [ plus, times ] path

                        timesFirst : Result ParseError Int
                        timesFirst =
                            Url.SimpleParser.parseOneOf [ times, plus ] path
                    in
                    plusFirst
                        |> Expect.notEqual timesFirst
            , Test.test "order doesn't matter - oneOf [a,a/b] vs oneOf [a/b,a]" <|
                \() ->
                    let
                        shallow : Parser ( Int, Maybe Int )
                        shallow =
                            Url.SimpleParser.succeed (\n -> ( n, Nothing ))
                                |> Url.SimpleParser.int

                        deep : Parser ( Int, Maybe Int )
                        deep =
                            Url.SimpleParser.succeed (\n m -> ( n, Just m ))
                                |> Url.SimpleParser.int
                                |> Url.SimpleParser.int

                        path : String
                        path =
                            "123/456"

                        shallowFirst : Result ParseError ( Int, Maybe Int )
                        shallowFirst =
                            Url.SimpleParser.parseOneOf [ shallow, deep ] path

                        deepFirst : Result ParseError ( Int, Maybe Int )
                        deepFirst =
                            Url.SimpleParser.parseOneOf [ deep, shallow ] path
                    in
                    shallowFirst
                        |> Expect.equal deepFirst
            , Test.fuzz pathFuzzer "parseOneOf [] -> error" <|
                \path ->
                    Url.SimpleParser.parseOneOf [] path
                        |> Expect.equal (Err NoParsers)
            ]
        , Test.describe "parse"
            [ Test.fuzz2 parserFuzzer routePathFuzzer "parse x == parseOneOf [x]" <|
                \parser path ->
                    Url.SimpleParser.parse parser path
                        |> Expect.equal (Url.SimpleParser.parseOneOf [ parser ] path)
            ]
        ]

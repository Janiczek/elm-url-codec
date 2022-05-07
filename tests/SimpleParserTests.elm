module SimpleParserTests exposing (suite)

import Expect
import Test exposing (Test)
import TestUtils as Utils exposing (PRoute(..))
import Url.Codec.Internal as Internal
import Url.SimpleParser exposing (ParseError(..), Parser)


runParseCase : ( String, Result ParseError PRoute ) -> Test
runParseCase ( input, expectedOutput ) =
    Test.test ("\"" ++ input ++ "\" -> " ++ Debug.toString expectedOutput) <|
        \() ->
            input
                |> Url.SimpleParser.parsePath Utils.allParsers
                |> Expect.equal expectedOutput


cases : List ( String, Result ParseError PRoute )
cases =
    [ ( "", Err SegmentNotAvailable )
    , ( "topic", Err SegmentNotAvailable )
    , ( "topic/hello", Ok (PTopic "hello") )
    , ( "topic/123", Ok (PTopic "123") )
    , ( "blog", Err SegmentNotAvailable )
    , ( "blog/hello", Err (WasNotInt "hello") )
    , ( "blog/123", Ok (PBlog 123 { page = Nothing, tags = [] }) )
    , ( "blog/-999", Ok (PBlog -999 { page = Nothing, tags = [] }) )
    , ( "blog/123?page=1", Ok (PBlog 123 { page = Just 1, tags = [] }) )
    , ( "blog/123?page=1&page=2"
      , Err
            (NeededSingleQueryParameterValueGotMultiple
                { got = [ "1", "2" ]
                , key = "page"
                }
            )
      )
    , ( "blog/123?page=xyz", Ok (PBlog 123 { page = Nothing, tags = [] }) )
    , ( "blog/123?tags=1", Ok (PBlog 123 { page = Nothing, tags = [ "1" ] }) )
    , ( "blog/123?tags=1&tags=hello", Ok (PBlog 123 { page = Nothing, tags = [ "1", "hello" ] }) )
    , ( "blog/123?tags=", Ok (PBlog 123 { page = Nothing, tags = [ "" ] }) )
    , ( "user", Err SegmentNotAvailable )
    , ( "user/hello", Ok (PUser "hello" { full = False }) )
    , ( "user/123", Ok (PUser "123" { full = False }) )
    , ( "user/123?full", Ok (PUser "123" { full = True }) )
    , ( "user/123?full=", Ok (PUser "123" { full = False }) )
    , ( "user/123?full=0", Ok (PUser "123" { full = False }) )
    , ( "user/123?full=1", Ok (PUser "123" { full = False }) )
    , ( "topic/comment", Ok (PTopic "comment") )
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
    , ( "topic/123/comment/123", Ok (PComment "123" 123 { fragment = Nothing }) )
    , ( "topic/hello/comment", Err SegmentNotAvailable )
    , ( "topic/hello/comment/hello", Err (WasNotInt "hello") )
    , ( "topic/hello/comment/123", Ok (PComment "hello" 123 { fragment = Nothing }) )
    , ( "topic/hello/comment/123#ohai", Ok (PComment "hello" 123 { fragment = Just "ohai" }) )
    , ( "something-else"
      , Err
            (SegmentMismatch
                { expected = "topic"
                , available = "something-else"
                }
            )
      )

    -- percent-encoded strings
    , ( "topic/H%26M", Ok (PTopic "H&M") )
    , ( "blog/123?tags=H%26M", Ok (PBlog 123 { page = Nothing, tags = [ "H&M" ] }) )
    , ( "topic/hello/comment/123#H%26M", Ok (PComment "hello" 123 { fragment = Just "H&M" }) )
    ]


caseInputs : List String
caseInputs =
    List.map Tuple.first cases


suite : Test
suite =
    Test.describe "Url.SimpleParser"
        [ Test.describe "parse"
            [ Test.describe "Hardcoded cases" <|
                List.map runParseCase cases
            , Test.test "Doesn't care about leading /" <|
                \() ->
                    let
                        actual : List (Result ParseError PRoute)
                        actual =
                            caseInputs
                                |> List.map (Utils.prependSlash >> Url.SimpleParser.parsePath Utils.allParsers)

                        expected : List (Result ParseError PRoute)
                        expected =
                            caseInputs
                                |> List.map (Url.SimpleParser.parsePath Utils.allParsers)
                    in
                    actual
                        |> Expect.equalLists expected
            , Test.test "doesn't care about trailing /" <|
                \() ->
                    let
                        actual : List (Result ParseError PRoute)
                        actual =
                            caseInputs
                                |> List.map (Utils.appendSlash >> Url.SimpleParser.parsePath Utils.allParsers)

                        expected : List (Result ParseError PRoute)
                        expected =
                            caseInputs
                                |> List.map (Url.SimpleParser.parsePath Utils.allParsers)
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
                            Url.SimpleParser.parsePath [ plus, times ] path

                        timesFirst : Result ParseError Int
                        timesFirst =
                            Url.SimpleParser.parsePath [ times, plus ] path
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
                            Url.SimpleParser.parsePath [ shallow, deep ] path

                        deepFirst : Result ParseError ( Int, Maybe Int )
                        deepFirst =
                            Url.SimpleParser.parsePath [ deep, shallow ] path
                    in
                    shallowFirst
                        |> Expect.equal deepFirst
            , Test.fuzz Utils.pathFuzzer "parse [] -> error" <|
                \path ->
                    Url.SimpleParser.parsePath [] path
                        |> Expect.equal (Err NoParsers)
            ]
        , Test.fuzz Utils.urlFuzzer "parsePath <--> parseUrl" <|
            \url ->
                let
                    path : String
                    path =
                        Internal.constructPath url
                in
                Url.SimpleParser.parsePath Utils.allParsers path
                    |> Expect.equal (Url.SimpleParser.parseUrl Utils.allParsers url)
        ]

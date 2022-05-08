module CodecTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Set
import Test exposing (Test)
import TestUtils as Utils exposing (CRoute(..))
import Url.Codec
import Url.SimpleParser



-- TESTS


toStringCases : List ( CRoute, String )
toStringCases =
    [ ( CTopic "abc", "topic/abc" )
    , ( CTopic "def", "topic/def" )
    , ( CTopic "123", "topic/123" )
    , ( CBlog 1 { page = Nothing, tags = [] }, "blog/1" )
    , ( CBlog 1 { page = Just 123, tags = [] }, "blog/1?page=123" )
    , ( CBlog 1 { page = Nothing, tags = [ "a" ] }, "blog/1?tags=a" )
    , ( CBlog 1 { page = Nothing, tags = [ "a", "b" ] }, "blog/1?tags=a&tags=b" )
    , ( CBlog 1 { page = Just 123, tags = [ "a", "b" ] }, "blog/1?page=123&tags=a&tags=b" )
    , ( CBlog 1 { page = Nothing, tags = [ "" ] }, "blog/1?tags=" )
    , ( CBlog 42 { page = Nothing, tags = [] }, "blog/42" )
    , ( CBlog 999 { page = Nothing, tags = [] }, "blog/999" )
    , ( CBlog -999 { page = Nothing, tags = [] }, "blog/-999" )
    , ( CUser "abc" { full = False }, "user/abc" )
    , ( CUser "def" { full = True }, "user/def?full" )
    , ( CUser "123" { full = False }, "user/123" )
    , ( CComment "abc" 1 { fragment = Nothing }, "topic/abc/comment/1" )
    , ( CComment "def" 42 { fragment = Just "hello" }, "topic/def/comment/42#hello" )
    , ( CComment "123" 999 { fragment = Nothing }, "topic/123/comment/999" )
    , ( CComment "123" 999 { fragment = Just "" }, "topic/123/comment/999#" )
    , ( CSearch [], "search" )
    , ( CSearch [ 1 ], "search?id=1" )
    , ( CSearch [ 1, 2 ], "search?id=1&id=2" )
    , ( CFlags [], "flags" )
    , ( CFlags [ "x" ], "flags?x" )
    , ( CFlags [ "x", "y" ], "flags?x&y" )

    -- percent-encoded strings:
    , ( CTopic "H&M", "topic/H%26M" )
    , ( CBlog 1 { page = Nothing, tags = [ "H&M" ] }, "blog/1?tags=H%26M" )
    , ( CComment "def" 42 { fragment = Just "H&M" }, "topic/def/comment/42#H%26M" )
    ]


cRouteFuzzer : Fuzzer CRoute
cRouteFuzzer =
    Fuzz.oneOf
        [ Fuzz.map CTopic Utils.nonemptyStringFuzzer
        , Fuzz.map3 (\id page tags -> CBlog id { page = page, tags = tags })
            Fuzz.int
            (Fuzz.maybe Fuzz.int)
            (Fuzz.list Utils.nonemptyStringFuzzer)
        , Fuzz.map2 (\name full -> CUser name { full = full })
            Utils.nonemptyStringFuzzer
            Fuzz.bool
        , Fuzz.map3 (\post page fragment -> CComment post page { fragment = fragment })
            Utils.nonemptyStringFuzzer
            Fuzz.int
            (Fuzz.maybe Utils.nonemptyStringFuzzer)
        , Fuzz.map CSearch (Fuzz.list Fuzz.int)
        , Fuzz.map CFlags
            (Fuzz.list Utils.nonemptyStringFuzzer
                |> Fuzz.map (Set.fromList >> Set.toList)
            )
        ]


runToStringCase : ( CRoute, String ) -> Test
runToStringCase ( input, expectedOutput ) =
    Test.test (Debug.toString input ++ " -> \"" ++ expectedOutput ++ "\"") <|
        \() ->
            input
                |> Url.Codec.toString Utils.allCodecs
                |> Expect.equal (Just expectedOutput)


suite : Test
suite =
    Test.describe "Url.Codec"
        [ Test.fuzz Utils.pathFuzzer "parsePath <--> Url.SimpleParser.parsePath" <|
            \path ->
                Url.Codec.parsePath Utils.allCodecs path
                    |> Result.mapError Utils.cErrorToPError
                    |> Result.map Utils.cRouteToPRoute
                    |> Expect.equal (Url.SimpleParser.parsePath Utils.allParsers path)
        , Test.fuzz Utils.urlFuzzer "parseUrl <--> Url.SimpleParser.parseUrl" <|
            \url ->
                Url.Codec.parseUrl Utils.allCodecs url
                    |> Result.mapError Utils.cErrorToPError
                    |> Result.map Utils.cRouteToPRoute
                    |> Expect.equal (Url.SimpleParser.parseUrl Utils.allParsers url)
        , Test.describe "toString"
            [ Test.describe "Hardcoded cases" <|
                List.map runToStringCase toStringCases
            ]
        , Test.fuzz cRouteFuzzer "Roundtrip" <|
            \route ->
                route
                    |> Url.Codec.toString Utils.allCodecs
                    |> Maybe.andThen (Url.Codec.parsePath Utils.allCodecs >> Result.toMaybe)
                    |> Expect.equal (Just route)
        ]

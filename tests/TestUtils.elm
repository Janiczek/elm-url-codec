module TestUtils exposing (..)

import Fuzz exposing (Fuzzer)
import Url exposing (Url)
import Url.Codec exposing (Codec)
import Url.Codec.Internal as Internal
import Url.SimpleParser exposing (Parser)


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


possiblyEmptySafeStringFuzzer : Fuzzer String
possiblyEmptySafeStringFuzzer =
    Fuzz.map String.fromList (Fuzz.list safeCharFuzzer)


segmentFuzzer : Fuzzer String
segmentFuzzer =
    Fuzz.oneOf
        [ Fuzz.map String.fromInt Fuzz.int
        , safeStringFuzzer
        ]


simplePathFuzzer : Fuzzer String
simplePathFuzzer =
    Fuzz.map (String.join "/") (Fuzz.list segmentFuzzer)


pathSegmentsFuzzer : Fuzzer String
pathSegmentsFuzzer =
    Fuzz.oneOf
        [ simplePathFuzzer
        , simplePathFuzzer |> Fuzz.map prependSlash
        , simplePathFuzzer |> Fuzz.map appendSlash
        ]


pathFuzzer : Fuzzer String
pathFuzzer =
    Fuzz.map3
        (\p q f ->
            Internal.constructPath
                { path = p
                , query = q
                , fragment = f
                }
        )
        pathSegmentsFuzzer
        queryFuzzer
        fragmentFuzzer


queryFuzzer : Fuzzer (Maybe String)
queryFuzzer =
    let
        flagFuzzer : Fuzzer String
        flagFuzzer =
            safeStringFuzzer

        paramFuzzer : Fuzzer String
        paramFuzzer =
            Fuzz.map2 (\k v -> k ++ "=" ++ v)
                safeStringFuzzer
                possiblyEmptySafeStringFuzzer
    in
    Fuzz.maybe
        (Fuzz.list
            (Fuzz.oneOf
                [ flagFuzzer
                , paramFuzzer
                ]
            )
            |> Fuzz.map (String.join "&")
        )


fragmentFuzzer : Fuzzer (Maybe String)
fragmentFuzzer =
    Fuzz.maybe safeStringFuzzer


oneOfValues : List a -> Fuzzer a
oneOfValues list =
    list
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


protocolFuzzer : Fuzzer Url.Protocol
protocolFuzzer =
    oneOfValues
        [ Url.Http
        , Url.Https
        ]


hostFuzzer : Fuzzer String
hostFuzzer =
    oneOfValues
        [ "example.com"
        , "google.com"
        , "google.cz"
        , "elm-lang.org"
        ]


portFuzzer : Fuzzer (Maybe Int)
portFuzzer =
    Fuzz.maybe (Fuzz.intRange 0 65535)


urlFuzzer : Fuzzer Url
urlFuzzer =
    Fuzz.constant Url
        |> Fuzz.andMap protocolFuzzer
        |> Fuzz.andMap hostFuzzer
        |> Fuzz.andMap portFuzzer
        |> Fuzz.andMap pathSegmentsFuzzer
        |> Fuzz.andMap queryFuzzer
        |> Fuzz.andMap fragmentFuzzer


appendSlash : String -> String
appendSlash path =
    let
        handlePath p =
            p ++ "/"

        handleQuery p =
            case String.split "?" p of
                [ beforeQuery ] ->
                    handlePath beforeQuery

                [ beforeQuery, query ] ->
                    handlePath beforeQuery ++ "?" ++ query

                _ ->
                    -- won't happen
                    p
    in
    case String.split "#" path of
        [ beforeFragment ] ->
            handleQuery beforeFragment

        [ beforeFragment, fragment ] ->
            handleQuery beforeFragment ++ "#" ++ fragment

        _ ->
            -- won't happen
            path


prependSlash : String -> String
prependSlash s =
    "/" ++ s



-- PARSER EXAMPLE


type PRoute
    = PTopic String
    | PBlog Int { page : Maybe Int, tags : List String }
    | PUser String { full : Bool }
    | PComment String Int { fragment : Maybe String }



-- PARSERS


topicParser : Parser PRoute
topicParser =
    Url.SimpleParser.succeed PTopic
        |> Url.SimpleParser.s "topic"
        |> Url.SimpleParser.string


blogParser : Parser PRoute
blogParser =
    Url.SimpleParser.succeed (\id page tags -> PBlog id { page = page, tags = tags })
        |> Url.SimpleParser.s "blog"
        |> Url.SimpleParser.int
        |> Url.SimpleParser.intQuery "page"
        |> Url.SimpleParser.stringsQuery "tags"


userParser : Parser PRoute
userParser =
    Url.SimpleParser.succeed (\name full -> PUser name { full = full })
        |> Url.SimpleParser.s "user"
        |> Url.SimpleParser.string
        |> Url.SimpleParser.queryFlag "full"


commentParser : Parser PRoute
commentParser =
    Url.SimpleParser.succeed (\topic page fragment -> PComment topic page { fragment = fragment })
        |> Url.SimpleParser.s "topic"
        |> Url.SimpleParser.string
        |> Url.SimpleParser.s "comment"
        |> Url.SimpleParser.int
        |> Url.SimpleParser.fragment


allParsers : List (Parser PRoute)
allParsers =
    [ topicParser
    , blogParser
    , userParser
    , commentParser
    ]



-- CODEC EXAMPLE


type CRoute
    = CTopic String
    | CBlog Int { page : Maybe Int, tags : List String }
    | CUser String { full : Bool }
    | CComment String Int { fragment : Maybe String }



-- GETTERS (for `Url.Codec.toString`)


getTopicSlug : CRoute -> Maybe String
getTopicSlug cRoute =
    case cRoute of
        CTopic slug ->
            Just slug

        _ ->
            Nothing


getBlogPostId : CRoute -> Maybe Int
getBlogPostId cRoute =
    case cRoute of
        CBlog postId _ ->
            Just postId

        _ ->
            Nothing


getBlogPage : CRoute -> Maybe Int
getBlogPage cRoute =
    case cRoute of
        CBlog _ { page } ->
            page

        _ ->
            Nothing


getBlogTags : CRoute -> List String
getBlogTags cRoute =
    case cRoute of
        CBlog _ { tags } ->
            tags

        _ ->
            []


getUserName : CRoute -> Maybe String
getUserName cRoute =
    case cRoute of
        CUser name _ ->
            Just name

        _ ->
            Nothing


getUserQueryFlag : CRoute -> Bool
getUserQueryFlag cRoute =
    case cRoute of
        CUser _ { full } ->
            full

        _ ->
            False


getCommentTopicSlug : CRoute -> Maybe String
getCommentTopicSlug cRoute =
    case cRoute of
        CComment slug _ _ ->
            Just slug

        _ ->
            Nothing


getCommentIndex : CRoute -> Maybe Int
getCommentIndex cRoute =
    case cRoute of
        CComment _ index _ ->
            Just index

        _ ->
            Nothing


getCommentFragment : CRoute -> Maybe String
getCommentFragment cRoute =
    case cRoute of
        CComment _ _ { fragment } ->
            fragment

        _ ->
            Nothing



-- CODECS


topicCodec : Codec CRoute
topicCodec =
    Url.Codec.succeed CTopic
        |> Url.Codec.s "topic"
        |> Url.Codec.string getTopicSlug


blogCodec : Codec CRoute
blogCodec =
    Url.Codec.succeed (\id page tags -> CBlog id { page = page, tags = tags })
        |> Url.Codec.s "blog"
        |> Url.Codec.int getBlogPostId
        |> Url.Codec.intQuery "page" getBlogPage
        |> Url.Codec.stringsQuery "tags" getBlogTags


userCodec : Codec CRoute
userCodec =
    Url.Codec.succeed (\name full -> CUser name { full = full })
        |> Url.Codec.s "user"
        |> Url.Codec.string getUserName
        |> Url.Codec.queryFlag "full" getUserQueryFlag


commentCodec : Codec CRoute
commentCodec =
    Url.Codec.succeed (\topic page fragment -> CComment topic page { fragment = fragment })
        |> Url.Codec.s "topic"
        |> Url.Codec.string getCommentTopicSlug
        |> Url.Codec.s "comment"
        |> Url.Codec.int getCommentIndex
        |> Url.Codec.fragment getCommentFragment


allCodecs : List (Codec CRoute)
allCodecs =
    [ topicCodec
    , blogCodec
    , userCodec
    , commentCodec
    ]


cErrorToPError : Url.Codec.ParseError -> Url.SimpleParser.ParseError
cErrorToPError err =
    case err of
        Url.Codec.SegmentMismatch r ->
            Url.SimpleParser.SegmentMismatch r

        Url.Codec.SegmentNotAvailable ->
            Url.SimpleParser.SegmentNotAvailable

        Url.Codec.WasNotInt s ->
            Url.SimpleParser.WasNotInt s

        Url.Codec.DidNotConsumeEverything l ->
            Url.SimpleParser.DidNotConsumeEverything l

        Url.Codec.NeededSingleQueryParameterValueGotMultiple r ->
            Url.SimpleParser.NeededSingleQueryParameterValueGotMultiple r

        Url.Codec.NotAllQueryParameterValuesWereInts r ->
            Url.SimpleParser.NotAllQueryParameterValuesWereInts r

        Url.Codec.NoCodecs ->
            Url.SimpleParser.NoParsers


cRouteToPRoute : CRoute -> PRoute
cRouteToPRoute cRoute =
    case cRoute of
        CTopic s ->
            PTopic s

        CBlog i r ->
            PBlog i r

        CUser s r ->
            PUser s r

        CComment s i r ->
            PComment s i r

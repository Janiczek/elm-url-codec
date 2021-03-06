module TestUtils exposing (..)

import Fuzz exposing (Fuzzer)
import Url exposing (Url)
import Url.Codec exposing (Codec)
import Url.Codec.Internal as Internal
import Url.SimpleParser exposing (Parser)


nonemptyListFuzzer : Fuzzer a -> Fuzzer (List a)
nonemptyListFuzzer itemFuzzer =
    Fuzz.map2 (::) itemFuzzer (Fuzz.list itemFuzzer)


nonemptyStringFuzzer : Fuzzer String
nonemptyStringFuzzer =
    Fuzz.map String.fromList (nonemptyListFuzzer Fuzz.char)


segmentFuzzer : Fuzzer String
segmentFuzzer =
    Fuzz.oneOf
        [ Fuzz.map String.fromInt Fuzz.int
        , nonemptyStringFuzzer
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
            nonemptyStringFuzzer

        paramFuzzer : Fuzzer String
        paramFuzzer =
            Fuzz.map2 (\k v -> k ++ "=" ++ v)
                nonemptyStringFuzzer
                Fuzz.string
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
    Fuzz.maybe nonemptyStringFuzzer


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
        handlePath : String -> String
        handlePath p =
            p ++ "/"

        handleQuery : String -> String
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
    | PSearch (Maybe String) (List Int)
    | PFlags (List String)



-- PARSERS


allParsers : List (Parser PRoute)
allParsers =
    [ topicParser
    , blogParser
    , userParser
    , commentParser
    , searchParser
    , flagsParser
    ]


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
        |> Url.SimpleParser.queryInt "page"
        |> Url.SimpleParser.queryStrings "tags"


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


searchParser : Parser PRoute
searchParser =
    Url.SimpleParser.succeed PSearch
        |> Url.SimpleParser.s "search"
        |> Url.SimpleParser.queryString "term"
        |> Url.SimpleParser.queryInts "id"


flagsParser : Parser PRoute
flagsParser =
    Url.SimpleParser.succeed PFlags
        |> Url.SimpleParser.s "flags"
        |> Url.SimpleParser.allQueryFlags



-- CODEC EXAMPLE


type CRoute
    = CTopic String
    | CBlog Int { page : Maybe Int, tags : List String }
    | CUser String { full : Bool }
    | CComment String Int { fragment : Maybe String }
    | CSearch (Maybe String) (List Int)
    | CFlags (List String)



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


getSearchTerm : CRoute -> Maybe String
getSearchTerm cRoute =
    case cRoute of
        CSearch term _ ->
            term

        _ ->
            Nothing


getSearchIds : CRoute -> List Int
getSearchIds cRoute =
    case cRoute of
        CSearch _ ids ->
            ids

        _ ->
            []


getFlags : CRoute -> List String
getFlags cRoute =
    case cRoute of
        CFlags flags ->
            flags

        _ ->
            []



-- PREDICATES


isCTopic : CRoute -> Bool
isCTopic cRoute =
    case cRoute of
        CTopic _ ->
            True

        _ ->
            False


isCBlog : CRoute -> Bool
isCBlog cRoute =
    case cRoute of
        CBlog _ _ ->
            True

        _ ->
            False


isCUser : CRoute -> Bool
isCUser cRoute =
    case cRoute of
        CUser _ _ ->
            True

        _ ->
            False


isCComment : CRoute -> Bool
isCComment cRoute =
    case cRoute of
        CComment _ _ _ ->
            True

        _ ->
            False


isCSearch : CRoute -> Bool
isCSearch cRoute =
    case cRoute of
        CSearch _ _ ->
            True

        _ ->
            False


isCFlags : CRoute -> Bool
isCFlags cRoute =
    case cRoute of
        CFlags _ ->
            True

        _ ->
            False



-- CODECS


allCodecs : List (Codec CRoute)
allCodecs =
    [ topicCodec
    , blogCodec
    , userCodec
    , commentCodec
    , searchCodec
    , flagsCodec
    ]


topicCodec : Codec CRoute
topicCodec =
    Url.Codec.succeed CTopic isCTopic
        |> Url.Codec.s "topic"
        |> Url.Codec.string getTopicSlug


blogCodec : Codec CRoute
blogCodec =
    Url.Codec.succeed (\id page tags -> CBlog id { page = page, tags = tags }) isCBlog
        |> Url.Codec.s "blog"
        |> Url.Codec.int getBlogPostId
        |> Url.Codec.queryInt "page" getBlogPage
        |> Url.Codec.queryStrings "tags" getBlogTags


userCodec : Codec CRoute
userCodec =
    Url.Codec.succeed (\name full -> CUser name { full = full }) isCUser
        |> Url.Codec.s "user"
        |> Url.Codec.string getUserName
        |> Url.Codec.queryFlag "full" getUserQueryFlag


commentCodec : Codec CRoute
commentCodec =
    Url.Codec.succeed (\topic page fragment -> CComment topic page { fragment = fragment }) isCComment
        |> Url.Codec.s "topic"
        |> Url.Codec.string getCommentTopicSlug
        |> Url.Codec.s "comment"
        |> Url.Codec.int getCommentIndex
        |> Url.Codec.fragment getCommentFragment


searchCodec : Codec CRoute
searchCodec =
    Url.Codec.succeed CSearch isCSearch
        |> Url.Codec.s "search"
        |> Url.Codec.queryString "term" getSearchTerm
        |> Url.Codec.queryInts "id" getSearchIds


flagsCodec : Codec CRoute
flagsCodec =
    Url.Codec.succeed CFlags isCFlags
        |> Url.Codec.s "flags"
        |> Url.Codec.allQueryFlags getFlags


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

        CSearch t i ->
            PSearch t i

        CFlags f ->
            PFlags f

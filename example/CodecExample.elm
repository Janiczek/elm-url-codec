module CodecExample exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Parser exposing (Codec)


type Route
    = Topic String
    | Blog Int { page : Maybe Int, tags : List String }
    | User String { full : Bool }
    | Comment String Int { fragment : Maybe String }
    | Search (Maybe String) (List Int)
    | Flags (List String)



-- TOP LEVEL FUNCTIONS


fromUrl : Url -> Result Parser.ParseError Route
fromUrl url =
    Parser.parseUrl allCodecs url


toString : Route -> Maybe String
toString route =
    Parser.toString allCodecs route



-- CODECS


allCodecs : List (Codec Route)
allCodecs =
    [ topicCodec
    , blogCodec
    , userCodec
    , commentCodec
    , searchCodec
    , flagsCodec
    ]


topicCodec : Codec Route
topicCodec =
    Parser.succeed Topic isTopicRoute
        |> Parser.s "topic"
        |> Parser.string getTopicSlug


blogCodec : Codec Route
blogCodec =
    Parser.succeed (\id page tags -> Blog id { page = page, tags = tags }) isBlogRoute
        |> Parser.s "blog"
        |> Parser.int getBlogPostId
        |> Parser.queryInt "page" getBlogPage
        |> Parser.queryStrings "tags" getBlogTags


userCodec : Codec Route
userCodec =
    Parser.succeed (\name full -> User name { full = full }) isUserRoute
        |> Parser.s "user"
        |> Parser.string getUserName
        |> Parser.queryFlag "full" getUserQueryFlag


commentCodec : Codec Route
commentCodec =
    Parser.succeed (\topic page fragment -> Comment topic page { fragment = fragment }) isCommentRoute
        |> Parser.s "topic"
        |> Parser.string getCommentTopicSlug
        |> Parser.s "comment"
        |> Parser.int getCommentIndex
        |> Parser.fragment getCommentFragment


searchCodec : Codec Route
searchCodec =
    Parser.succeed Search isSearchRoute
        |> Parser.s "search"
        |> Parser.queryString "term" getSearchTerm
        |> Parser.queryInts "id" getSearchIds


flagsCodec : Codec Route
flagsCodec =
    Parser.succeed Flags isFlagsRoute
        |> Parser.s "flags"
        |> Parser.allQueryFlags getFlags



-- PREDICATES


isTopicRoute : Route -> Bool
isTopicRoute route =
    case route of
        Topic _ ->
            True

        _ ->
            False


isBlogRoute : Route -> Bool
isBlogRoute route =
    case route of
        Blog _ _ ->
            True

        _ ->
            False


isUserRoute : Route -> Bool
isUserRoute route =
    case route of
        User _ _ ->
            True

        _ ->
            False


isCommentRoute : Route -> Bool
isCommentRoute route =
    case route of
        Comment _ _ _ ->
            True

        _ ->
            False


isSearchRoute : Route -> Bool
isSearchRoute route =
    case route of
        Search _ _ ->
            True

        _ ->
            False


isFlagsRoute : Route -> Bool
isFlagsRoute route =
    case route of
        Flags _ ->
            True

        _ ->
            False



-- GETTERS


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
        Blog postId _ ->
            Just postId

        _ ->
            Nothing


getBlogPage : Route -> Maybe Int
getBlogPage route =
    case route of
        Blog _ { page } ->
            page

        _ ->
            Nothing


getBlogTags : Route -> List String
getBlogTags route =
    case route of
        Blog _ { tags } ->
            tags

        _ ->
            []


getUserName : Route -> Maybe String
getUserName route =
    case route of
        User name _ ->
            Just name

        _ ->
            Nothing


getUserQueryFlag : Route -> Bool
getUserQueryFlag route =
    case route of
        User _ { full } ->
            full

        _ ->
            False


getCommentTopicSlug : Route -> Maybe String
getCommentTopicSlug route =
    case route of
        Comment slug _ _ ->
            Just slug

        _ ->
            Nothing


getCommentIndex : Route -> Maybe Int
getCommentIndex route =
    case route of
        Comment _ index _ ->
            Just index

        _ ->
            Nothing


getCommentFragment : Route -> Maybe String
getCommentFragment route =
    case route of
        Comment _ _ { fragment } ->
            fragment

        _ ->
            Nothing


getSearchTerm : Route -> Maybe String
getSearchTerm route =
    case route of
        Search term _ ->
            term

        _ ->
            Nothing


getSearchIds : Route -> List Int
getSearchIds route =
    case route of
        Search _ ids ->
            ids

        _ ->
            []


getFlags : Route -> List String
getFlags route =
    case route of
        Flags flags ->
            flags

        _ ->
            []

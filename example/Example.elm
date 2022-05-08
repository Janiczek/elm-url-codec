module Example exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Codec exposing (Codec)


type Route
    = Topic String
    | Blog Int { page : Maybe Int, tags : List String }
    | User String { full : Bool }
    | Comment String Int { fragment : Maybe String }
    | Search (Maybe String) (List Int)
    | Flags (List String)



-- TOP LEVEL FUNCTIONS


fromUrl : Url -> Result Url.Codec.ParseError Route
fromUrl url =
    Url.Codec.parseUrl allCodecs url


toString : Route -> Maybe String
toString route =
    Url.Codec.toString allCodecs route



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
    Url.Codec.succeed Topic isTopicRoute
        |> Url.Codec.s "topic"
        |> Url.Codec.string getTopicSlug


blogCodec : Codec Route
blogCodec =
    Url.Codec.succeed (\id page tags -> Blog id { page = page, tags = tags }) isBlogRoute
        |> Url.Codec.s "blog"
        |> Url.Codec.int getBlogPostId
        |> Url.Codec.queryInt "page" getBlogPage
        |> Url.Codec.queryStrings "tags" getBlogTags


userCodec : Codec Route
userCodec =
    Url.Codec.succeed (\name full -> User name { full = full }) isUserRoute
        |> Url.Codec.s "user"
        |> Url.Codec.string getUserName
        |> Url.Codec.queryFlag "full" getUserQueryFlag


commentCodec : Codec Route
commentCodec =
    Url.Codec.succeed (\topic page fragment -> Comment topic page { fragment = fragment }) isCommentRoute
        |> Url.Codec.s "topic"
        |> Url.Codec.string getCommentTopicSlug
        |> Url.Codec.s "comment"
        |> Url.Codec.int getCommentIndex
        |> Url.Codec.fragment getCommentFragment


searchCodec : Codec Route
searchCodec =
    Url.Codec.succeed Search isSearchRoute
        |> Url.Codec.s "search"
        |> Url.Codec.queryString "term" getSearchTerm
        |> Url.Codec.queryInts "id" getSearchIds


flagsCodec : Codec Route
flagsCodec =
    Url.Codec.succeed Flags isFlagsRoute
        |> Url.Codec.s "flags"
        |> Url.Codec.allQueryFlags getFlags



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

module Example exposing (Route(..), fromUrl, toString)

import Url exposing (Url)
import Url.Codec exposing (Codec)


type Route
    = Topic String
    | Blog Int { page : Maybe Int, tags : List String }
    | User String { full : Bool }
    | Comment String Int { fragment : Maybe String }



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
    ]


topicCodec : Codec Route
topicCodec =
    Url.Codec.succeed Topic
        |> Url.Codec.s "topic"
        |> Url.Codec.string getTopicSlug


blogCodec : Codec Route
blogCodec =
    Url.Codec.succeed (\id page tags -> Blog id { page = page, tags = tags })
        |> Url.Codec.s "blog"
        |> Url.Codec.int getBlogPostId
        |> Url.Codec.queryInt "page" getBlogPage
        |> Url.Codec.queryStrings "tags" getBlogTags


userCodec : Codec Route
userCodec =
    Url.Codec.succeed (\name full -> User name { full = full })
        |> Url.Codec.s "user"
        |> Url.Codec.string getUserName
        |> Url.Codec.queryFlag "full" getUserQueryFlag


commentCodec : Codec Route
commentCodec =
    Url.Codec.succeed (\topic page fragment -> Comment topic page { fragment = fragment })
        |> Url.Codec.s "topic"
        |> Url.Codec.string getCommentTopicSlug
        |> Url.Codec.s "comment"
        |> Url.Codec.int getCommentIndex
        |> Url.Codec.fragment getCommentFragment



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

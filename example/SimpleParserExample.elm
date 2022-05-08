module Example exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.SimpleParser as Parser exposing (Parser)


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
    Parser.parseUrl allParsers url



-- ParserS


allParsers : List (Parser Route)
allParsers =
    [ topicParser
    , blogParser
    , userParser
    , commentParser
    , searchParser
    , flagsParser
    ]


topicParser : Parser Route
topicParser =
    Parser.succeed Topic
        |> Parser.s "topic"
        |> Parser.string


blogParser : Parser Route
blogParser =
    Parser.succeed (\id page tags -> Blog id { page = page, tags = tags })
        |> Parser.s "blog"
        |> Parser.int
        |> Parser.queryInt "page"
        |> Parser.queryStrings "tags"


userParser : Parser Route
userParser =
    Parser.succeed (\name full -> User name { full = full })
        |> Parser.s "user"
        |> Parser.string
        |> Parser.queryFlag "full"


commentParser : Parser Route
commentParser =
    Parser.succeed (\topic page fragment -> Comment topic page { fragment = fragment })
        |> Parser.s "topic"
        |> Parser.string
        |> Parser.s "comment"
        |> Parser.int
        |> Parser.fragment


searchParser : Parser Route
searchParser =
    Parser.succeed Search
        |> Parser.s "search"
        |> Parser.queryString "term"
        |> Parser.queryInts "id"


flagsParser : Parser Route
flagsParser =
    Parser.succeed Flags
        |> Parser.s "flags"
        |> Parser.allQueryFlags

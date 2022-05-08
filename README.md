# `Janiczek/elm-url-codec`

A simpler alternative to the [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/)
package.

* **[`Url.Codec`](Url-Codec)** allows you to define both the URL parser
  and the URL builder at the same time.
* **[`Url.SimpleParser`](Url-SimpleParser)** only deals with parsing, but
  has a nicer API than [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/).

Accepts both the [`Url`](https://package.elm-lang.org/packages/elm/url/latest/Url#Url)
type and strings like:

```
post/hello-world/comments/2
post/hello-world/comments/2/
/post/hello-world/comments/2
/post/hello-world/comments/2/

/path?admin=true
/path?admin=true&fireZeMissiles=1
/path?highlight=123&highlight=154
/path?foo&bar

/path#something
```

## Example

```elm
import Url.Codec exposing (Codec)


type Route
    = Topic String
    | Blog Int String


topicCodec : Codec Route
topicCodec =
    Url.Codec.succeed Topic isTopicRoute
        |> Url.Codec.s "topic"
        |> Url.Codec.string getTopicSlug


--     needed for Url.Codec
-- not needed for Url.SimpleParser
getTopicSlug : Route -> Maybe String
getTopicSlug route =
    case route of
        Topic slug ->
            Just slug

        _ ->
            Nothing

--     needed for Url.Codec
-- not needed for Url.SimpleParser
isTopicRoute : Route -> Maybe String
isTopicRoute route =
    case route of
        Topic _ ->
            True

        _ ->
            False

Url.Codec.parsePath [topicCodec] "topic/hello-world"
--> Ok (Topic "hello-world")


Url.Codec.toString [topicCodec] (Topic "foo-bar-baz")
--> Just "topic/foo-bar-baz"
```

For a more fleshed-out example see [example/CodecExample.elm](https://github.com/Janiczek/elm-url-codec/blob/e895e7301e0c2f73dc36427698ed5ee905e1a120/example/CodecExample.elm) and [example/SimpleParserExample.elm](https://github.com/Janiczek/elm-url-codec/blob/e895e7301e0c2f73dc36427698ed5ee905e1a120/example/SimpleParserExample.elm).

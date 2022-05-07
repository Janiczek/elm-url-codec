# `Janiczek/elm-url-codec`

A simpler alternative to the [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/)
package.

* **[`Url.Codec`](Url-Codec)** allows you to define both the URL parser
  and the URL builder at the same time.
* **[`Url.SimpleParser`](Url-SimpleParser)** only deals with parsing, but
  has a nicer API than [`elm/url`](https://package.elm-lang.org/packages/elm/url/latest/).

Accepts the [`Url`](https://package.elm-lang.org/packages/elm/url/latest/Url#Url) type and strings like:

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
    Url.Codec.succeed Topic
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


Url.Codec.parsePath [topicCodec] "topic/hello-world"
--> Ok (Topic "hello-world")


Url.Codec.toString [topicCodec] (Topic "foo-bar-baz")
--> Just "topic/foo-bar-baz"
```

For more fleshed-out example see [example/Example.elm](https://github.com/Janiczek/elm-url-codec/blob/b9dc96a3e4788b2392c6aec3126282fe6085c7b7/example/Example.elm)

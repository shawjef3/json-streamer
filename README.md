# Json Streaming

This library provides methods to produce Iterators or ZStreams from any source of JSON data that Jackson can read.
It has the advantage that you can choose at each level if you want to build a JSON objec or array, emit the raw value,
or skip the value. You can, for example, handle an infinite JSON array from standard input.


## Examples

### Streaming all top level values

```scala
import com.fasterxml.jackson.core.JsonParser
import io.circe.Json
import me.jeffshaw.json.JsonIterator

val parser: JsonParser = ???

val tokens: Iterator[Token] = JsonIterator.tokens(parser)

val jsons: Iterator[Json] = JsonIterator.jsons(Decider.Build, tokens)
```

or equivalently

```scala
import com.fasterxml.jackson.core.JsonParser
import io.circe.Json
import me.jeffshaw.json.JsonIterator

val parser: JsonParser = ???

val jsons: Iterator[Json] = JsonIterator.jsons(Decider.Build, parser)
```

Then at each stage in the iterator, you can do whatever you would normally want to do if you weren't streaming.

### Streaming all second level values

This will emit root values to the resulting iterator, but not include them in any json array or object.

For instance, the following json will give the resulting iterator. Note that each element is given with its path.

```json
{"a": 0, "b": [0, 1]}
```

```scala
import com.fasterxml.jackson.core.{JsonFactory, JsonParser}
import io.circe.Json
import me.jeffshaw.json.JsonIterator

val parser: JsonParser = new JsonFactory().createParser("""{"a": 0, "b": [0, 1]}""")

val jsons: Iterator[(Path, Json)] = JsonIterator.jsons(Decider.streamUntilDepth(1), parser)
```

```scala
Iterator(($.a,0), ($.b,[0, 1]))
```

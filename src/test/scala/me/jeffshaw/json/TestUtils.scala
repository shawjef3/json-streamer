package me.jeffshaw.json

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder, JsonParser, StreamReadConstraints}
import java.io.InputStream
import scala.collection.JavaConverters._

trait TestUtils {

  val objectCount: Int = 1000000

  val jsonFactory: JsonFactory =
    new JsonFactoryBuilder().build()
      .setStreamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(objectCount).build())

  lazy val exampleJson: String = {
    val in = scala.io.Source.fromResource("example.json")
    try {
      in.mkString
    } finally {
      in.close()
    }
  }

  def getInputStream(strings: Stream[String]): InputStream = {
    new InputStream() {
      val ints: Iterator[Integer] = strings.iterator.flatMap(_.codePoints().iterator.asScala)
      override def read(): Int = {
        if (ints.hasNext) {
          ints.next()
        } else {
          -1
        }
      }
    }
  }

  def getParser(tokens: Stream[String]): JsonParser = {
    jsonFactory.createParser(getInputStream(tokens))
  }

  def serialObjects: JsonParser = {
    getParser(Stream.fill(objectCount)("{}"))
  }

  def embeddedObjects: JsonParser = {
    val head = "{\"a\":"
    val middle = "0"
    val tail = "}"
    getParser(Stream.fill(objectCount)(head) ++ Stream(middle) ++ Stream.fill(objectCount)(tail))
  }

  def serialArrays: JsonParser = {
    getParser(Stream.fill(objectCount)("[]"))
  }

  def embeddedArrays: JsonParser = {
    getParser(Stream.fill(objectCount)("[") ++ Stream.fill(objectCount)("]"))
  }

}

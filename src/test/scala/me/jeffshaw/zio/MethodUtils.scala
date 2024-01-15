package me.jeffshaw.zio

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder, JsonParser, StreamReadConstraints}
import java.io.InputStream
import scala.jdk.CollectionConverters._

trait MethodUtils {

  val objectCount = 1_000_000

  val jsonFactory: JsonFactory =
    new JsonFactoryBuilder().build()
      .setStreamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(objectCount).build())

  def getInputStream(strings: LazyList[String]): InputStream = {
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

  def getParser(tokens: LazyList[String]): JsonParser = {
    jsonFactory.createParser(getInputStream(tokens))
  }

  def serialObjects: JsonParser = {
    getParser(LazyList.fill(objectCount)("{}"))
  }

  def embeddedObjects: JsonParser = {
    val head = "{\"a\":"
    val middle = "0"
    val tail = "}"
    getParser(LazyList.fill(objectCount)(head) ++ LazyList(middle) ++ LazyList.fill(objectCount)(tail))
  }

  def serialArrays: JsonParser = {
    getParser(LazyList.fill(objectCount)("[]"))
  }

  def embeddedArrays: JsonParser = {
    getParser(LazyList.fill(objectCount)("[") ++ LazyList.fill(objectCount)("]"))
  }

}

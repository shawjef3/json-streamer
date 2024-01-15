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

  def serialObjects: JsonParser = {
    jsonFactory.createParser(getInputStream(LazyList.fill(objectCount)("{}")))
  }

  def embeddedObjects: JsonParser = {
    val head = "{\"a\":"
    val middle = "0"
    val tail = "}"
    jsonFactory.createParser(getInputStream(LazyList.fill(objectCount)(head) ++ LazyList(middle) ++ LazyList.fill(objectCount)(tail)))
  }

  def serialArrays: JsonParser = {
    jsonFactory.createParser(getInputStream(LazyList.fill(objectCount)("[]")))
  }

  def embeddedArrays: JsonParser = {
    jsonFactory.createParser(getInputStream(LazyList.fill(objectCount)("[") ++ LazyList.fill(objectCount)("]")))
  }

}

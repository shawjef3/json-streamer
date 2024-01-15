package me.jeffshaw.zio

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder, StreamReadConstraints}
import java.io.InputStream
import org.scalatest.funsuite.AnyFunSuite
import zio.{Chunk, Runtime, Unsafe}

class ValuedJsonTokenSpec extends AnyFunSuite {

  val objectCount = 1_000_000

  val jsonFactory: JsonFactory =
     new JsonFactoryBuilder().build()
       .setStreamReadConstraints(StreamReadConstraints.builder().maxNestingDepth(objectCount).build())

  def getInputStream(strings: LazyList[String]): InputStream = {
    var ints: LazyList[Int] = strings.flatMap(_.codePoints().toArray)
    new InputStream() {
      override def read(): Int = {
        if (ints.isEmpty) {
          -1
        } else {
          val result = ints.head
          ints = ints.tail
          result
        }
      }
    }
  }

  test("empty object") {
    val string = "{}"
    val p = jsonFactory.createParser(string)
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.stream(p).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject))(js.getOrThrowFiberFailure())
    }
  }

  test("lots of serial objects") {
    val p = jsonFactory.createParser(getInputStream(LazyList.fill(objectCount)("{}")))
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.stream(p).runCount
        }
      assert(js.isSuccess)
      assertResult(objectCount * 2)(js.getOrThrowFiberFailure())
    }
  }

  test("lots of embedded objects") {
    val head = "{\"a\":"
    val middle = "0"
    val tail = "}"
    val in = getInputStream(LazyList.fill(objectCount)(head) ++ LazyList(middle) ++ LazyList.fill(objectCount)(tail))
    val p = jsonFactory.createParser(in)
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.stream(p).runCount
        }
      assert(js.isSuccess)
      assertResult(3 * objectCount + 1)(js.getOrThrowFiberFailure())
    }
  }

  test("lots of serial arrays") {
    val in = getInputStream(LazyList.fill(objectCount)("[]"))
    val p = jsonFactory.createParser(in)
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.stream(p).runCount
        }
      assert(js.isSuccess)
      assertResult(objectCount * 2)(js.getOrThrowFiberFailure())
    }
  }

  test("lots of embedded arrays") {
    val in = getInputStream(LazyList.fill(objectCount)("[") ++ LazyList.fill(objectCount)("]"))

    val p = jsonFactory.createParser(in)
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.stream(p).runCount
        }
      assert(js.isSuccess)
      assertResult(objectCount * 2)(js.getOrThrowFiberFailure())
    }
  }

}

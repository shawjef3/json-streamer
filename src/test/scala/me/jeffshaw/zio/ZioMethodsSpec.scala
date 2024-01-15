package me.jeffshaw.zio

import org.scalatest.funsuite.AnyFunSuite
import zio.{Chunk, Runtime, Unsafe}

class ZioMethodsSpec extends AnyFunSuite with MethodUtils {

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
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          ZioMethods.stream(serialObjects).runCount
        }
      assert(count.isSuccess)
      assertResult(objectCount * 2)(count.getOrThrowFiberFailure())
    }
  }

  test("lots of embedded objects") {
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          ZioMethods.stream(embeddedObjects).runCount
        }
      assert(count.isSuccess)
      assertResult(3 * objectCount + 1)(count.getOrThrowFiberFailure())
    }
  }

  test("lots of serial arrays") {
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          ZioMethods.stream(serialArrays).runCount
        }
      assert(count.isSuccess)
      assertResult(objectCount * 2)(count.getOrThrowFiberFailure())
    }
  }

  test("lots of embedded arrays") {
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          ZioMethods.stream(embeddedArrays).runCount
        }
      assert(count.isSuccess)
      assertResult(objectCount * 2)(count.getOrThrowFiberFailure())
    }
  }

}

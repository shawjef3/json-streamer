package me.jeffshaw.json

import org.scalatest.funsuite.AnyFunSuite
import zio.{Chunk, Runtime, Unsafe}

class JsonZioSpec extends AnyFunSuite with TestUtils {

  test("empty object") {
    val string = "{}"
    val p = jsonFactory.createParser(string)
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          JsonZio.tokens(p).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk(Token.StartObject, Token.EndObject))(js.getOrThrowFiberFailure())
    }
  }

  test("lots of serial objects") {
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          JsonZio.tokens(serialObjects).runCount
        }
      assert(count.isSuccess)
      assertResult(objectCount * 2)(count.getOrThrowFiberFailure())
    }
  }

  test("lots of embedded objects") {
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          JsonZio.tokens(embeddedObjects).runCount
        }
      assert(count.isSuccess)
      assertResult(3 * objectCount + 1)(count.getOrThrowFiberFailure())
    }
  }

  test("lots of serial arrays") {
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          JsonZio.tokens(serialArrays).runCount
        }
      assert(count.isSuccess)
      assertResult(objectCount * 2)(count.getOrThrowFiberFailure())
    }
  }

  test("lots of embedded arrays") {
    Unsafe.unsafe { implicit unsafe =>
      val count =
        Runtime.default.unsafe.run {
          JsonZio.tokens(embeddedArrays).runCount
        }
      assert(count.isSuccess)
      assertResult(objectCount * 2)(count.getOrThrowFiberFailure())
    }
  }

}

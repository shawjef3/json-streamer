package me.jeffshaw.zio

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import zio.{Chunk, Runtime, Unsafe}
import zio.stream.ZStream

class StateStreamingSpec extends AnyFunSuite with TestUtils {

  test("Init") {
    assertResult(State.BuildingObject(ObjectDecision.Build, State.Init))(State.Init.nextState(Decider.Build, ValuedJsonToken.StartObject))
  }

  test("build trivial object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Build, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, Json.obj())))(js.getOrThrowFiberFailure())
    }
  }

  test("build single value object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Build, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, Json.obj("hi" -> Json.fromInt(3)))))(js.getOrThrowFiberFailure())
    }
  }

  test("build complex object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Build, ZioMethods.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, Json.obj("a" -> Json.fromInt(0), "b" -> Json.arr(Json.fromInt(1)), "c" -> Json.obj("d" -> Json.fromInt(2))))))(js.getOrThrowFiberFailure())
    }
  }

  test("build two complex objects") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Build, ZioMethods.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.fill(2)((Path.root, Json.obj("a" -> Json.fromInt(0), "b" -> Json.arr(Json.fromInt(1)), "c" -> Json.obj("d" -> Json.fromInt(2))))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream trivial object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Stream, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("stream single value object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Stream, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.field("hi"), Json.fromInt(3))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream complex object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.streamUntilDepth(1), ZioMethods.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.field("a"), Json.fromInt(0)), (Path.root.field("b"), Json.arr(Json.fromInt(1))), (Path.root.field("c"), Json.obj("d" -> Json.fromInt(2)))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream two complex objects") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.streamUntilDepth(1), ZioMethods.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.fill(2)(Chunk((Path.root.field("a"), Json.fromInt(0)), (Path.root.field("b"), Json.arr(Json.fromInt(1))), (Path.root.field("c"), Json.obj("d" -> Json.fromInt(2))))).flatten)(js.getOrThrowFiberFailure())
    }
  }

  test("test trivial object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Ignore, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("test single value object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Ignore, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("single value array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Build, ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(3), ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, Json.arr(Json.fromInt(3)))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream empty array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Stream, ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("stream single value array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Stream, ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(3), ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.index(0), Json.fromInt(3))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream until inner array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.streamUntilDepth(1), ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(0), ValuedJsonToken.StartArray, ValuedJsonToken.JInt(1), ValuedJsonToken.EndArray, ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.index(0), Json.fromInt(0)), (Path.root.index(1), Json.arr(Json.fromInt(1)))))(js.getOrThrowFiberFailure())
    }
  }

  test("ignore until inner array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.buildAtDepth(2), ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(0), ValuedJsonToken.StartArray, ValuedJsonToken.JInt(1), ValuedJsonToken.EndArray, ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.index(1).index(0), Json.fromInt(1))))(js.getOrThrowFiberFailure())
    }
  }

  test("same as json parser") {
    import io.circe.parser._
    val native: Json = parse(exampleJson).getOrElse(throw new Exception("invalid json"))
    val streamed: Chunk[(Path, Json)] = {
      Unsafe.unsafe { implicit unsafe =>
        Runtime.default.unsafe.run {
          ZioMethods.toJsons(Decider.Build, ZioMethods.stream(jsonFactory.createParser(exampleJson))).runCollect
        }.getOrThrow()
      }
    }
    assertResult(Chunk(Path.root -> native))(streamed)
  }
}

package me.jeffshaw.zio

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder}
import org.json4s.{JArray, JInt, JObject, JValue}
import org.scalatest.funsuite.AnyFunSuite
import zio.{Chunk, Runtime, Unsafe}
import zio.stream.ZStream

class StateStreamingSpec extends AnyFunSuite {

  val jsonFactory: JsonFactory = new JsonFactoryBuilder().build()

  test("Init") {
    assertResult(State.BuildingObject(ObjectDecision.Build, State.Init))(State.Init.nextState(Decider.Build, ValuedJsonToken.StartObject))
  }

  test("build trivial object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Build, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, JObject())))(js.getOrThrowFiberFailure())
    }
  }

  test("build single value object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Build, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, JObject("hi" -> JInt(3)))))(js.getOrThrowFiberFailure())
    }
  }

  test("build complex object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Build, ValuedJsonToken.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, JObject("a" -> JInt(0), "b" -> JArray(List(JInt(1))), "c" -> JObject("d" -> JInt(2))))))(js.getOrThrowFiberFailure())
    }
  }

  test("build two complex objects") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Build, ValuedJsonToken.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.fill(2)((Path.root, JObject("a" -> JInt(0), "b" -> JArray(List(JInt(1))), "c" -> JObject("d" -> JInt(2))))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream trivial object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Stream, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("stream single value object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Stream, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.field("hi"), JInt(3))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream complex object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.streamUntilDepth(1), ValuedJsonToken.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.field("a"), JInt(0)), (Path.root.field("b"), JArray(List(JInt(1)))), (Path.root.field("c"), JObject("d" -> JInt(2)))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream two complex objects") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.streamUntilDepth(1), ValuedJsonToken.stream(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.fill(2)(Chunk((Path.root.field("a"), JInt(0)), (Path.root.field("b"), JArray(List(JInt(1)))), (Path.root.field("c"), JObject("d" -> JInt(2))))).flatten)(js.getOrThrowFiberFailure())
    }
  }

  test("test trivial object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Ignore, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("test single value object") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Ignore, ZStream(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("single value array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Build, ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(3), ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root, JArray(List(JInt(3))))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream empty array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Stream, ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk.empty)(js.getOrThrowFiberFailure())
    }
  }

  test("stream single value array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Stream, ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(3), ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.index(0), JInt(3))))(js.getOrThrowFiberFailure())
    }
  }

  test("stream until inner array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.streamUntilDepth(1), ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(0), ValuedJsonToken.StartArray, ValuedJsonToken.JInt(1), ValuedJsonToken.EndArray, ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.index(0), JInt(0)), (Path.root.index(1), JArray(List(JInt(1))))))(js.getOrThrowFiberFailure())
    }
  }

  test("ignore until inner array") {
    Unsafe.unsafe { implicit unsafe =>
      val js =
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.buildAtDepth(2), ZStream(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(0), ValuedJsonToken.StartArray, ValuedJsonToken.JInt(1), ValuedJsonToken.EndArray, ValuedJsonToken.EndArray)).runCollect
        }
      assert(js.isSuccess)
      assertResult(Chunk((Path.root.index(1).index(0), JInt(1))))(js.getOrThrowFiberFailure())
    }
  }

  test("same as json parser") {
    import org.json4s.jackson.JsonMethods._
    val string = {
      val in = io.Source.fromResource("example.json")
      try {
        in.mkString
      } finally {
        in.close()
      }
    }
    val native: JValue = parse(string)
    val streamed: Chunk[(Path, JValue)] = {
      Unsafe.unsafe { implicit unsafe =>
        Runtime.default.unsafe.run {
          ValuedJsonToken.toJValues(Decider.Build, ValuedJsonToken.stream(jsonFactory.createParser(string))).runCollect
        }.getOrThrow()
      }
    }
    assertResult(Chunk(Path.root -> native))(streamed)
  }
}

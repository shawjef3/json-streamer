package me.jeffshaw.zio

import com.fasterxml.jackson.core.{JsonFactory, JsonFactoryBuilder}
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

class StateIteratorSpec extends AnyFunSuite {

  val jsonFactory: JsonFactory = new JsonFactoryBuilder().build()

  test("Init") {
    assertResult(State.BuildingObject(ObjectDecision.Build, State.Init))(State.Init.nextState(Decider.Build, ValuedJsonToken.StartObject))
  }

  test("build trivial object") {
    val js = IteratorMethods.toJsons(Decider.Build, Iterator(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).toVector
    assertResult(Vector((Path.root, Json.obj())))(js)
  }

  test("build single value object") {
    val js =
      IteratorMethods.toJsons(
        Decider.Build,
        Iterator(
          ValuedJsonToken.StartObject,
          ValuedJsonToken.FieldName("hi"),
          ValuedJsonToken.JInt(3),
          ValuedJsonToken.EndObject
        )
      ).toVector
    assertResult(Vector((Path.root, Json.obj("hi" -> Json.fromInt(3)))))(js)
  }

  test("build complex object") {
    val js =
      IteratorMethods.toJsons(
          Decider.Build,
          IteratorMethods.iterator(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))
      ).toVector
    assertResult(Vector((Path.root, Json.obj("a" -> Json.fromInt(0), "b" -> Json.arr(Json.fromInt(1)), "c" -> Json.obj("d" -> Json.fromInt(2))))))(js)
  }

  test("build two complex objects") {
    val js =
      IteratorMethods.toJsons(
          Decider.Build,
          IteratorMethods.iterator(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))
        ).toVector
    assertResult(Vector.fill(2)((Path.root, Json.obj("a" -> Json.fromInt(0), "b" -> Json.arr(Json.fromInt(1)), "c" -> Json.obj("d" -> Json.fromInt(2))))))(js)
  }

  test("stream trivial object") {
      val js =
        IteratorMethods
          .toJsons(Decider.Stream, Iterator(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject))
          .toVector
      assertResult(Vector.empty)(js)
  }

  test("stream single value object") {
    val js =
      IteratorMethods.toJsons(Decider.Stream, Iterator(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).toVector
    assertResult(Vector((Path.root.field("hi"), Json.fromInt(3))))(js)
  }

  test("stream complex object") {
    val js =
        IteratorMethods.toJsons(Decider.streamUntilDepth(1), IteratorMethods.iterator(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))).toVector
    assertResult(Vector((Path.root.field("a"), Json.fromInt(0)), (Path.root.field("b"), Json.arr(Json.fromInt(1))), (Path.root.field("c"), Json.obj("d" -> Json.fromInt(2)))))(js)
  }

  test("stream two complex objects") {
    val js =
      IteratorMethods.toJsons(Decider.streamUntilDepth(1), IteratorMethods.iterator(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))).toVector
    assertResult(Vector.fill(2)(Vector((Path.root.field("a"), Json.fromInt(0)), (Path.root.field("b"), Json.arr(Json.fromInt(1))), (Path.root.field("c"), Json.obj("d" -> Json.fromInt(2))))).flatten)(js)
  }

  test("test trivial object") {
    val js =
      IteratorMethods.toJsons(Decider.Ignore, Iterator(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject)).toVector
    assertResult(Vector.empty)(js)
  }

  test("test single value object") {
    val js =
      IteratorMethods.toJsons(Decider.Ignore, Iterator(ValuedJsonToken.StartObject, ValuedJsonToken.FieldName("hi"), ValuedJsonToken.JInt(3), ValuedJsonToken.EndObject)).toVector
    assertResult(Vector.empty)(js)
  }

  test("single value array") {
    val js =
      IteratorMethods.toJsons(Decider.Build, Iterator(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(3), ValuedJsonToken.EndArray)).toVector
    assertResult(Vector((Path.root, Json.arr(Json.fromInt(3)))))(js)
  }

  test("stream empty array") {
    val js =
      IteratorMethods.toJsons(Decider.Stream, Iterator(ValuedJsonToken.StartArray, ValuedJsonToken.EndArray)).toVector
    assertResult(Vector.empty)(js)
  }

  test("stream single value array") {
    val js =
      IteratorMethods.toJsons(Decider.Stream, Iterator(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(3), ValuedJsonToken.EndArray)).toVector
    assertResult(Vector((Path.root.index(0), Json.fromInt(3))))(js)
  }

  test("stream until inner array") {
    val js =
      IteratorMethods.toJsons(Decider.streamUntilDepth(1), Iterator(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(0), ValuedJsonToken.StartArray, ValuedJsonToken.JInt(1), ValuedJsonToken.EndArray, ValuedJsonToken.EndArray)).toVector
    assertResult(Vector((Path.root.index(0), Json.fromInt(0)), (Path.root.index(1), Json.arr(Json.fromInt(1)))))(js)
  }

  test("ignore until inner array") {
    val js =
      IteratorMethods.toJsons(Decider.buildAtDepth(2), Iterator(ValuedJsonToken.StartArray, ValuedJsonToken.JInt(0), ValuedJsonToken.StartArray, ValuedJsonToken.JInt(1), ValuedJsonToken.EndArray, ValuedJsonToken.EndArray)).toVector
    assertResult(Vector((Path.root.index(1).index(0), Json.fromInt(1))))(js)
  }

  test("same as json parser") {
    import io.circe.parser._
    val string = {
      val in = scala.io.Source.fromResource("example.json")
      try {
        in.mkString
      } finally {
        in.close()
      }
    }
    val native: Json = parse(string).getOrElse(throw new Exception("invalid json"))
    val iterated: Vector[(Path, Json)] =
      IteratorMethods.toJsons(Decider.Build, IteratorMethods.iterator(jsonFactory.createParser(string))).toVector
    assertResult(Vector(Path.root -> native))(iterated)
  }
}

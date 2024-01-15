package me.jeffshaw.json

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

class StateIteratorSpec extends AnyFunSuite with TestUtils {

  test("Init") {
    assertResult(State.BuildingObject(ObjectDecision.Build, State.Init))(State.Init.nextState(Decider.build, Token.StartObject))
  }

  test("build trivial object") {
    val js = JsonIterator.jsons(Decider.build, Iterator(Token.StartObject, Token.EndObject)).toVector
    assertResult(Vector((Path.root, Json.obj())))(js)
  }

  test("build single value object") {
    val js =
      JsonIterator.jsons(
        Decider.build,
        Iterator(
          Token.StartObject,
          Token.FieldName("hi"),
          Token.JInt(3),
          Token.EndObject
        )
      ).toVector
    assertResult(Vector((Path.root, Json.obj("hi" -> Json.fromInt(3)))))(js)
  }

  test("build complex object") {
    val js =
      JsonIterator.jsons(
          Decider.build,
          JsonIterator.tokens(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))
      ).toVector
    assertResult(Vector((Path.root, Json.obj("a" -> Json.fromInt(0), "b" -> Json.arr(Json.fromInt(1)), "c" -> Json.obj("d" -> Json.fromInt(2))))))(js)
  }

  test("build two complex objects") {
    val js =
      JsonIterator.jsons(
          Decider.build,
          JsonIterator.tokens(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))
        ).toVector
    assertResult(Vector.fill(2)((Path.root, Json.obj("a" -> Json.fromInt(0), "b" -> Json.arr(Json.fromInt(1)), "c" -> Json.obj("d" -> Json.fromInt(2))))))(js)
  }

  test("stream trivial object") {
      val js =
        JsonIterator
          .jsons(Decider.stream, Iterator(Token.StartObject, Token.EndObject))
          .toVector
      assertResult(Vector.empty)(js)
  }

  test("stream single value object") {
    val js =
      JsonIterator.jsons(Decider.stream, Iterator(Token.StartObject, Token.FieldName("hi"), Token.JInt(3), Token.EndObject)).toVector
    assertResult(Vector((Path.root.field("hi"), Json.fromInt(3))))(js)
  }

  test("stream complex object") {
    val js =
        JsonIterator.jsons(Decider.streamUntilDepth(1), JsonIterator.tokens(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}"""))).toVector
    assertResult(Vector((Path.root.field("a"), Json.fromInt(0)), (Path.root.field("b"), Json.arr(Json.fromInt(1))), (Path.root.field("c"), Json.obj("d" -> Json.fromInt(2)))))(js)
  }

  test("stream two complex objects") {
    val js =
      JsonIterator.jsons(Decider.streamUntilDepth(1), JsonIterator.tokens(jsonFactory.createParser("""{"a":0,"b":[1],"c":{"d":2}}{"a":0,"b":[1],"c":{"d":2}}"""))).toVector
    assertResult(Vector.fill(2)(Vector((Path.root.field("a"), Json.fromInt(0)), (Path.root.field("b"), Json.arr(Json.fromInt(1))), (Path.root.field("c"), Json.obj("d" -> Json.fromInt(2))))).flatten)(js)
  }

  test("test trivial object") {
    val js =
      JsonIterator.jsons(Decider.ignore, Iterator(Token.StartObject, Token.EndObject)).toVector
    assertResult(Vector.empty)(js)
  }

  test("test single value object") {
    val js =
      JsonIterator.jsons(Decider.ignore, Iterator(Token.StartObject, Token.FieldName("hi"), Token.JInt(3), Token.EndObject)).toVector
    assertResult(Vector.empty)(js)
  }

  test("single value array") {
    val js =
      JsonIterator.jsons(Decider.build, Iterator(Token.StartArray, Token.JInt(3), Token.EndArray)).toVector
    assertResult(Vector((Path.root, Json.arr(Json.fromInt(3)))))(js)
  }

  test("stream empty array") {
    val js =
      JsonIterator.jsons(Decider.stream, Iterator(Token.StartArray, Token.EndArray)).toVector
    assertResult(Vector.empty)(js)
  }

  test("stream single value array") {
    val js =
      JsonIterator.jsons(Decider.stream, Iterator(Token.StartArray, Token.JInt(3), Token.EndArray)).toVector
    assertResult(Vector((Path.root.index(0), Json.fromInt(3))))(js)
  }

  test("stream until inner array") {
    val js =
      JsonIterator.jsons(Decider.streamUntilDepth(1), Iterator(Token.StartArray, Token.JInt(0), Token.StartArray, Token.JInt(1), Token.EndArray, Token.EndArray)).toVector
    assertResult(Vector((Path.root.index(0), Json.fromInt(0)), (Path.root.index(1), Json.arr(Json.fromInt(1)))))(js)
  }

  test("ignore until inner array") {
    val js =
      JsonIterator.jsons(Decider.ignoreUntilDepth(2), Iterator(Token.StartArray, Token.JInt(0), Token.StartArray, Token.JInt(1), Token.EndArray, Token.EndArray)).toVector
    assertResult(Vector((Path.root.index(1).index(0), Json.fromInt(1))))(js)
  }

  test("same as json parser") {
    import io.circe.parser._
    val native: Json = parse(exampleJson).getOrElse(throw new Exception("invalid json"))
    val iterated: Vector[(Path, Json)] =
      JsonIterator.jsons(Decider.build, JsonIterator.tokens(jsonFactory.createParser(exampleJson))).toVector
    assertResult(Vector(Path.root -> native))(iterated)
  }
}

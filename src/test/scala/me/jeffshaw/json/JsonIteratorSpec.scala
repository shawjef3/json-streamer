package me.jeffshaw.json

import org.scalatest.funsuite.AnyFunSuite

class JsonIteratorSpec extends AnyFunSuite with TestUtils {

  test("hi") {
    val string = """{"a": 0, "b": [0, 1]}"""
    val p = jsonFactory.createParser(string)
    val js = JsonIterator.jsons(Decider.streamUntilDepth(1), p).toVector
    println(js)
  }

  test("empty object") {
    val string = "{}"
    val p = jsonFactory.createParser(string)
    val js = JsonIterator.tokens(p).toVector
    assertResult(Vector(Token.StartObject, Token.EndObject))(js)
  }

  test("lots of serial objects") {
    val count = JsonIterator.tokens(serialObjects).size
    assertResult(objectCount * 2)(count)
  }

  test("lots of embedded objects") {
    val count = JsonIterator.tokens(embeddedObjects).size
    assertResult(3 * objectCount + 1)(count)
  }

  test("lots of serial arrays") {
    val count = JsonIterator.tokens(serialArrays).size
    assertResult(objectCount * 2)(count)
  }

  test("lots of embedded arrays") {
    val count = JsonIterator.tokens(embeddedArrays).size
    assertResult(objectCount * 2)(count)
  }

}

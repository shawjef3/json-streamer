package me.jeffshaw.zio

import org.scalatest.funsuite.AnyFunSuite

class IteratorMethodsSpec extends AnyFunSuite with MethodUtils {

  test("empty object") {
    val string = "{}"
    val p = jsonFactory.createParser(string)
    val js = IteratorMethods.iterator(p).toVector
    assertResult(Vector(ValuedJsonToken.StartObject, ValuedJsonToken.EndObject))(js)
  }

  test("lots of serial objects") {
    val count = IteratorMethods.iterator(serialObjects).size
    assertResult(objectCount * 2)(count)
  }

  test("lots of embedded objects") {
    val count = IteratorMethods.iterator(embeddedObjects).size
    assertResult(3 * objectCount + 1)(count)
  }

  test("lots of serial arrays") {
    val count = IteratorMethods.iterator(serialArrays).size
    assertResult(objectCount * 2)(count)
  }

  test("lots of embedded arrays") {
    val count = IteratorMethods.iterator(embeddedArrays).size
    assertResult(objectCount * 2)(count)
  }

}

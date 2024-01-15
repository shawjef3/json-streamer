package me.jeffshaw.json

import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

class StateSpec extends AnyFunSuite {

  test("Array Build") {
    val state = State.BuildingArray(ObjectDecision.Build, State.Init)

    assertResult(Path.root.index(0))(state.path)

    val afterJInt = state.nextState(Decider.build, Token.JInt(0))

    assertResult(Path.root.index(1))(afterJInt.path)
    assertResult(State.BuildingArray(ObjectDecision.Build, State.Init))(afterJInt)
  }

  test("Array Stream") {
    val state = State.BuildingArray(ObjectDecision.Emit, State.Init)

    assertResult(Path.root.index(0))(state.path)

    val afterJInt = state.nextState(Decider.build, Token.JInt(0))

    assertResult(Path.root.index(0))(afterJInt.path)
    assertResult(Path.root.index(1))(state.path)
    assertResult(State.Emit(Path.root.index(0), Json.fromInt(0), state))(afterJInt)

    val afterEnd = afterJInt.nextState(Decider.build, Token.EndArray)
    assertResult(State.Init)(afterEnd)
  }

  test("Array Stream with inner Build") {
    val state = State.BuildingArray(ObjectDecision.Emit, State.Init)

    assertResult(Path.root.index(0))(state.path)

    val afterArrayStart = state.nextState(Decider.build, Token.StartArray)

    assertResult(Path.root.index(0).index(0))(afterArrayStart.path)
    assertResult(Path.root.index(0))(state.path)
    assertResult(State.BuildingArray(ObjectDecision.Build, state))(afterArrayStart)

    val afterFirstInt = afterArrayStart.nextState(Decider.build, Token.JInt(0))
    assertResult(Path.root.index(0).index(1))(afterFirstInt.path)
    assertResult(afterArrayStart)(afterFirstInt)

    val afterInnerEnd = afterFirstInt.nextState(Decider.build, Token.EndArray)
    assertResult(State.Emit(Path.root.index(0), Json.arr(Json.fromInt(0)), state))(afterInnerEnd)

    val afterSecondElement = afterInnerEnd.nextState(Decider.stream, Token.JInt(1))
    assertResult(State.Emit(Path.root.index(1), Json.fromInt(1), state))(afterSecondElement)

    val afterOuterEnd = afterInnerEnd.nextState(Decider.stream, Token.EndArray)
    assertResult(State.Init)(afterOuterEnd)
  }

  test("Object Build") {
    val state = State.BuildingObject(ObjectDecision.Build, State.Init)

    assertResult(Path.root)(state.path)

    val afterJField = state.nextState(Decider.build, Token.FieldName("a"))
    assertResult(Path.root.field("a"))(afterJField.path)
    assertResult(State.ExpectingObjectValue("a", state))(afterJField)

    val afterJInt = afterJField.nextState(Decider.build, Token.JInt(0))
    assertResult(Path.root)(afterJInt.path)
    assertResult(state)(afterJInt)
    assertResult(List("a" -> Json.fromInt(0)))(state.objectBuilder.result())

    val afterEnd = afterJInt.nextState(Decider.build, Token.EndObject)
    assertResult(State.Emit(Path.root, Json.obj("a" -> Json.fromInt(0)), State.Init))(afterEnd)
  }

  test("Object Stream") {
    val state = State.BuildingObject(ObjectDecision.Emit, State.Init)

    assertResult(Path.root)(state.path)

    val afterJField = state.nextState(Decider.stream, Token.FieldName("a"))
    assertResult(Path.root.field("a"))(afterJField.path)
    assertResult(State.ExpectingObjectValue("a", state))(afterJField)

    val afterJInt = afterJField.nextState(Decider.stream, Token.JInt(0))
    assertResult(Path.root.field("a"))(afterJInt.path)
    assertResult(State.Emit(Path.root.field("a"), Json.fromInt(0), state))(afterJInt)

    val afterEmit = afterJInt.nextState(Decider.stream, Token.EndObject)

    assertResult(State.Init)(afterEmit)
  }

  test("Object Stream with inner Build") {
    val state = State.BuildingObject(ObjectDecision.Emit, State.Init)

    assertResult(Path.root)(state.path)

    val afterJField = state.nextState(Decider.stream, Token.FieldName("a"))
    assertResult(Path.root.field("a"))(afterJField.path)
    assertResult(State.ExpectingObjectValue("a", state))(afterJField)

    val afterInnerObjectStart = afterJField.nextState(Decider.build, Token.StartObject)
    assertResult(Path.root.field("a"))(afterInnerObjectStart.path)
    assertResult(State.BuildingObject(ObjectDecision.Build, afterJField))(afterInnerObjectStart)

    val afterInnerJField = afterInnerObjectStart.nextState(Decider.build, Token.FieldName("b"))
    assertResult(Path.root.field("a").field("b"))(afterInnerJField.path)
    assertResult(State.ExpectingObjectValue("b", afterInnerObjectStart.asInstanceOf[State.BuildingObject]))(afterInnerJField)

    val afterJInt = afterInnerJField.nextState(Decider.build, Token.JInt(0))
    assertResult(Path.root.field("a"))(afterJInt.path)
    assertResult(afterInnerObjectStart)(afterJInt)

    val afterInnerEnd = afterJInt.nextState(Decider.build, Token.EndObject)
    assertResult(Path.root.field("a"))(afterInnerEnd.path)
    assertResult(State.Emit(Path.root.field("a"), Json.obj("b" -> Json.fromInt(0)), state))(afterInnerEnd)

    val afterOuterEnd = afterInnerEnd.nextState(Decider.stream, Token.EndObject)

    assertResult(State.Init)(afterOuterEnd)
  }

}

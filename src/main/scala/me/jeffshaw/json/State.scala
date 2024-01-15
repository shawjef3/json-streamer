package me.jeffshaw.json

import io.circe.Json
import scala.collection.mutable

sealed trait State {
  def nextState(decider: Decider, token: Token): State

  def path: Path
}

object State {
  case object Init extends State {
    override def path: Path = Path.root

    override def nextState(decider: Decider, token: Token): State = {
      token match {
        case Token.StartObject =>
          BuildingObject(decider.`object`(path), Init)
        case Token.StartArray =>
          BuildingArray(decider.`object`(path), Init)
        case _ =>
          throw new IllegalStateException(s"Unexpected token $token")
      }
    }
  }

  sealed trait InnerState extends State {
    def outerState: State

    override def path: Path = outerState.path
  }


  case class Emit(
    override val path: Path,
    value: Json,
    outerState: State
  ) extends InnerState {
    override def nextState(
      decider: Decider,
      token: Token
    ): State = {
      outerState.nextState(decider, token)
    }
  }

  /**
   * States that are recursive.
   */
  sealed trait Builder[AddValue] extends InnerState {
    def withValue(value: AddValue, decision: ValueDecision): State
  }

  case class BuildingObject(
    decision: ObjectDecision,
    override val outerState: State
  ) extends Builder[(String, Json)] {
    // visible for testing
    private[json] val objectBuilder: mutable.Builder[(String, Json), Vector[(String, Json)]] =
      if (decision == ObjectDecision.Build) Vector.newBuilder else null

    override def nextState(decider: Decider, token: Token): State = {
      token match {
        case Token.EndObject =>
          decision match {
            case ObjectDecision.Ignore | ObjectDecision.Emit =>
              outerState match {
                case outerState: ExpectingObjectValue =>
                  outerState.objectBuilder
                case outerState: BuildingArray =>
                  outerState.index += 1
                  outerState
                case Init =>
                  Init
                case state =>
                  throw new IllegalStateException(s"Unexpected state $state")
              }
            case ObjectDecision.Build =>
              val result = Json.fromFields(objectBuilder.result())
              outerState match {
                case outerState: ExpectingObjectValue =>
                  outerState.withValue(result, decider.value(outerState.path))
                case outerState: BuildingArray =>
                  outerState.withValue(result, decider.value(outerState.path))
                case Init =>
                  // top level object
                  Emit(Path.root, result, Init)
                case state =>
                  throw new IllegalStateException(s"Unexpected state $state")
              }
          }
        case Token.FieldName(name) =>
          ExpectingObjectValue(name, this)
        case _ =>
          throw new IllegalArgumentException(s"Unexpected token $token")
      }
    }

    override def withValue(field: (String, Json), decision: ValueDecision): State = {
      decision match {
        case ValueDecision.Keep =>
          this.decision match {
            case ObjectDecision.Emit =>
              Emit(path.field(field._1), field._2, this)
            case ObjectDecision.Build =>
              objectBuilder += field
              this
            case ObjectDecision.Ignore =>
              this
          }
        case ValueDecision.Ignore =>
          this
      }
    }
  }

  case class ExpectingObjectValue(
    fieldName: String,
    objectBuilder: BuildingObject
  ) extends Builder[Json] {
    override def withValue(value: Json, decision: ValueDecision): State = {
      objectBuilder.withValue(fieldName -> value, decision)
    }

    override def nextState(decider: Decider, token: Token): State = {
      token match {
        case Token.StartArray =>
          BuildingArray(decider.`object`(path), this)
        case Token.StartObject =>
          BuildingObject(decider.`object`(path), this)
        case value: Token.TokenValue =>
          withValue(value.asCirce, decider.value(path))
        case _ =>
          throw new IllegalArgumentException(s"Unexpected token $token")
      }
    }

    override val outerState: State = {
      objectBuilder.outerState
    }

    override final val path: Path = {
      super.path.field(fieldName)
    }
  }

  case class BuildingArray(
    decision: ObjectDecision,
    override val outerState: State
  ) extends Builder[Json] {
    var index: Long = 0
    private val arrayBuilder: mutable.Builder[Json, Vector[Json]] =
      if (decision == ObjectDecision.Build) Vector.newBuilder else null

    override def path: Path = {
      super.path.index(index)
    }

    override def nextState(decider: Decider, token: Token): State = {
      token match {
        case Token.EndArray =>
          decision match {
            case ObjectDecision.Ignore | ObjectDecision.Emit =>
              outerState match {
                case outerState: ExpectingObjectValue =>
                  outerState.objectBuilder
                case outerState: BuildingArray =>
                  outerState.index += 1
                  outerState
                case Init =>
                  Init
                case state =>
                  throw new IllegalStateException(s"Unexpected state $state")
              }
            case ObjectDecision.Build =>
              val result = Json.fromValues(arrayBuilder.result())
              outerState match {
                case outerState: ExpectingObjectValue =>
                  outerState.withValue(result, decider.value(path.tail))
                case outerState: BuildingArray =>
                  outerState.withValue(result, decider.value(path.tail))
                case Init =>
                  // top level array
                  Emit(Path.root, result, Init)
                case state =>
                  throw new IllegalStateException(s"Unexpected state $state")
              }
          }
        case Token.StartArray =>
          val innerArrayDecision: ObjectDecision = decider.`object`(path)
          BuildingArray(innerArrayDecision, this)
        case Token.StartObject =>
          BuildingObject(decider.`object`(path), this)
        case value: Token.TokenValue =>
           withValue(value.asCirce, decider.value(path))
        case _ =>
          throw new IllegalArgumentException(s"Unexpected token $token")
      }
    }

    override def withValue(value: Json, decision: ValueDecision): State = {
      decision match {
        case ValueDecision.Keep =>
          this.decision match {
            case ObjectDecision.Emit | ObjectDecision.Ignore =>
              val next = Emit(path, value, this)
              index += 1
              next
            case ObjectDecision.Build =>
              arrayBuilder += value
              index += 1
              this
          }
        case ValueDecision.Ignore =>
          index += 1
          this
      }
    }
  }

}

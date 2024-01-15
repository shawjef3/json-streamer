package me.jeffshaw.zio

import org.json4s._

sealed trait State {
  def nextState(decider: Decider, token: ValuedJsonToken): State

  def path: Path
}

object State {
  case object Init extends State {
    override def path: Path = Path.root

    override def nextState(decider: Decider, token: ValuedJsonToken): State = {
      token match {
        case ValuedJsonToken.StartObject =>
          BuildingObject(decider.`object`(path), Init)
        case ValuedJsonToken.StartArray =>
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
    value: JValue,
    outerState: State
  ) extends InnerState {
    override def nextState(
      decider: Decider,
      token: ValuedJsonToken
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
  ) extends Builder[JField] {
    val objectBuilder = if (decision == ObjectDecision.Build) List.newBuilder[JField] else null

    override def nextState(decider: Decider, token: ValuedJsonToken): State = {
      token match {
        case ValuedJsonToken.EndObject =>
          decision match {
            case ObjectDecision.Ignore | ObjectDecision.Stream =>
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
              val result = JObject(objectBuilder.result())
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
        case ValuedJsonToken.FieldName(name) =>
          ExpectingObjectValue(name, this)
        case _ =>
          throw new IllegalArgumentException(s"Unexpected token $token")
      }
    }

    override def withValue(field: JField, decision: ValueDecision): State = {
      decision match {
        case ValueDecision.Keep =>
          this.decision match {
            case ObjectDecision.Stream =>
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
  ) extends Builder[JValue] {
    override def withValue(value: JValue, decision: ValueDecision): State = {
      objectBuilder.withValue(fieldName -> value, decision)
    }

    override def nextState(decider: Decider, token: ValuedJsonToken): State = {
      token match {
        case ValuedJsonToken.StartArray =>
          BuildingArray(decider.`object`(path), this)
        case ValuedJsonToken.StartObject =>
          BuildingObject(decider.`object`(path), this)
        case value: ValuedJsonToken.ValuedJsonTokenValue =>
          withValue(value.asJValue, decider.value(path))
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
  ) extends Builder[JValue] {
    var index: Long = 0
    val arrayBuilder = if (decision == ObjectDecision.Build) List.newBuilder[JValue] else null

    override def path: Path = {
      super.path.index(index)
    }

    override def nextState(decider: Decider, token: ValuedJsonToken): State = {
      token match {
        case ValuedJsonToken.EndArray =>
          decision match {
            case ObjectDecision.Ignore | ObjectDecision.Stream =>
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
              val result = JArray(arrayBuilder.result())
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
        case ValuedJsonToken.StartArray =>
          val innerArrayDecision: ObjectDecision = decider.`object`(path)
          BuildingArray(innerArrayDecision, this)
        case ValuedJsonToken.StartObject =>
          BuildingObject(decider.`object`(path), this)
        case value: ValuedJsonToken.ValuedJsonTokenValue =>
           withValue(value.asJValue, decider.value(path))
        case _ =>
          throw new IllegalArgumentException(s"Unexpected token $token")
      }
    }

    override def withValue(value: JValue, decision: ValueDecision): State = {
      decision match {
        case ValueDecision.Keep =>
          this.decision match {
            case ObjectDecision.Stream | ObjectDecision.Ignore =>
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

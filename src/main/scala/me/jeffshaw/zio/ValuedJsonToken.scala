package me.jeffshaw.zio

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import org.{json4s => j}

/**
 * `ValuedJsonToken` is an amalgam of [[JsonToken]] and [[j.JValue]] containing the values
 * appropriate for a stateful, streaming parser.
 */
sealed trait ValuedJsonToken

object ValuedJsonToken {
  case object StartObject extends ValuedJsonToken
  case object EndObject extends ValuedJsonToken
  case object StartArray extends ValuedJsonToken
  case object EndArray extends ValuedJsonToken
  case class FieldName(name: String) extends ValuedJsonToken

  sealed trait ValuedJsonTokenValue extends ValuedJsonToken {
    def asJValue: j.JValue
  }

  case class JString(value: String) extends ValuedJsonTokenValue {
    override val asJValue: j.JValue = {
      j.JString(value)
    }
  }
  case class JInt(value: Int) extends ValuedJsonTokenValue {
    override val asJValue: j.JValue = {
      j.JInt(value)
    }
  }
  case class JLong(value: Long) extends ValuedJsonTokenValue {
    override val asJValue: j.JValue = {
      j.JLong(value)
    }
  }
  case class JBigInt(value: BigInt) extends ValuedJsonTokenValue {
    override val asJValue: j.JValue = {
      j.JInt(value)
    }
  }
  case class JFloat(value: Float) extends ValuedJsonTokenValue {
    override val asJValue: j.JValue = {
      j.JDouble(value.toDouble)
    }
  }
  case class JDouble(value: Double) extends ValuedJsonTokenValue {
    override val asJValue: j.JValue = {
      j.JDouble(value)
    }
  }
  case class JBigDecimal(value: BigDecimal) extends ValuedJsonTokenValue {
    override val asJValue: j.JValue = {
      j.JDecimal(value)
    }
  }
  case object JNull extends ValuedJsonTokenValue {
    override def asJValue: j.JValue = {
      j.JNull
    }
  }
  case object JFalse extends ValuedJsonTokenValue {
    override def asJValue: j.JValue = {
      j.JBool.False
    }
  }
  case object JTrue extends ValuedJsonTokenValue {
    override def asJValue: j.JValue = {
      j.JBool.True \ "true"
    }
  }

  def apply(p: JsonParser, token: JsonToken): ValuedJsonToken = {
    token match {
      case JsonToken.START_OBJECT =>
        StartObject
      case JsonToken.END_OBJECT =>
        EndObject
      case JsonToken.START_ARRAY =>
        StartArray
      case JsonToken.END_ARRAY =>
        EndArray
      case JsonToken.FIELD_NAME =>
        FieldName(p.getCurrentName)
      case JsonToken.VALUE_STRING =>
        JString(p.getValueAsString)
      case JsonToken.VALUE_NUMBER_INT | JsonToken.VALUE_NUMBER_FLOAT =>
        // This mirrors com.fasterxml.jackson.databind.util.TokenBuffer.Parser.getNumberType().
        p.getNumberValue match {
          case i: java.lang.Integer =>
            JInt(i)
          case l: java.lang.Long =>
            JLong(l)
          case d: java.lang.Double =>
            JDouble(d)
          case b: java.math.BigDecimal =>
            JBigDecimal(b)
          case b: java.math.BigInteger =>
            JBigInt(b)
          case f: java.lang.Float =>
            JFloat(f)
          case s: java.lang.Short =>
            JInt(s.toInt)
        }
      case JsonToken.VALUE_TRUE =>
        JTrue
      case JsonToken.VALUE_FALSE =>
        JFalse
      case JsonToken.VALUE_NULL =>
        JNull
    }
  }

}

package me.jeffshaw.zio

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import io.circe._

/**
 * `ValuedJsonToken` is an amalgam of [[JsonToken]] and [[Json]] containing the values
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
    def asCirce: Json
  }

  case class JString(value: String) extends ValuedJsonTokenValue {
    override val asCirce: Json = {
      Json.fromString(value)
    }
  }
  case class JInt(value: Int) extends ValuedJsonTokenValue {
    override val asCirce: Json = {
      Json.fromInt(value)
    }
  }
  case class JLong(value: Long) extends ValuedJsonTokenValue {
    override val asCirce: Json = {
      Json.fromLong(value)
    }
  }
  case class JBigInt(value: BigInt) extends ValuedJsonTokenValue {
    override val asCirce: Json = {
      Json.fromBigInt(value)
    }
  }
  case class JFloat(value: Float) extends ValuedJsonTokenValue {
    override val asCirce: Json = {
      Json.fromFloat(value).get
    }
  }
  case class JDouble(value: Double) extends ValuedJsonTokenValue {
    override val asCirce: Json = {
      Json.fromDouble(value).get
    }
  }
  case class JBigDecimal(value: BigDecimal) extends ValuedJsonTokenValue {
    override val asCirce: Json = {
      Json.fromBigDecimal(value)
    }
  }
  case object JNull extends ValuedJsonTokenValue {
    override def asCirce: Json = {
      Json.Null
    }
  }
  case object JFalse extends ValuedJsonTokenValue {
    override def asCirce: Json = {
      Json.False
    }
  }
  case object JTrue extends ValuedJsonTokenValue {
    override def asCirce: Json = {
      Json.True
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

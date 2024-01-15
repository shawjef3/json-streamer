package me.jeffshaw.json

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import io.circe._

/**
 * `ValuedJsonToken` is an amalgam of [[JsonToken]] and [[Json]] containing the values
 * appropriate for a stateful, streaming parser.
 */
sealed trait Token

object Token {
  case object StartObject extends Token
  case object EndObject extends Token
  case object StartArray extends Token
  case object EndArray extends Token
  case class FieldName(name: String) extends Token

  sealed trait TokenValue extends Token {
    def asCirce: Json
  }

  case class JString(value: String) extends TokenValue {
    override val asCirce: Json = {
      Json.fromString(value)
    }
  }
  case class JInt(value: Int) extends TokenValue {
    override val asCirce: Json = {
      Json.fromInt(value)
    }
  }
  case class JLong(value: Long) extends TokenValue {
    override val asCirce: Json = {
      Json.fromLong(value)
    }
  }
  case class JBigInt(value: BigInt) extends TokenValue {
    override val asCirce: Json = {
      Json.fromBigInt(value)
    }
  }
  case class JFloat(value: Float) extends TokenValue {
    override val asCirce: Json = {
      Json.fromFloat(value).get
    }
  }
  case class JDouble(value: Double) extends TokenValue {
    override val asCirce: Json = {
      Json.fromDouble(value).get
    }
  }
  case class JBigDecimal(value: BigDecimal) extends TokenValue {
    override val asCirce: Json = {
      Json.fromBigDecimal(value)
    }
  }
  case object JNull extends TokenValue {
    override def asCirce: Json = {
      Json.Null
    }
  }
  case object JFalse extends TokenValue {
    override def asCirce: Json = {
      Json.False
    }
  }
  case object JTrue extends TokenValue {
    override def asCirce: Json = {
      Json.True
    }
  }

  def apply(p: JsonParser, token: JsonToken): Token = {
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

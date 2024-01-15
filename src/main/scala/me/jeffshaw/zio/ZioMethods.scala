package me.jeffshaw.zio

import com.fasterxml.jackson.core.JsonParser
import io.circe.Json
import java.io.IOException
import zio.ZIO
import zio.stream.ZStream

object ZioMethods {

  def next(p: JsonParser): Option[ValuedJsonToken] = {
    val nextToken = p.nextToken()
    if (nextToken == null) {
      None
    } else {
      Some(ValuedJsonToken(p, nextToken))
    }
  }

  def nextZio(p: JsonParser): ZIO[Any, Option[IOException], ValuedJsonToken] = {
    for {
      maybeNext <- ZIO.attempt(next(p)).refineOrDie { case io: IOException => Some(io) }
      result <-
        maybeNext match {
          case Some(result) =>
            ZIO.succeed(result)
          case None =>
            ZIO.fail(None)
        }
    } yield result
  }

  def stream: ZStream[JsonParser, IOException, ValuedJsonToken] =
    for {
      p <- ZStream.service[JsonParser]
      token <- stream(p)
    } yield token

  def stream(p: JsonParser): ZStream[Any, IOException, ValuedJsonToken] = {
    ZStream.repeatZIOOption(nextZio(p))
  }

  def toJValues[R, E](decider: Decider, tokens: ZStream[R, E, ValuedJsonToken]): ZStream[R, E, (Path, Json)] = {
    tokens.mapAccum[State, Option[(Path, Json)]](State.Init) {
      case (state, token) =>
        val nextState = state.nextState(decider, token)
        nextState match {
          case e: State.Emit =>
            (nextState, Some((e.path, e.value)))
          case _ =>
            (nextState, None)
        }
    }.collect { case Some(result) => result }
  }

}

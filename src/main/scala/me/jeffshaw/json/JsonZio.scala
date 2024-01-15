package me.jeffshaw.json

import com.fasterxml.jackson.core.JsonParser
import io.circe.Json
import java.io.IOException
import zio.ZIO
import zio.stream.ZStream

object JsonZio {

  def next(p: JsonParser): Option[Token] = {
    val nextToken = p.nextToken()
    if (nextToken == null) {
      None
    } else {
      Some(Token(p, nextToken))
    }
  }

  def nextZio(p: JsonParser): ZIO[Any, Option[IOException], Token] = {
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

  def tokens: ZStream[JsonParser, IOException, Token] =
    for {
      p <- ZStream.service[JsonParser]
      token <- tokens(p)
    } yield token

  def tokens(p: JsonParser): ZStream[Any, IOException, Token] = {
    ZStream.repeatZIOOption(nextZio(p))
  }

  def jsons[R](decider: Decider, p: JsonParser): ZStream[R, IOException, (Path, Json)] = {
    jsons(decider, tokens(p))
  }

  def jsons[R, E](decider: Decider, tokens: ZStream[R, E, Token]): ZStream[R, E, (Path, Json)] = {
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

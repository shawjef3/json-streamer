package me.jeffshaw.json

import com.fasterxml.jackson.core.JsonParser
import io.circe.Json

object JsonIterator {

  def tokens(p: JsonParser): Iterator[Token] =
    for (token <- Iterator.continually(p.nextToken()).takeWhile(_ != null)) yield {
      Token(p, token)
    }

  def jsons(decider: Decider, p: JsonParser): Iterator[(Path, Json)] = {
    jsons(decider, tokens(p))
  }

  def jsons(decider: Decider, tokens: Iterator[Token]): Iterator[(Path, Json)] = {
    tokens.scanLeft[State](State.Init) {
      case (state, token) =>
        state.nextState(decider, token)
    }.collect { case e: State.Emit => (e.path, e.value) }
  }

}

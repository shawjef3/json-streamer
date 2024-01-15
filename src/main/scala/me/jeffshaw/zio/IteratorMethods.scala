package me.jeffshaw.zio

import com.fasterxml.jackson.core.JsonParser

object IteratorMethods {

  def iterator(p: JsonParser): Iterator[ValuedJsonToken] =
    for (token <- Iterator.continually(p.nextToken()).takeWhile(_ != null)) yield {
      ValuedJsonToken(p, token)
    }

  def toJValues(decider: Decider, tokens: Iterator[ValuedJsonToken]): Iterator[(Path, org.json4s.JValue)] = {
    tokens.scanLeft[State](State.Init) {
      case (state, token) =>
        state.nextState(decider, token)
    }.collect { case e: State.Emit => (e.path, e.value) }
  }

}
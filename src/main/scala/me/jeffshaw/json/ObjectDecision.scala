package me.jeffshaw.json

sealed trait ObjectDecision

object ObjectDecision {
  /**
   * Emit each element in the array or object.
   */
  case object Emit extends ObjectDecision

  /**
   * Build the array or object into a [[io.circe.Json]].
   */
  case object Build extends ObjectDecision

  /**
   * Ignore the array or object.
   */
  case object Ignore extends ObjectDecision
}

package me.jeffshaw.zio

sealed trait ValueDecision

object ValueDecision {
  /**
   * Include the value in the output or its parent object or array.
   */
  case object Keep extends ValueDecision

  /**
   * Ignore the value.
   */
  case object Ignore extends ValueDecision
}

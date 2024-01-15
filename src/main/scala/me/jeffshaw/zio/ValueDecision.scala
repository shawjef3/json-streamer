package me.jeffshaw.zio

sealed trait ValueDecision

object ValueDecision {
  case object Keep extends ValueDecision
  case object Ignore extends ValueDecision
}

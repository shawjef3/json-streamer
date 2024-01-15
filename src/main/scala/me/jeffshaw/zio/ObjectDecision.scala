package me.jeffshaw.zio

sealed trait ObjectDecision

object ObjectDecision {
  case object Stream extends ObjectDecision
  case object Build extends ObjectDecision
  case object Ignore extends ObjectDecision
}

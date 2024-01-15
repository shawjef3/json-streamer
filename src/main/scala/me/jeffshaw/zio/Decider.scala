package me.jeffshaw.zio

/**
 * Control how the token streamer handles each value.
 */
trait Decider {
  def value(path: Path): ValueDecision
  def `object`(path: Path): ObjectDecision
}

object Decider {
  def apply(
    v: Path => ValueDecision,
    o: Path => ObjectDecision
  ): Decider = new Decider {
    override def value(path: Path): ValueDecision = v(path)
    override def `object`(path: Path): ObjectDecision = o(path)
  }

  val build: Decider =
    Decider(Function.const(ValueDecision.Keep), Function.const(ObjectDecision.Build))

  val ignore: Decider =
    Decider(Function.const(ValueDecision.Ignore), Function.const(ObjectDecision.Ignore))

  val stream: Decider =
    Decider(Function.const(ValueDecision.Keep), Function.const(ObjectDecision.Emit))

  def streamUntilDepth(depth: Int): Decider = {
    Decider(
      Function.const(ValueDecision.Keep),
      path => {
        if (path.depth >= depth) {
          ObjectDecision.Build
        } else {
          ObjectDecision.Emit
        }
      }
    )
  }

  def ignoreUntilDepth(depth: Int): Decider = {
    Decider(
      path => {
        if (path.depth >= depth) {
          ValueDecision.Keep
        } else {
          ValueDecision.Ignore
        }
      },
      path => {
        if (path.depth >= depth) {
          ObjectDecision.Build
        } else {
          ObjectDecision.Ignore
        }
      }
    )
  }
}

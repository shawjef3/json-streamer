package me.jeffshaw.zio

trait Decider {
  def value(path: Path): ValueDecision
  def `object`(path: Path): ObjectDecision
}

object Decider {
  object Build extends Decider {
    override def value(path: Path): ValueDecision = ValueDecision.Keep
    override def `object`(path: Path): ObjectDecision = ObjectDecision.Build
  }

  object Ignore extends Decider {
    override def value(path: Path): ValueDecision = ValueDecision.Ignore

    override def `object`(path: Path): ObjectDecision = ObjectDecision.Ignore
  }

  object Stream extends Decider {
    override def value(path: Path): ValueDecision = ValueDecision.Keep

    override def `object`(path: Path): ObjectDecision = ObjectDecision.Stream
  }

  def streamUntilDepth(depth: Int): Decider = new Decider {
    override def value(path: Path): ValueDecision = ValueDecision.Keep

    override def `object`(path: Path): ObjectDecision = {
      if (path.depth >= depth) {
        ObjectDecision.Build
      } else {
        ObjectDecision.Stream
      }
    }
  }

  def buildAtDepth(depth: Int): Decider = new Decider {
    override def value(path: Path): ValueDecision = {
      if (path.depth >= depth) {
        ValueDecision.Keep
      } else {
        ValueDecision.Ignore
      }
    }

    override def `object`(path: Path): ObjectDecision = {
      if (path.depth >= depth) {
        ObjectDecision.Build
      } else {
        ObjectDecision.Ignore
      }
    }
  }
}

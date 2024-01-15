package me.jeffshaw.zio

case class Path(path: List[Path.Elem]) {
  lazy val depth: Int = path.size

  def tail: Path = Path(path.tail)

  def +: (elem: Path.Elem): Path = {
    Path(elem :: path)
  }

  def index(i: Long): Path = {
    Path.Elem.Index(i) +: this
  }

  def field(name: String): Path = {
    Path.Elem.Field(name) +: this
  }

  override def toString: String = {
    val sb = new StringBuilder()
    sb += '$'
    for {
      elem <- path.reverse
    } {
      elem match {
        case Path.Elem.Field(name) =>
          sb += '.'
          sb ++= name
        case Path.Elem.Index(index) =>
          sb += '['
          sb ++= index.toString
          sb += ']'
      }
    }
    sb.toString
  }
}

object Path {
  val root: Path = Path(Nil)

  sealed trait Elem

  object Elem {
    case class Field(name: String) extends Elem
    case class Index(index: Long) extends Elem
  }
}

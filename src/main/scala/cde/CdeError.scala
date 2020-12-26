package cde

sealed trait CdeError:
  def source: CdeSource
  def message: String
  override def toString: String =
    source match
      case CdeSource.Empty => message
      case CdeSource.File(f, l, c) => s"$f:$l:$c\n  $message"

object CdeError:
  def apply(src: CdeSource, msg: String): CdeError =
    new CdeError:
      val source = src
      val message = msg

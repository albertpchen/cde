package cde

sealed trait CdeError:
  def source: CdeSource.File
  def message: String
  override def toString: String =
    s"${source.prettyPrint()}\n${CdeError.tab}${message.replace("\n", s"\n${CdeError.tab}")}"

object CdeError:
  val tab = "  "

  case class CdeErrorImp(source: CdeSource.File, message: String) extends CdeError

  def apply(src: CdeSource.File, msg: String): CdeError = CdeErrorImp(src, msg)

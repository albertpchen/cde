package cde

sealed trait CdeError:
  def source: CdeSource

  def message: String

  override def toString: String =
    s"${source.prettyPrint()}\n${CdeError.tab}${message.replace("\n", s"\n${CdeError.tab}")}"


object CdeError:
  val tab = "  "

  private case class CdeErrorImp(source: CdeSource, message: String) extends CdeError

  def apply(src: CdeSource, msg: String): CdeError = CdeErrorImp(src, msg)

package cde

/** Interface for errors encountered during [[Cde]] elaboration
  */
sealed trait CdeError derives CanEqual:
  /** Source locator for this error
    */
  def source: CdeSource

  /** Message explaining the cause of this error, may be multi-line
    */
  def message: String

  /** Pretty prints this error. The first line is the source locator, and
    * subsequent lines are the message prefixed by [[CdeError.tab]]
    */
  override def toString: String =
    s"${source.prettyPrint()}\n${CdeError.tab}${message.replace("\n", s"\n${CdeError.tab}")}"


object CdeError:
  /** The string used to indent error messages
    */
  val tab = "  "

  /** Private case class implementation of [[CdeError]].
    *
    * This is private so that we do not expose public copy, unapply, etc.
    * methods.
    */
  private case class CdeErrorImp(source: CdeSource, message: String) extends CdeError

  /** Constructs a new [[CdeError]]
    */
  def apply(src: CdeSource, msg: String): CdeError = CdeErrorImp(src, msg)

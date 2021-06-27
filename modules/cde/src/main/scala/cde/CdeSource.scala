package cde

import scala.quoted._

/** Source locators used by Cde for error messages
  */
case class CdeSource(file: String, line: Int, col: Int) derives CanEqual:
  def prettyPrint(tab: String = ""): String =
    s"$tab${file}:${line}:${col}"

object CdeSource:
  inline given source: CdeSource = ${cdeSourceImpl}

  /** Macro for synthesizing [[CdeSource]] instances
    */
  def cdeSourceImpl(using Quotes): Expr[CdeSource] =
    import quotes.reflect._
    val pos = quotes.reflect.Position.ofMacroExpansion
    val jpath = pos.sourceFile.jpath
    val file =
      if jpath == null then
        "REPL"
      else
        jpath.toAbsolutePath.toString
    val line = pos.startLine + 1
    val col = pos.startColumn + 1
    '{CdeSource(${Expr(file)}, ${Expr(line)}, ${Expr(col)})}

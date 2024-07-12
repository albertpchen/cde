package cde

import scala.quoted._

/** Source locators used by Cde for error messages
  */
case class Source(file: String, line: Int, col: Int) derives CanEqual:
  def prettyPrint(tab: String = ""): String =
    s"$tab${file}:${line}:${col}"

object Source:
  inline given source: Source = ${cdeSourceImpl}

  /** Macro for synthesizing [[Source]] instances
    */
  def cdeSourceImpl(using Quotes): Expr[Source] =
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
    '{Source(${Expr(file)}, ${Expr(line)}, ${Expr(col)})}

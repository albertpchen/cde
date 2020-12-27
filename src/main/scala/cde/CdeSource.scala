package cde

import scala.quoted._

case class CdeSource(file: String, line: Int, col: Int)

object CdeSource:
  inline given CdeSource = ${cdeSourceImpl}

  extension (src: CdeSource)
    def prettyPrint(tab: String = ""): String =
      s"$tab${src.file}:${src.line}:${src.col}"

  def cdeSourceImpl(using Quotes): Expr[CdeSource] =
    import quotes.reflect._
    val pos = quotes.reflect.Position.ofMacroExpansion
    val file = pos.sourceFile.jpath.toAbsolutePath.toString
    val line = pos.startLine + 1
    val col = pos.startColumn + 1
    '{CdeSource(${Expr(file)}, ${Expr(line)}, ${Expr(col)})}

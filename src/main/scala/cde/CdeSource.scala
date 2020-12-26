package cde

import scala.quoted._

enum CdeSource:
  case Empty
  case File(file: String, line: Int, col: Int)

object CdeSource:

  inline given CdeSource = ${cdeSourceImpl}
  def cdeSourceImpl(using Quotes): Expr[CdeSource] =
    import quotes.reflect._
    val pos = quotes.reflect.Position.ofMacroExpansion
    val file = pos.sourceFile.jpath.toAbsolutePath.toString
    val line = pos.startLine + 1
    val col = pos.startColumn + 1
    '{CdeSource.File(${Expr(file)}, ${Expr(line)}, ${Expr(col)})}

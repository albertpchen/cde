package cde

import scala.quoted._

enum CdeSource:
  case Empty
  case File(file: String, line: Int, col: Int)
  case Multiple(sources: Seq[File])

  extension (src: File | Multiple)
    def prettyPrint(tab: String = ""): String =
      src match
        case File(file, line, col) => s"$tab$file:$line:$col"
        case Multiple(sources) if sources.size == 1 => sources.head.toString
        case Multiple(sources) =>
          val map = sources.groupBy(_.file)
          sources.map(_.file).distinct.map { file =>
            map(file).map(src => s"${src.line}:${src.col}").mkString(start="[", sep=", ", end="]")
          }.mkString(start=tab, sep=s"\n$tab", end="")

object CdeSource:

  inline given CdeSource.File = ${cdeSourceImpl}
  def cdeSourceImpl(using Quotes): Expr[CdeSource.File] =
    import quotes.reflect._
    val pos = quotes.reflect.Position.ofMacroExpansion
    val file = pos.sourceFile.jpath.toAbsolutePath.toString
    val line = pos.startLine + 1
    val col = pos.startColumn + 1
    '{CdeSource.File(${Expr(file)}, ${Expr(line)}, ${Expr(col)})}

package cde

import scala.quoted._

sealed trait Tag[T]:
  protected def tag: String
  def isSameType(t: Tag[?]): Boolean = tag == t.tag
  override def toString: String = tag

object Tag {
  inline given [T]: Tag[T] = ${ makeTag[T] }

  def apply[T](t: String): Tag[T] =
    new Tag[T]:
      val tag = t.toString

  def makeTag[T: Type](using Quotes): Expr[Tag[T]] =
    import quotes.reflect._
    '{ Tag[T](${Expr(TypeTree.of[T].show)}) }
}

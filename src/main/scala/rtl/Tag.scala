package cde

import scala.quoted._

sealed trait Tag[T]:
  protected def tag: String
  def isSameType(t: Tag[?]): Boolean = tag == t.tag
  override def toString: String = tag
  def castIfEquals(value: Any, valueTag: Tag[_]): Option[T] =
    if isSameType(valueTag) then
      Some(value.asInstanceOf[T])
    else
      None

object Tag:
  inline given [T]: Tag[T] = ${ makeTag[T] }

  def apply[T](t: String): Tag[T] =
    new Tag[T]:
      val tag = t.toString

  def makeTag[T: Type](using Quotes): Expr[Tag[T]] =
    import quotes.reflect._
    '{ Tag[T](${Expr(TypeTree.of[T].show)}) }

  def castIfTagEquals[T](a: Any, tag1: Tag[T], tag2: Tag[_]): Option[T] =
    if tag1.isSameType(tag2) then
      Some(a.asInstanceOf[T])
    else
      None

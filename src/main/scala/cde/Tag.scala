package cde

import scala.annotation.implicitNotFound
import scala.quoted._

/** Replacement for scala 2 TypeTag that only supports checking type equality
  */
@implicitNotFound("Cannot synthesize type Tag for type ${T}")
sealed trait Tag[T]:
  /** The string value of the type [[T]] that uniquely identifies this type
    */
  protected def tag: String

  /** Returns true if this tag represents the same type as the given tag
    */
  def isSameType(t: Tag[?]): Boolean = tag == t.tag

  override def toString: String = tag

  /** Helper method to cast a value to this type if its associated tag is equal
    * to this tag
    */
  def castIfEquals(value: Any, valueTag: Tag[_]): Option[T] =
    if isSameType(valueTag) then
      Some(value.asInstanceOf[T])
    else
      None

object Tag:
  inline given [T]: Tag[T] = ${ makeTag[T] }

  /** Construct a [[Tag]] with the given string representation
    *
    * Should only be used by [[makeTag]].
    */
  def apply[T](t: String): Tag[T] =
    new Tag[T]:
      val tag = t.toString

  /** Macro for syntesizing [[Tag]]s
    */
  def makeTag[T: Type](using Quotes): Expr[Tag[T]] =
    import quotes.reflect._
    '{ Tag[T](${Expr(TypeTree.of[T].show)}) }

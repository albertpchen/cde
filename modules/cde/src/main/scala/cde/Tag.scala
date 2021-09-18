package cde

import scala.annotation.implicitNotFound
import scala.quoted._

/** Replacement for scala 2 TypeTag that only supports checking type equality
  */
@implicitNotFound("Cannot synthesize type Tag for type ${T}")
case class Tag[T]private (protected val tag: String) derives CanEqual:
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

  def apply[T: Tag]: Tag[T] = summon[Tag[T]]

  /** Construct a [[Tag]] with the given string representation
    *
    * Should only be used by [[makeTag]].
    */
  def create[T](t: String): Tag[T] = Tag[T](t.toString)

  /** Macro for syntesizing [[Tag]]s
    */
  private def makeTag[T: Type](using Quotes): Expr[Tag[T]] =
    import quotes.reflect._
    def isClass(tpe: TypeRepr): Boolean = tpe.typeSymbol.isClassDef

    def isGround(t: Boolean, tpe: TypeRepr): Boolean =
      tpe match
      case _ if !t => t
      case tp: TypeRef => isClass(tp.dealias)
      case tp: ConstantType => true
      case tp: AppliedType =>
        @annotation.tailrec def foldArgs(x: Boolean, args: List[TypeRepr]): Boolean =
          if args.isEmpty || !x then x else foldArgs(isGround(x, args.head), args.tail)
        foldArgs(isClass(tp.tycon.dealias), tp.args)
      case tp: AndOrType => isGround(isGround(t, tp.left), tp.right)
      case _ => false

    val tpe = TypeTree.of[T].tpe.dealias.simplified

    if isGround(true, tpe) then
      '{ Tag.create[T](${Expr(tpe.show)}) }
    else
      report.throwError(s"cannot create type tag for non-ground type: ${tpe.show}")

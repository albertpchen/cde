package cde

import scala.annotation.implicitNotFound
import scala.quoted._

/** Replacement for scala 2 TypeTag that only supports checking type equality
  */
@implicitNotFound("Cannot synthesize type Tag for type ${T}")
sealed trait Tag[T] derives CanEqual:
  /** The string value of the type [[T]] that uniquely identifies this type
    */
  protected def tag: String
  val isIO: Boolean

  /** Returns true if this tag represents the same type as the given tag
    */
  def isSameType(t: Tag[?]): Boolean = tag == t.tag
  override final def equals(other: Any): Boolean =
    other match
    case t: Tag[?] => t.isSameType(this)
    case _ => false

  override def toString: String = tag

  /** Helper method to cast a value to this type if its associated tag is equal
    * to this tag
    */
  def castIfEquals(value: Any, valueTag: Tag[?]): Option[T] =
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
  def apply[T: IsIOTest](t: String): Tag[T] =
    new Tag[T]:
      val tag = t.toString
      val isIO = summon[IsIOTest[T]].isIO

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
      '{ Tag[T](${Expr(tpe.show)})(using scala.compiletime.summonInline) }
    else
      report.throwError(s"cannot create type tag for non-ground type: ${tpe.show}")

final class IsIOTest[T] private (val isIO: Boolean)

object IsIOTest:
  given [T]: IsIOTest[T] = new IsIOTest(false)
  given [T]: IsIOTest[cats.effect.IO[T]] = new IsIOTest(true)

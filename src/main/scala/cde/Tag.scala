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
    def isClass(tpe: TypeRepr): Boolean = tpe.typeSymbol.isClassDef

    def isGround(t: Boolean, tpe: TypeRepr): Boolean =
      tpe match
      case _ if !t => t
      case tp: TermRef => false
      case tp: TypeRef => isClass(tp.dealias)
      case tp: ConstantType => true
      case tp: SuperType => false
      case tp: AppliedType =>
        @annotation.tailrec def foldArgs(x: Boolean, args: List[TypeRepr]): Boolean =
          if args.isEmpty || !x then x else foldArgs(isGround(x, args.head), args.tail)
        val res = isClass(tp.tycon.dealias)
        if res then
          foldArgs(res, tp.args)
        else
          false
      case tp: AnnotatedType => false
      case tp: AndOrType => isGround(isGround(t, tp.left), tp.right)
      case tp: MatchType => false
      case tp: ByNameType => false
      case tp: ParamRef => false
      case tp: ThisType => false
      case tp: RecursiveThis => false
      case tp: RecursiveType => false
      case tp: LambdaType => false
      case tp: MatchCase => false
      case tp: TypeBounds => false
      case tp: NoPrefix => false

    val tpe = TypeTree.of[T].tpe.dealias.simplified

    if isGround(true, tpe) then
      '{ Tag[T](${Expr(TypeTree.of[T].show)}) }
    else
      report.throwError(s"cannot create type tag for non-ground type: ${tpe.show}")

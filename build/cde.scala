package cde

import cats.data.{Chain, NonEmptyChain}
import scala.collection.immutable.VectorMap
import scala.language.dynamics
import scala.util.Try
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, JsonReader, JsonWriter}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

sealed trait Cde extends Dynamic derives CanEqual:
  def getSource(): Source

  final def +(overrides: Cde)(using Source): Cde =
    Cde.Mixin(summon, this, overrides)

  def selectDynamic(name: String): Cde.Select =
    Cde.Select(this, NonEmptyChain(name))

  private[cde] def get(field: String): Option[Cde.Value]

  private val cache = scala.collection.concurrent.TrieMap[String, Try[Any]]()
  private def force[T: Tag](field: String)(using Source) =
    cache
      .getOrElseUpdate(field, Try {
        val value = get(field).getOrElse(throw new Cde.LookupError(this, field, summon))
        if summon[Tag[T]].isSameType(value.tag) then
          value
            .thunk(using Cde.Ctx(self = this, fieldName = field, summon))
            .asInstanceOf[T]
            .match {
              case io: cats.effect.IO[?] => io.memoize.flatten
              case a: T => a
            }
        else
          throw new Cde.LookupTypeError(this, field, value.tag, summon)
    })
    .get
    .asInstanceOf[T]

  private def get[T: Tag](path: NonEmptyChain[String])(using Source): T =
    path
      .tail
      .uncons
      .fold {
        force[T](path.head)
      } { (head, tail) =>
        val child = force[Cde](path.head)
        child.get[T](NonEmptyChain(head) :++ tail)
      }

  def apply(build: Cde.Builder ?=> Unit)(using Source): Cde =
    this + Cde.apply(build)



object Cde:
  opaque type Codec[T] = JsonValueCodec[T]
  sealed trait HasJsonCodecMaker:
    inline given [T]: Codec[T] = JsonCodecMaker.make

  object Codec extends HasJsonCodecMaker:
    given Codec[Cde] = Cde.codec

  class CodecValue[T](t: T, codec: JsonValueCodec[T], val isForcedVisible: Boolean):
    def notForcedVisibe = !isForcedVisible
    def writeValue(writer: JsonWriter): Unit = codec.encodeValue(t, writer)

  given codec: JsonValueCodec[Cde] = new JsonValueCodec[Cde]:
    def nullValue: Cde = null
    def encodeValue(root: Cde, writer: JsonWriter): Unit =
      val isHidden = scala.collection.mutable.Set[String]()
      val members = scala.collection.mutable.ListMap[String, CodecValue[?]]()
      def findHidden(cde: Cde): Unit =
        cde match
          case mixin: Mixin =>
            findHidden(mixin.right)
            findHidden(mixin.left)
          case leaf: Leaf =>
            leaf.members.foreach { (name, member) =>
              if member.visibility.isHidden then
                isHidden.add(name)
            }
      def addMembers(cde: Cde): Unit =
        cde match
          case mixin: Mixin =>
            addMembers(mixin.right)
            addMembers(mixin.left)
          case leaf: Leaf =>
            leaf.members.foreach { (name, member) =>
                if !members.contains(name) && !(isHidden(name) && member.visibility.isDefault) then
                  member
                    .getCodecValue(Ctx(root, name, member.source))
                    .foreach(members(name) = _)
            }
      findHidden(root)
      addMembers(root)
      writer.writeObjectStart()
      members.foreach { (key, value) =>
        writer.writeKey(key)
        value.writeValue(writer)
      }
      writer.writeObjectEnd()
    def decodeValue(reader: JsonReader, default: Cde): Cde =
      reader.decodeError("cannot decode Cde")
  
  enum Visibility[+T]:
    case Hidden extends Visibility[Nothing]
    case Default[T](codec: JsonValueCodec[T]) extends Visibility[T]
    case ForcedVisible[T](codec: JsonValueCodec[T]) extends Visibility[T]
    def isForcedVisible: Boolean = this match
      case _: ForcedVisible[?] => true
      case _ => false
    def isHidden: Boolean = this match
      case Hidden => true
      case _ => false
    def isDefault: Boolean = this match
      case _: Default[?] => true
      case _ => false

  case class LookupError(
    cde: Cde,
    field: String,
    tag: Tag[?],
  ) extends Exception(s"$cde is missing field $field")

  case class LookupTypeError(
    cde: Cde,
    field: String,
    got: Tag[?],
    expected: Tag[?]
  ) extends Exception(s"$field: got type $got, expected $expected")

  case class SuperLookupError(ctx: Ctx)
    extends Exception(s"no super while evaluating ${ctx.fieldName}: ${ctx.source.prettyPrint()}"):
    override def toString: String = s"SuperLookupError(${ctx.self})"

  val tag: Tag[Cde] = summon

  final class Select(cde: Cde, path: NonEmptyChain[String]) extends Dynamic derives CanEqual:
    def selectDynamic(name: String): Select =
      Select(cde, path :+ name)

    def ![T: Tag](using Source) =
      cde.get(path)

  final case class Mixin(source: Source, left: Cde, right: Cde) extends Cde:
    def getSource() = source
    private[cde] def get(field: String): Option[Value] =
      val getRight = right.get(field)
      if getRight.isDefined then getRight else left.get(field)

    lazy val Super: Cde = {
      right match
      case _: Leaf => left
      case mixin: Mixin => mixin.Super
    }

  final case class Leaf(source: Source, members: VectorMap[String, Value]) extends Cde:
    def getSource() = source
    private[cde] def get(field: String): Option[Value] = members.get(field)

  final case class Ctx private[cde] (
    private[cde] val self: Cde,
    private[cde] val fieldName: String,
    private[cde] val source: Source,
  )
  type Value = ValueT[?]
  final class ValueT[T](
    val source: Source,
    val name: String,
    val visibility: Visibility[T],
    val tag: Tag[T],
    val thunk: Ctx ?=> T,
  ):
    type Type = T
    def getCodecValue(ctx: Ctx): Option[CodecValue[Type]] =
      visibility match
        case visible: Visibility.ForcedVisible[?] => Some(CodecValue(thunk(using ctx), visible.codec.asInstanceOf, true))
        case visible: Visibility.Default[?] => Some(CodecValue(thunk(using ctx), visible.codec.asInstanceOf, false))
        case _ => None

  final class Builder():
    private[cde] val members = scala.collection.mutable.ArrayBuffer[Value]()
    def bind[T](name: String, visibility: Visibility[T], thunk: Ctx ?=> T)(using tag: Tag[T], src: Source) =
      members.append(new ValueT[T](src, name, visibility, tag, thunk))

    extension (name: String)
      def :=[T](thunk: Ctx ?=> T)(using Tag[T], Source, Codec[T]): Unit =
        bind(name, Visibility.Default(summon), thunk)

      def ::=[T](thunk: Ctx ?=> T)(using Tag[T], Source): Unit =
        bind(name, Visibility.Hidden, thunk)

      def :::=[T](thunk: Ctx ?=> T)(using Tag[T], Source, Codec[T]): Unit =
        bind(name, Visibility.ForcedVisible(summon), thunk)

  def apply(build: Builder ?=> Unit)(using src: Source): Cde =
    val builder = Builder()
    build(using builder)
    Leaf(src, VectorMap.from(builder.members.map(m => m.name -> m)))

def Self(using ctx: Cde.Ctx): Cde = ctx.self

object Super extends Dynamic:
  def apply(using ctx: Cde.Ctx): Cde =
    ctx.self match
    case leaf: Cde.Leaf => throw new Cde.SuperLookupError(ctx)
    case mixin: Cde.Mixin => mixin.Super

  def selectDynamic(fieldName: String)(using ctx: Cde.Ctx): Cde.Select =
    ctx.self match
    case leaf: Cde.Leaf => throw new Cde.SuperLookupError(ctx)
    case mixin: Cde.Mixin => mixin.Super.selectDynamic(fieldName)

  def ![T: Tag](using ctx: Cde.Ctx): T =
    apply.selectDynamic(ctx.fieldName).![T]

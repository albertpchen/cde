package cde.json

import scala.deriving._
import scala.compiletime.{erasedValue, summonInline}
import scala.util.NotGiven

import cde.Cde
import scala.collection.mutable

/** Type class for encoding a type into a [[JValue]]
  */
trait JValueEncoder[T]:
  def encode(t: T): JValue

object JValueEncoder:
  import JValue._

  given JValueEncoder[Int] with
    def encode(i: Int) = JInteger(i)

  given JValueEncoder[Long] with
    def encode(l: Long) = JInteger(l)

  given JValueEncoder[Float] with
    def encode(f: Float) = JDecimal(BigDecimal(f))

  given JValueEncoder[Double] with
    def encode(d: Double) = JDecimal(d)

  given JValueEncoder[String] with
    def encode(s: String) = JString(s)

  given JValueEncoder[Boolean] with
    def encode(b: Boolean) = JBoolean(b)

  given [T, S <: Iterable[T]](using elementEncoder: JValueEncoder[T]): JValueEncoder[S] with
    def encode(s: S) = JArray(s.map(elementEncoder.encode).toIndexedSeq)

  given JValueEncoder[Cde] with
    def encode(cde: Cde) =
      Cde.elaborate[JValue.JObject](cde)
        .fold(e => throw new Exception(e.mkString("\n")), identity)

  def apply[T: JValueEncoder] = summon[JValueEncoder[T]]

  /** Encoder for Tuples types
    *
    * Tuples are encoded as JArrays of their elements
    */
  inline given [T <: Tuple]: JValueEncoder[T] =
    val encoders = summonAll[T]
    new JValueEncoder[T]:
      def encode(t: T) =
        val elements = encoders.zip(t.productIterator).map {
          case (enc, e) => enc.asInstanceOf[JValueEncoder[Any]].encode(e)
        }
        JArray(elements.toIndexedSeq)

  /** Encoder for Singleton types
    *
    * Singletons are encoded as a JString of their .toString value
    */
  inline given [T](using ValueOf[T]): JValueEncoder[T] =
    new JValueEncoder[T]:
      def encode(t: T) = JString(valueOf[T].toString)

  inline given [T](using Mirror.Of[T], NotGiven[ValueOf[T]]): JValueEncoder[T] = derived[T]

  inline def derived[T](using m: Mirror.Of[T]): JValueEncoder[T] =
    lazy val encoders = summonAll[m.MirroredElemTypes]
    inline m match
       case s: Mirror.SumOf[T] => derivedSum(s, encoders)
       case p: Mirror.ProductOf[T] => derivedProduct(p, encoders)

  private inline def summonAll[T <: Tuple]: List[JValueEncoder[_]] =
    inline erasedValue[T] match
       case _: EmptyTuple => Nil
       case _: (t *: ts) => summonInline[JValueEncoder[t]] :: summonAll[ts]

  private inline def derivedSum[T](s: Mirror.SumOf[T], encoders: List[JValueEncoder[?]]): JValueEncoder[T] =
    new JValueEncoder:
      def encode(t: T) = encoders(s.ordinal(t)).asInstanceOf[JValueEncoder[Any]].encode(t)

  private inline def names[T <: Tuple]: List[String] =
    inline erasedValue[T] match
       case _: EmptyTuple => Nil
       case _: (head *: tail) => valueOf[head].toString :: names[tail]

  private inline def derivedProduct[T](p: Mirror.ProductOf[T], encoders: List[JValueEncoder[?]]): JValueEncoder[T] =
    new JValueEncoder:
      def encode(t: T) =
        val fields = encoders.zip(t.asInstanceOf[Product].productIterator).zip(names[p.MirroredElemLabels]).map { case ((e, t), name) =>
          name -> e.asInstanceOf[JValueEncoder[Any]].encode(t)
        }
        JObject(fields)

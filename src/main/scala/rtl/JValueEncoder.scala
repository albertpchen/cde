package cde

import scala.collection.mutable

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

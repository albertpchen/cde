package cde.json

import cde._
import scala.collection.mutable

/** A simple JSON AST
  */
enum JValue derives CanEqual:
  case JString(value: String)
  case JInteger(value: BigInt)
  case JDecimal(value: BigDecimal)
  case JBoolean(value: Boolean)
  case JArray(value: IndexedSeq[JValue])
  case JObject(value: Seq[(String, JValue)])

  def prettyPrint(tab: String = "  "): String =
    val builder = new StringBuilder
    prettyPrintImp(tab, 0, None, builder)
    builder.toString

  private def prettyPrintImp(tab: String, tabNum: Int, firstPrefix: Option[String], builder: StringBuilder): Unit =
    val prefix = tab * tabNum
    builder ++= firstPrefix.getOrElse(prefix)
    this match
      case JString(value) =>
        builder += '"'
        JValue.escape(value, builder)
        builder += '"'
      case JInteger(value) => builder ++= value.toString
      case JDecimal(value) => builder ++= value.toString
      case JBoolean(value) => builder ++= value.toString
      case JArray(value) if value.isEmpty => builder ++= "[]"
      case JArray(value) =>
        builder ++= "[\n"
        value.head.prettyPrintImp(tab, tabNum + 1, None, builder)
        value.tail.foreach { e =>
          builder ++= ",\n"
          e.prettyPrintImp(tab, tabNum + 1, None, builder)
        }
        builder += '\n'
        builder ++= prefix
        builder += ']'
      case JObject(value) if value.isEmpty => builder ++= "{}"
      case JObject(value) =>
        builder ++= "{\n"
        builder ++= prefix
        builder ++= tab
        builder += '"'
        JValue.escape(value.head._1, builder)
        value.head._2.prettyPrintImp(tab, tabNum + 1, Some("\": "), builder)
        value.tail.foreach { case (k, v) =>
          builder ++= ",\n"
          builder ++= prefix
          builder ++= tab
          builder += '"'
          JValue.escape(k, builder)
          v.prettyPrintImp(tab, tabNum + 1, Some("\": "), builder)
        }
        builder += '\n'
        builder ++= prefix
        builder += '}'


/** Private helper class that keeps track of a Cde lookup result
  *
  * Defined as a separate trait so that we can use type members to ensure
  * encoder and tag have the same type parameter.
  */
private trait Entry derives CanEqual:
  type Value
  def value: Value
  def encoder: Option[JValueEncoder[Value]]
  def tag: Tag[Value]
  def src: CdeSource

private object Entry:
  /** Constructs a new [[Entry]]
    */
  def apply[V](v: V, e: Option[JValueEncoder[V]], t: Tag[V], s: CdeSource): Entry =
    new Entry:
      type Value = V
      def value = v
      def encoder = e
      def tag = t
      def src = s


object JValue:
  private def escape(s: String, builder: StringBuilder): Unit =
    var idx = 0
    val len = s.length
    while idx < len do
      (s(idx): @annotation.switch) match
        case '"'  => builder ++= "\\\""
        case '\\' => builder ++= "\\\\"
        case '\b' => builder ++= "\\b"
        case '\f' => builder ++= "\\f"
        case '\n' => builder ++= "\\n"
        case '\r' => builder ++= "\\r"
        case '\t' => builder ++= "\\t"
        case c =>
          val shouldEscape = (c >= '\u0000' && c <= '\u001f')
          || (c >= '\u0080' && c < '\u00a0')
          || (c >= '\u2000' && c < '\u2100')
          if shouldEscape then
            builder ++= "\\u%04x".format(c: Int)
          else
            builder ++= c.toString
      idx += 1

  given CdeElaborator[JObject] with
    def elaborate(ctx: ElaboratedCde): JObject =
      val fields =
        for
          (name, entry) <- ctx.entries
          encoder <- entry.encoder
        yield
          name -> encoder.encode(entry.value)
      JObject(fields)

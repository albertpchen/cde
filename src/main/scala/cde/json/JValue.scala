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
    prettyPrintImp(tab, 0, "", builder)
    builder.toString

  private def prettyPrintImp(tab: String, tabNum: Int, firstPrefix: String, builder: StringBuilder): Unit =
    builder ++= firstPrefix
    val prefix = tab * tabNum
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
        value.head.prettyPrintImp(tab, tabNum + 1, "[\n", builder)
        value.tail.foreach { e =>
          e.prettyPrintImp(tab, tabNum + 1, ",\n", builder)
        }
        builder ++= prefix
        builder += ']'
      case JObject(value) if value.isEmpty => builder ++= "{}"
      case JObject(value) =>
        builder ++= "{\n"
        builder ++= prefix
        builder ++= tab
        builder += '"'
        JValue.escape(value.head._1, builder)
        value.head._2.prettyPrintImp(tab, tabNum + 1, "\": ", builder)
        value.tail.foreach { case (k, v) =>
          builder ++= ",\n"
          builder ++= prefix
          builder ++= tab
          builder += '"'
          JValue.escape(k, builder)
          v.prettyPrintImp(tab, tabNum + 1, "\": ", builder)
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
    new Entry {
      type Value = V
      def value = v
      def encoder = e
      def tag = t
      def src = s
    }

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

  /** Thrown when an error is encountered during [[Up]] or [[Site]] lookup
    *
    * These errors are caught when running [[CdeUpdate]] functions and
    * aggregated during elaboration.
    */
  private class LookupException(val src: CdeSource, msg: String) extends Exception(msg)

  private def typeErrorMessage(name: String, expected: Tag[_], found: Entry): String =
    s"""type mismatch for field "$name"
       |expected $expected
       |found ${found.tag} at ${found.src.prettyPrint()}""".stripMargin

  /** Contructs a [[CdeUpdateContext]] that preforms lookups of `ledger` at
    * index `idx`
    *
    * @param currName The field being elaborated currently
    * @param ledger The ledger of [[CdeCmd]]s
    * @param siteImp function to perform a recursive lookup for a field at
    *                certain index. Third ad fourth arguments are the source
    *                and message of the [[LookupException]] to throw if no
    *                entry is found
    * @param idx the index at which this context should perform lookups at
    */
  private def updateCtxForIdx(
    currName: String,
    ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]],
    lookupImp: (String, Int, CdeSource, String) => Either[LookupException, Entry],
    idx: Int): CdeUpdateContext =
    new CdeUpdateContext:
      def currentName: String = currName
      def up[T](name: String)(using tag: Tag[T], src: CdeSource) =
        val map = if idx <= 0 then
          throw new LookupException(src, s"""no super value for field "$name", no super Cde""")
        else
          ledger(idx - 1)
        val entry = lookupImp(name, idx, src, s"""no super value for field "$name"""").fold(throw _, identity)
        tag.castIfEquals(entry.value, entry.tag).getOrElse {
          throw new LookupException(src, typeErrorMessage(name, tag, entry))
        }

      def site[T](name: String)(using tag: Tag[T], src: CdeSource) =
        val entry = lookupImp(name, ledger.size, src, s"""no field named "$name" defined""").fold(throw _, identity)
        tag.castIfEquals(entry.value, entry.tag).getOrElse {
          throw new LookupException(src, typeErrorMessage(name, tag, entry))
        }
    end new

  given CdeElaborator[JValue.JObject] with
    def elaborate(ctx: Cde.Context): Either[Seq[CdeError], JValue.JObject] =
      /** First check for any duplicate fields and return them if any are
        * found. We cannot proceed if there are duplicate fields.
        */
      val validationErrors = mutable.ArrayBuffer[CdeError]()
      ctx.ledger.foreach { map =>
        map.foreach {
          case (k, v) if v.size > 1 =>
            validationErrors += CdeError(v.last.source, s"""duplicate field "$k" set at:\n  ${v.map(_.source.prettyPrint()).mkString("\n  ")}""")
          case _ =>
        }
      }
      if validationErrors.nonEmpty then
        return Left(validationErrors.toSeq)

      val ledger = ctx.ledger
      // Cache that stores results of lookups of fields at different indices
      val cache = mutable.HashMap[(String, Int), Either[LookupException, Entry]]()

      /** The lookup function used by [[CdeUpdateContext]]s, results of this
        * function are cached by [[cache]]
        */
      def lookupImp(name: String, idx: Int, src: CdeSource, msg: String): Either[LookupException, Entry] =
        cache.getOrElseUpdate(name -> idx,
          ledger.slice(0, idx).flatMap(_.get(name).map(_.head)) match {
            case cmds if cmds.isEmpty => Left(new LookupException(src, msg))
            case cmds =>
              try
                val entry = cmds.last match
                  case cmd: CdeCmd.Bind =>
                    Entry(cmd.value, cmd.encoder, cmd.tag, cmd.source)
                  case cmd: CdeCmd.Update =>
                    val ctx = updateCtxForIdx(name, ledger, lookupImp, idx - 1)
                    Entry(cmd.updateFn(using ctx), cmd.encoder, cmd.tag, cmd.source)
                Right(entry)
              catch
                case e: LookupException => Left(e)
          }
        )

      // List of all keys that have been set, in order of insertion
      val keys = ledger.flatten.distinctBy(_._1)

      // Buffer to collect lookup errors during elaboration
      val lookupErrors = mutable.ArrayBuffer[LookupException]()

      /** The list of elaborated fields. Caculated by running [[lookupImp]] for
        * each view from the bottom index of the ledger. These are done in a
        * try/catch that catches [[LookupException]]s and appends them to
        * [[lookupErrors]
        */
      val fields = keys.flatMap { case (name, cmds) =>
        lookupImp(name, ledger.size, cmds.head.source, s"""no field named "$name" defined""") match
          case Left(e) =>
            lookupErrors += e
            None
          case Right(e) =>
            // Some(name -> e.encoder.encode(e.value))
            e.encoder.map(enc => name -> enc.encode(e.value))
      }.toSeq

      if lookupErrors.isEmpty then
        Right(JValue.JObject(fields))
      else
        Left(lookupErrors.map(e => CdeError(e.src, e.getMessage)).distinct.toSeq)
  end given

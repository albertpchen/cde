package cde.json

import cde._
import scala.collection.mutable

enum JValue derives CanEqual:
  case JString(value: String)
  case JInteger(value: BigInt)
  case JDecimal(value: BigDecimal)
  case JBoolean(value: Boolean)
  case JArray(value: IndexedSeq[JValue])
  case JObject(value: Seq[(String, JValue)])

  def str: Option[String] =
    this match
      case JString(s) => Some(s)
      case _ => None

  def int: Option[BigInt] =
    this match
      case JInteger(i) => Some(i)
      case _ => None

  def dec: Option[BigDecimal] =
    this match
      case JDecimal(d) => Some(d)
      case _ => None

  def bool: Option[Boolean] =
    this match
      case JBoolean(b) => Some(b)
      case _ => None

private trait Entry {
  type Value
  def value: Value
  def encoder: JValueEncoder[Value]
  def tag: Tag[Value]
  def src: CdeSource
}

private object Entry {
  def apply[V](v: V, e: JValueEncoder[V], t: Tag[V], s: CdeSource): Entry =
    new Entry {
      type Value = V
      def value = v
      def encoder = e
      def tag = t
      def src = s
    }
}

object JValue:
  private class LookupException(val src: CdeSource, msg: String) extends Exception(msg)

  opaque type JBuilder = mutable.LinkedHashMap[String, mutable.ArrayBuffer[CdeCmd]]

  private def typeErrorMessage(name: String, expected: Tag[_], found: Entry): String =
    s"""type mismatch for field "$name"
       |expected $expected
       |found ${found.tag} at ${found.src.prettyPrint()}""".stripMargin

  private def updateCtxForIdx(
    currName: String,
    ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]],
    siteImp: (String, Int, CdeSource, String) => Either[LookupException, Entry],
    idx: Int): CdeUpdateContext =
    new CdeUpdateContext:
      def currentName: String = currName
      def up[T](name: String)(using tag: Tag[T], src: CdeSource) =
        val map = if idx <= 0 then
          throw new LookupException(src, s"""no super value for field "$name", no super Cde""")
        else
          ledger(idx - 1)
        val entry = siteImp(name, idx, src, s"""no super value for field "$name"""").fold(throw _, identity)
        tag.castIfEquals(entry.value, entry.tag).getOrElse {
          throw new LookupException(src, typeErrorMessage(name, tag, entry))
        }

      def site[T](name: String)(using tag: Tag[T], src: CdeSource) =
        val entry = siteImp(name, ledger.size, src, s"""no field named "$name" defined""").fold(throw _, identity)
        tag.castIfEquals(entry.value, entry.tag).getOrElse {
          throw new LookupException(src, typeErrorMessage(name, tag, entry))
        }

  given CdeElaborator[JValue.JObject] with
    def elaborate(ctx: Cde.Context): Either[Seq[CdeError], JValue.JObject] =
      val validationErrors = mutable.ArrayBuffer[CdeError]()
      ctx.ledger.foreach { map =>
        map.foreach {
          case (k, v) if v.size > 1 =>
            validationErrors += CdeError(v.last.source, s"duplicate field $k set at:\n  ${v.map(_.source.prettyPrint()).mkString("\n  ")}")
          case _ =>
        }
      }
      if validationErrors.nonEmpty then
        return Left(validationErrors.toSeq)

      val ledger = ctx.ledger
      val cache = mutable.HashMap[(String, Int), Either[LookupException, Entry]]()
      def siteImp(name: String, idx: Int, src: CdeSource, msg: String): Either[LookupException, Entry] =
        cache.getOrElseUpdate(name -> idx,
          ledger.slice(0, idx).flatMap(_.get(name).map(_.head)) match {
            case cmds if cmds.isEmpty => Left(new LookupException(src, s"""no field named "$name" defined"""))
            case cmds =>
              try
                val entry = cmds.last match
                  case cmd: SetField =>
                    Entry(cmd.value, cmd.encoder, cmd.tag, cmd.source)
                  case cmd: UpdateField =>
                    val ctx = updateCtxForIdx(name, ledger, siteImp, idx - 1)
                    Entry(cmd.updateFn(using ctx), cmd.encoder, cmd.tag, cmd.source)
                Right(entry)
              catch
                case e: LookupException => Left(e)
          }
        )

      val keys = ledger.flatten.distinctBy(_._1)
      val lookupErrors = mutable.ArrayBuffer[LookupException]()
      val fields = keys.flatMap { case (name, cmds) =>
        siteImp(name, ledger.size, cmds.head.source, s"""no field named "$name" defined""") match {
          case Left(e) =>
            lookupErrors += e
            None
          case Right(e) => Some(name -> e.encoder.encode(e.value))
        }
      }.toSeq
      if lookupErrors.isEmpty then
        Right(JValue.JObject(fields))
      else
        Left(lookupErrors.map(e => CdeError(e.src, e.getMessage)).distinct.toSeq)
  end given

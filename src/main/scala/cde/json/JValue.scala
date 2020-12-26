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
}

private[cde] object Entry {
  def apply[V](v: V, e: JValueEncoder[V], t: Tag[V]): Entry { type Value = V } =
    new Entry {
      type Value = V
      def value = v
      def encoder = e
      def tag = t
    }
}

object JValue:
  private class LookupException(val src: CdeSource, msg: String) extends Exception(msg)

  opaque type JBuilder = mutable.LinkedHashMap[String, mutable.ArrayBuffer[CdeCmd]]

  private def findEntry(
    src: CdeSource,
    name: String,
    cmds: scala.collection.Seq[CdeCmd],
    ctx: CdeUpdateContext
  ): Entry = 
    val startIdx = cmds.lastIndexWhere(_.isInstanceOf[SetField])
    if startIdx < 0 then
      throw new LookupException(src, """cannot update field "$name", no initial value set""")
    else
      val filteredCmds = cmds.drop(startIdx - 1)
      val head = filteredCmds.head.asInstanceOf[SetField]
      val init: Entry = Entry(head.value, head.encoder, head.tag)
      val entry = filteredCmds.tail.foldLeft(init) {
        case (_, cmd: SetField) =>
          Entry(cmd.value, cmd.encoder, cmd.tag)
        case (e, cmd: UpdateField) if e.tag.isSameType(cmd.tag) =>
          Entry(cmd.updateFn(using ctx).asInstanceOf[e.Value], e.encoder, e.tag)
        case (e, cmd: UpdateField) =>
            throw new LookupException(cmd.source, s"cannot update field ${cmd.name} with type mismatch: found ${e.tag}, expected ${cmd.tag}")
      }
      entry

  given CdeElaborator[JValue.JObject] with
    def elaborate(ctx: Cde.Context): Either[Seq[CdeError], JValue.JObject] =
      val validationErrors = mutable.ArrayBuffer[CdeError]()
      ctx.ledger.foreach { map =>
        map.foreach { case (k, v) =>
          if v.size > 1 then validationErrors += CdeError(CdeSource.Empty, s"duplicate field $k: ${v.map(_.source).mkString(", ")}")
        }
      }
      if validationErrors.nonEmpty then
        return Left(validationErrors.toSeq)

      val ledger = ctx.ledger
      val cache = mutable.HashMap[String, Either[LookupException, Entry]]()
      var updateCtx: CdeUpdateContext = null // set null here because ctx.here and getEntry are mutually recursive
      var currName: String = null // set null here because ctx.here and getEntry are mutually recursive
      def siteImp(name: String): Either[LookupException, Entry] =
        cache.getOrElseUpdate(name,
          ledger.flatMap(_.get(name).map(_.head)) match {
            case cmds if cmds.isEmpty => Left(new LookupException(CdeSource.Empty, s"""no field named "$name" defined"""))
            case cmds =>
              currName = name
              try {
                Right(findEntry(CdeSource.Empty, name, cmds, updateCtx))
              } catch {
                case e: LookupException => Left(e)
              }
          }
        )
      updateCtx = new CdeUpdateContext:
        def currentName: String = currName
        def up[T](name: String)(using tag: Tag[T], src: CdeSource) =
          val entry = ledger.dropRight(1).flatMap(_.get(name).map(_.head)) match {
            case cmds if cmds.isEmpty => throw new LookupException(src, s"""no super value for field "$name"""")
            case cmds => findEntry(src, name, cmds, updateCtx)
          }
          tag.castIfEquals(entry.value, entry.tag).getOrElse(
            throw new LookupException(src, s"""type mismatch for field "$name": found ${entry.tag}, expected $tag"""))
        def site[T](name: String)(using tag: Tag[T], src: CdeSource) =
          val entry = siteImp(name).fold(e => throw new LookupException(src, e.getMessage), identity)
          tag.castIfEquals(entry.value, entry.tag).getOrElse(
            throw new LookupException(src, s"""type mismatch for field "$name": found ${entry.tag}, expected $tag"""))

      val keys = ledger.flatMap(_.keys).distinct
      val lookupErrors = mutable.ArrayBuffer[LookupException]()
      val fields = keys.flatMap { name =>
        siteImp(name) match {
          case Left(e) =>
            lookupErrors += e
            None
          case Right(e) => Some(name -> e.encoder.encode(e.value))
        }
      }.toSeq
      if lookupErrors.isEmpty then
        Right(JValue.JObject(fields))
      else
        fields.foreach(println)
        Left(lookupErrors.map(e => CdeError(e.src, e.getMessage)).toSeq)

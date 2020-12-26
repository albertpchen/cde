package cde

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
  opaque type JBuilder = mutable.LinkedHashMap[String, mutable.ArrayBuffer[OMCmd]]

  private def findEntry(name: String, cmds: scala.collection.Seq[OMCmd], ctx: OMUpdateContext): Entry = 
    val startIdx = cmds.lastIndexWhere(_.isInstanceOf[SetField])
    if startIdx < 0 then
      throw new Exception(s"""cannot update field "$name", no initial value set""")
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
            throw new Exception(s"cannot update field ${cmd.name} with type mismatch: found ${e.tag}, expected ${cmd.tag}")
      }
      entry

  given CdeElaborator[JValue.JObject] with
    def elaborate(ctx: Cde.Context): JValue.JObject =
      val errors = mutable.ArrayBuffer[String]()
      ctx.ledger.foreach { map =>
        map.foreach { case (k, v) =>
          if v.size > 1 then errors += s"duplicate field $k"
        }
      }
      if errors.nonEmpty then
        return throw new Exception(s"failed to elaborate Cde:\n  ${errors.mkString("\n  ")}")

      val ledger = ctx.ledger
      val cache = mutable.HashMap[String, Entry]()
      var updateCtx: OMUpdateContext = null // set null here because ctx.here and getEntry are mutually recursive
      def siteImp(name: String): Entry =
        cache.getOrElseUpdate(name, {
          ledger.flatMap(_.get(name).map(_.head)) match {
            case cmds if cmds.isEmpty => throw new Exception(s"""no field named "$name" defined""")
            case cmds => findEntry(name, cmds, updateCtx)
          }
        })
      updateCtx = new OMUpdateContext:
        def up[T](name: String)(using tag: Tag[T]) =
          val entry = ledger.dropRight(1).flatMap(_.get(name).map(_.head)) match {
            case cmds if cmds.isEmpty => throw new Exception(s"""no super value for field "$name"""")
            case cmds => findEntry(name, cmds, updateCtx)
          }
          if entry.tag.isSameType(tag) then
            entry.value.asInstanceOf[T]
          else
            throw new Exception(s"""type mismatch for field "$name": found ${entry.tag}, expected $tag""")
        def site[T](name: String)(using tag: Tag[T]) =
          val entry = siteImp(name)
          if entry.tag.isSameType(tag) then
            entry.value.asInstanceOf[T]
          else
            throw new Exception(s"""type mismatch for field "$name": found ${entry.tag}, expected $tag""")

      if errors.isEmpty then
        val keys = ledger.flatMap(_.keys).distinct
        JValue.JObject(keys.map { name =>
          val entry = siteImp(name)
          name -> entry.encoder.encode(entry.value)
        }.toSeq)
      else
        throw new Exception(s"failed to elaborate JValue:\n${errors.mkString("\n")}")

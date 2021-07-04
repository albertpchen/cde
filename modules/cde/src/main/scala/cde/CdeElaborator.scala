package cde

import cde._
import cde.json.{JValue, JValueEncoder}
import scala.collection.mutable

/** Thrown when an error is encountered during [[Up]] or [[Site]] lookup
  *
  * These errors are caught when running [[CdeUpdate]] functions and
  * aggregated during elaboration.
  */
private[cde] class LookupException(val src: CdeSource, msg: String) extends Exception(msg)

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

sealed trait ElaboratedCde:
  val entries: Seq[(String, Entry)]

sealed abstract class CdeEvaluator:
  def getField(name: String, src: CdeSource): Either[LookupException, Entry]


trait CdeElaborator[T]:
  def elaborate(ctx: ElaboratedCde): T

object CdeElaborator:

  private def typeErrorMessage(name: String, expected: Tag[_], found: Entry): String =
    s"""type mismatch for field "$name"
       |expected $expected
       |found ${found.tag} at ${found.src.prettyPrint()}""".stripMargin

  private case class FieldLookup(name: String, id: CdeId)

  private[cde] def elaborate(ctx: Cde): CdeEvaluator =
    val builders = ctx.builders
    val cmdLookupMap = mutable.HashMap[FieldLookup, CdeCmd]()
    val parentMap = mutable.HashMap[CdeId, CdeId]()
    builders.sliding(2).foreach { pair =>
      if pair.size == 2 then
        parentMap(pair(1).id) = pair(0).id
    }
    val siteId = builders.last.id
    for
      builder <- builders
      id = builder.id
      cmd <- builder.flattenedCommands
    do
      cmdLookupMap(FieldLookup(cmd.name, id)) = cmd

    val cache = mutable.HashMap[FieldLookup, Either[LookupException, Entry]]()

    def lookupParent(fieldLookup: FieldLookup, src: CdeSource): Either[LookupException, CdeId] =
      if parentMap.contains(fieldLookup.id) then
        Right(parentMap(fieldLookup.id))
      else
        Left(new LookupException(
          src,
          s"""no super value for field "${fieldLookup.name}""""
        ))

    def makeUpdateCtx(currName: String, id: CdeId): CdeUpdateContext =
      new CdeUpdateContext:
        private[cde] def getUpdateContextForId(id: CdeId)(using CdeSource): CdeUpdateContext =
          makeUpdateCtx(currName, id)
        private[cde] def currentName: String = currName
        private[cde] def up[T: Tag](name: String)(using src: CdeSource): T =
          lookupParent(FieldLookup(name, id), src).flatMap { parentId =>
            lookupFn(FieldLookup(name, parentId), src)
          }.fold(
            e => throw e,
            e => if e.tag.isSameType(summon[Tag[T]]) then
              e.value.asInstanceOf[T]
            else
              throw new LookupException(src, typeErrorMessage(name, summon[Tag[T]], e))
          )

        private[cde] def site[T: Tag](name: String)(using src: CdeSource): T =
          lookupFn(FieldLookup(name, siteId), src, Some(s"""no field named "$name" defined""")).fold(
            e => throw e,
            e => if e.tag.isSameType(summon[Tag[T]]) then
              e.value.asInstanceOf[T]
            else
              throw new LookupException(src, typeErrorMessage(name, summon[Tag[T]], e))
          )

    def lookupFn(fieldLookup: FieldLookup, src: CdeSource, msg: Option[String] = None): Either[LookupException, Entry] =
      cache.getOrElseUpdate(fieldLookup, {
        if !cmdLookupMap.contains(fieldLookup) then
          lookupParent(fieldLookup, src).fold(
            e => if msg.isDefined then Left(new LookupException(src, msg.get)) else Left(e),
            parentId => lookupFn(fieldLookup.copy(id = parentId), src, msg)
          )
        else
          cmdLookupMap(fieldLookup) match
            case b: CdeCmd.Bind => Right(Entry(b.value, b.encoder, b.tag, b.source))
            case u: CdeCmd.Update =>
              val updateCtx = makeUpdateCtx(fieldLookup.name, fieldLookup.id)
              try
                val value = u.updateFn(using updateCtx)
                Right(Entry(value, u.encoder, u.tag, u.source))
              catch
                case e: LookupException => Left(e)
      })
    new CdeEvaluator:
      def getField(name: String, src: CdeSource): Either[LookupException, Entry] =
        lookupFn(FieldLookup(name, siteId), src)
  
  def elaborated(cde: Cde): Either[Seq[CdeError], ElaboratedCde] =
    val errors = mutable.ArrayBuffer[LookupException]()
    val evaluator = elaborate(cde)
    val fields = cde.ledger.foldLeft(new mutable.LinkedHashMap[String, Option[CdeSource]]) { case (acc, (_, cmds)) =>
      cmds.foreach {
        case (key, cmd) if !acc.contains(key) =>
          val present = cmd.last match
            case b: CdeCmd.Bind => if b.encoder.isDefined then Some(b.source) else None
            case u: CdeCmd.Update => if u.encoder.isDefined then Some(u.source) else None
          acc(key) = present
        case _ =>
      }
      acc
    }
    val result = fields.foldLeft(Right(Seq.empty): Either[Seq[CdeError], Seq[(String, Entry)]]) {
      case (Right(fields), (name, src)) if src.isDefined =>
        evaluator.getField(name, src.get).fold(
          e => Left(Seq(CdeError(e.src, e.getMessage))),
          f => Right((name -> f) +: fields),
        )
      case (e, _) => e
    }
    result.map { reversed =>
      new ElaboratedCde:
        val entries: Seq[(String, Entry)] = reversed.reverse
    }

package cde

import cde._
import scala.collection.mutable

/** Thrown when an error is encountered during [[Up]] or [[Site]] lookup
  *
  * These errors are caught when running [[CdeUpdate]] functions and
  * aggregated during elaboration.
  */
//private[cde] class LookupException(val src: CdeSource, msg: String) extends Exception(msg)
private[cde] class CdeException(val errors: Seq[CdeError]) extends Exception(errors.head.message)

/** Private helper class that keeps track of a Cde lookup result
  *
  * Defined as a separate trait so that we can use type members to ensure
  * encoder and tag have the same type parameter.
  */
private trait Entry derives CanEqual:
  type Value
  def value: Value
  def visibility: CdeVisibility
  def tag: Tag[Value]
  def src: CdeSource

private object Entry:
  def apply[V](v: V, vis: CdeVisibility, t: Tag[V], s: CdeSource): Entry =
    new Entry:
      type Value = V
      def value = v
      def visibility = vis
      def tag = t
      def src = s

sealed trait ElaboratedCde:
  val entries: Seq[(String, Entry)]

sealed abstract class CdeEvaluator private[cde] (private[cde] val cde: Cde):
  def getField(name: String, src: CdeSource): Either[Seq[CdeError], Entry]
  def getField(name: String, subFields: Seq[String], src: CdeSource): Either[Seq[CdeError], Entry]


trait CdeElaborator[T]:
  def elaborate(ctx: ElaboratedCde): T

object CdeElaborator:
  type Errors = Seq[CdeError]

  private def typeErrorMessage(name: String, expected: Tag[_], found: Entry): String =
    s"""type mismatch for field "$name"
       |expected $expected
       |found ${found.tag} at ${found.src.prettyPrint()}""".stripMargin

  private case class FieldLookup(name: String, id: CdeId)

  private val cdeTag = summon[Tag[Cde]]
  private val cdeEvaluatorTag = summon[Tag[CdeEvaluator]]
  private[cde] def elaborate(ctx: Cde): CdeEvaluator =
    val builders = ctx.builders
    val cmdLookupMap = mutable.HashMap[FieldLookup, CdeCmd]()
    val parentMap = mutable.HashMap[CdeId, CdeId]()
    builders.sliding(2).foreach {
      case pair if pair.size == 2 => parentMap(pair(1).id) = pair(0).id
      case pair =>
    }
    val siteId = builders.last.id
    for
      builder <- builders
      id = builder.id
      cmd <- builder.flattenedCommands
    do
      cmdLookupMap(FieldLookup(cmd.name, id)) = cmd

    val cache = mutable.HashMap[FieldLookup, Either[Errors, Entry]]()

    def lookupParent(fieldLookup: FieldLookup, src: CdeSource): Either[Errors, CdeId] =
      if parentMap.contains(fieldLookup.id) then
        Right(parentMap(fieldLookup.id))
      else
        Left(Seq(CdeError(src, s"""no super value for field "${fieldLookup.name}"""")))

    val evaluatorCache = mutable.HashMap[FieldLookup, CdeEvaluator]()
    def evaluateSubFields(name: String, id: CdeId, subFields: Seq[String], msg: Option[String] = None)(using src: CdeSource): Either[Errors, Entry] =
      lookupFn(FieldLookup(name, id), src, msg).flatMap { result =>
        var restFields = subFields
        if subFields.nonEmpty then
          if result.tag.isSameType(cdeTag) then
            val evaluator = evaluatorCache.getOrElseUpdate(
              FieldLookup(name, id),
              elaborate(result.value.asInstanceOf[Cde])
            )
            evaluator.getField(subFields.head, subFields.tail, src)
          else
            Left(Seq(CdeError(src, typeErrorMessage(name, cdeTag, result))))
        else
          Right(result)
      }

    def makeUpdateCtx(currName: String, id: CdeId): CdeUpdateContext =
      new CdeUpdateContext:
        private[cde] def getUpdateContextForId(id: CdeId)(using CdeSource): CdeUpdateContext = makeUpdateCtx(currName, id)
        private[cde] def currentName: String = currName
        private[cde] def up[T: Tag](name: String, subFields: Seq[String])(using src: CdeSource): T =
          lookupParent(FieldLookup(name, id), src).flatMap { parentId =>
            evaluateSubFields(name, parentId, subFields)
          }.fold(
            e => throw CdeException(e),
            {
              case e if e.tag.isSameType(summon[Tag[T]]) => e.value.asInstanceOf[T]
              case e => throw new CdeException(Seq(CdeError(src, typeErrorMessage(name, summon[Tag[T]], e))))
            }
          )
        private[cde] def site[T: Tag](name: String, subFields: Seq[String])(using src: CdeSource): T =
          evaluateSubFields(name, siteId, subFields, Some(s"""no field named "$name" defined""")).fold(
            e => throw CdeException(e),
            {
              case e if e.tag.isSameType(summon[Tag[T]]) => e.value.asInstanceOf[T]
              case e => throw new CdeException(Seq(CdeError(src, typeErrorMessage(name, summon[Tag[T]], e))))
            }
          )

    def lookupFn(fieldLookup: FieldLookup, src: CdeSource, msg: Option[String] = None): Either[Errors, Entry] =
      cache.getOrElseUpdate(fieldLookup, {
        if !cmdLookupMap.contains(fieldLookup) then
          lookupParent(fieldLookup, src).fold(
            e => if msg.isDefined then Left(Seq(CdeError(src, msg.get))) else Left(e),
            parentId => lookupFn(fieldLookup.copy(id = parentId), src, msg)
          )
        else
          cmdLookupMap(fieldLookup) match
            case b: CdeCmd.Bind => Right(Entry(b.value, b.visibility, b.tag, b.source))
            case u: CdeCmd.Update =>
              val updateCtx = makeUpdateCtx(fieldLookup.name, fieldLookup.id)
              try
                val value = u.updateFn(using updateCtx)
                Right(Entry(value, u.visibility, u.tag, u.source))
              catch
                case e: CdeException => Left(e.errors)
      })

    def evaluate(lookup: FieldLookup, result: Either[Errors, Entry]): Either[Errors, Entry] =
      result.map { entry =>
        if entry.tag.isSameType(cdeTag) then
          val evaluator = evaluatorCache.getOrElseUpdate(
            lookup,
            elaborate(entry.value.asInstanceOf[Cde])
          )
          Entry(evaluator, entry.visibility, cdeEvaluatorTag, entry.src)
        else entry
      }

    new CdeEvaluator(ctx):
      def getField(name: String, src: CdeSource): Either[Errors, Entry] =
        val lookup = FieldLookup(name, siteId)
        evaluate(lookup, lookupFn(lookup, src))

      def getField(name: String, subFields: Seq[String], src: CdeSource): Either[Seq[CdeError], Entry] =
        evaluate(FieldLookup(name, siteId), evaluateSubFields(name, siteId, subFields)(using src))
  
  
  private[cde] def elaborated(cde: Cde): Either[Seq[CdeError], ElaboratedCde] =
    val evaluator = elaborate(cde)
    evaluate(evaluator)

  private[cde] def evaluate(evaluator: CdeEvaluator): Either[Seq[CdeError], ElaboratedCde] =
    val errors = mutable.ArrayBuffer[CdeException]()
    val fields = evaluator.cde.ledger.foldLeft(new mutable.LinkedHashMap[String, Option[CdeSource]]) { case (acc, (_, cmds)) =>
      cmds.foreach {
        case (key, cmd) if !acc.contains(key) =>
          val present = cmd.last match
            case b: CdeCmd.Bind => if b.visibility.isVisible then Some(b.source) else None
            case u: CdeCmd.Update => if u.visibility.isVisible then Some(u.source) else None
          acc(key) = present
        case _ =>
      }
      acc
    }
    val result = fields.foldLeft(Right(Seq.empty): Either[Seq[CdeError], Seq[(String, Entry)]]) {
      case (Right(fields), (name, src)) if src.isDefined =>
        evaluator.getField(name, src.get).flatMap { entry =>
          if entry.tag.isSameType(cdeEvaluatorTag) then
            evaluate(entry.value.asInstanceOf[CdeEvaluator]).map { value =>
              (name -> Entry(value, entry.visibility, Tag[ElaboratedCde], entry.src)) +: fields
            }
          else
            Right((name -> entry) +: fields)
        }
      case (e, _) => e
    }
    result.map { reversed =>
      new ElaboratedCde:
        val entries: Seq[(String, Entry)] = reversed.reverse
    }

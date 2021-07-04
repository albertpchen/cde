package cde

import cde.json.JValueEncoder

import scala.collection.{mutable => mut}
import scala.language.dynamics

/** Represents a Cde field assignment operation
  */
final class CdeHandle private[cde] (val source: CdeSource, val _id: CdeId) extends Dynamic:

  override def toString: String = s"Handle@${source.prettyPrint()}"

  /** Looks up a field from the top-level [[Cde]] by a string parameter
    */
  def apply[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    selectDynamic(name)

  /** Looks up a field from the top-level [[Cde]] using method syntax
    */
  def selectDynamic[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    summon[CdeUpdateContext].getUpdateContextForId(_id).site[T](name, Seq.empty)

opaque type CdeId = Int
object CdeId:
  private var nextId = 0
  def apply(): CdeId =
    val result = nextId
    nextId += 1
    result

/** Builder for building up a [[Cde]] instance
  */
sealed trait CdeBuilder derives CanEqual:
  /** Unique ID of this builder instance
    */
  private[cde] def id: CdeId

  /** The instantiation site of this builder
    */
  private[cde] def source: CdeSource

  /** The list of [[CdeCmd]]s executed in the context of this builder
    */
  private[cde] val ledger: mut.ArrayBuffer[CdeCmd] = new mut.ArrayBuffer[CdeCmd]()

  /** Appends a [[CdeCmd]] to this builder
    */
  private[cde] def addCmd(cmd: CdeCmd): Unit = ledger += cmd

  private[cde] def getHandle(src: CdeSource): CdeHandle =
    new CdeHandle(src, id)

  private[cde] def flattenedCommands: Seq[CdeCmd] =
    var result = Seq.empty[CdeCmd]
    val seen = mut.HashSet.empty[String]
    var idx = ledger.size - 1
    while idx >= 0 do
      val currCmd = ledger(idx)
      idx -= 1
      if !seen(currCmd.name) then
        result = currCmd +: result
    result


/** An immutable representation of a Context Dependend Environment
  *
  * This can be elaborated into useful data structures by using the
  * [[Cde.elaborate]] method.
  */
sealed trait Cde derives CanEqual:
  /** Returns the [[Cde]] resulting from stacking this Cde on top of another
    */
  def mixin(overrides: Cde): Cde = new MixinCde(this, overrides)

  private[cde] def ledger: collection.IndexedSeq[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])]

  final private[cde] val builders: collection.IndexedSeq[CdeBuilder] =
    val acc = mut.ArrayBuffer[CdeBuilder]()
    addBuilders(acc)
    acc

  private def addBuilders(acc: mut.ArrayBuffer[CdeBuilder]): Unit =
    this match
    case m: MixinCde =>
      m.up.addBuilders(acc)
      m.mixin.addBuilders(acc)
    case l: LeafCde =>
      acc += l.builder

private[cde] class MixinCde(val up: Cde, val mixin: Cde) extends Cde {
  private[cde] final lazy val ledger: collection.IndexedSeq[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])] =
    Cde.expandLedger(this)
}

private[cde] final class LeafCde(val builder: CdeBuilder) extends Cde {
  private[cde] override val ledger: collection.IndexedSeq[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])] = {
    val mapLedger = mut.LinkedHashMap[String, scala.collection.Seq[CdeCmd]]()
    val groups = builder.ledger.groupBy(_.name)
    builder.ledger.foreach {
      case cmd if !mapLedger.contains(cmd.name) => mapLedger(cmd.name) = groups(cmd.name)
      case _ =>
    }
    IndexedSeq(builder -> mapLedger)
  }
}

object Cde:
  private[cde] def expandLedger(cde: Cde): collection.IndexedSeq[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])] = {
    val ledger = mut.ArrayBuffer[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])]()
    expandLedgerImp(cde, ledger)
    ledger
  }
  private def expandLedgerImp(
    cde: Cde,
    acc: mut.ArrayBuffer[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])]
  ): Unit =
    cde match {
      case m: MixinCde =>
        expandLedgerImp(m.up, acc)
        expandLedgerImp(m.mixin, acc)
      case l: LeafCde => acc ++= l.ledger
    }
  /** Context that is passed to [[CdeElaborato]]s during elaboration
    *
    * This is just a view of the internal data structure of the [[Cde]]'s
    * ledger.
    */
  opaque type Context = Cde

  object Context:
    extension (ctx: Context)
      /** Returns a list of maps representing the list of [[Cde]]s that the
        * top-level [[Cde]] is comprised of
        */
      def ledger: collection.IndexedSeq[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])] =
        Cde.expandLedger(ctx)

  /** Elaborates a [[Cde]], given a [[CdeElaborator]] instance for the
    * elaborated type
    */
  def elaborate[T: CdeElaborator](node: Cde): Either[Seq[CdeError], T] =
    CdeElaborator.elaborated(node).map { elaborated =>
      summon[CdeElaborator[T]].elaborate(elaborated)
    }

  /** Constructs a [[Cde]] by running a builder function in the context of a
    * [[CdeBuilder]]
    */
  def apply(fn: CdeBuilder ?=> Unit)(using src: CdeSource): Cde =
    val builder = new CdeBuilder:
      val id = CdeId()
      val source = src
    fn(using builder)
    new LeafCde(builder)

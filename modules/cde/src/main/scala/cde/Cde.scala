package cde

import cde.json.JValueEncoder

import scala.collection.mutable
import scala.language.dynamics

/** Represents a Cde field assignment operation
  */
final class CdeHandle private[cde] (val source: CdeSource, val _cde: CdeBuilder) extends Dynamic:

  override def toString: String = s"Handle@${source.prettyPrint()}"

  /** Looks up a field from the top-level [[Cde]] by a string parameter
    */
  def apply[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    selectDynamic(name)

  /** Looks up a field from the top-level [[Cde]] using method syntax
    */
  def selectDynamic[T: Tag](name: String)(using CdeUpdateContext, CdeSource): T =
    summon[CdeUpdateContext].getUpdateContextForBuilder(_cde).site[T](name)

/** Builder for building up a [[Cde]] instance
  */
sealed trait CdeBuilder derives CanEqual:
  /** The instantiation site of this builder
    */
  private[cde] def source: CdeSource

  /** The list of [[CdeCmd]]s executed in the context of this builder
    */
  private[cde] val ledger: mutable.ArrayBuffer[CdeCmd] = new mutable.ArrayBuffer[CdeCmd]()
  private[cde] val handles: mutable.ArrayBuffer[CdeHandle] = new mutable.ArrayBuffer[CdeHandle]()

  /** Appends a [[CdeCmd]] to this builder
    */
  private[cde] def addCmd(cmd: CdeCmd): Unit = ledger += cmd

  private[cde] def getHandle(src: CdeSource): CdeHandle =
    val handle = new CdeHandle(src, this)
    handles += handle
    handle


/** An immutable representation of a Context Dependend Environment
  *
  * This can be elaborated into useful data structures by using the
  * [[Cde.elaborate]] method.
  */
sealed trait Cde derives CanEqual:
  /** Returns the [[Cde]] resulting from stacking this Cde on top of another
    */
  def mixin(overrides: Cde): Cde = new MixinCde(this, overrides)

private[cde] class MixinCde(val up: Cde, val mixin: Cde) extends Cde

private[cde] final class LeafCde(val builder: CdeBuilder) extends Cde {
  val ledger: collection.SeqMap[String, collection.Seq[CdeCmd]] = {
    val mapLedger = mutable.LinkedHashMap[String, scala.collection.Seq[CdeCmd]]()
    val groups = builder.ledger.groupBy(_.name)
    builder.ledger.foreach {
      case cmd if !mapLedger.contains(cmd.name) => mapLedger(cmd.name) = groups(cmd.name)
      case _ =>
    }
    mapLedger
  }
}

object Cde:
  private def ledger(cde: Cde): IndexedSeq[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])] =
    cde match {
      case m: MixinCde => ledger(m.up) ++ ledger(m.mixin)
      case l: LeafCde => IndexedSeq(l.builder -> l.ledger)
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
      def ledger: IndexedSeq[(CdeBuilder, collection.SeqMap[String, collection.Seq[CdeCmd]])] =
        Cde.ledger(ctx)

  /** Elaborates a [[Cde]], given a [[CdeElaborator]] instance for the
    * elaborated type
    */
  def elaborate[T: CdeElaborator](node: Cde): Either[Seq[CdeError], T] =
    summon[CdeElaborator[T]].elaborate(node)

  /** Constructs a [[Cde]] by running a builder function in the context of a
    * [[CdeBuilder]]
    */
  def apply(fn: CdeBuilder ?=> Unit)(using src: CdeSource): Cde =
    val builder = new CdeBuilder:
      val source = src
    fn(using builder)
    new LeafCde(builder)

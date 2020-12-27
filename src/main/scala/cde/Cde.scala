package cde

import cde.json.JValueEncoder

import scala.collection.mutable

/** Builder for building up a [[Cde]] instance
  */
sealed trait CdeBuilder derives CanEqual:
  /** The instantiation site of this builder
    */
  private[cde] def source: CdeSource

  /** The list of [[CdeCmd]]s executed in the context of this builder
    */
  private val ledger: mutable.ArrayBuffer[CdeCmd] = new mutable.ArrayBuffer[CdeCmd]()

  /** Appends a [[CdeCmd]] to this builder
    */
  private[cde] def addCmd(cmd: CdeCmd): Unit = ledger += cmd

  /** Converts this builder into a [[Cde]] instance
    */
  private[cde] def toNode[T]: Cde =
    val mapLedger = mutable.LinkedHashMap[String, scala.collection.Seq[CdeCmd]]()
    val groups = ledger.groupBy(_.name)
    ledger.foreach {
      case cmd if !mapLedger.contains(cmd.name) => mapLedger(cmd.name) = groups(cmd.name)
      case _ =>
    }
    Cde.make(IndexedSeq(source), IndexedSeq(mapLedger))


/** An immutable representation of a Context Dependend Environment
  *
  * This can be elaborated into useful data structures by using the
  * [[Cde.elaborate]] method.
  */
sealed trait Cde derives CanEqual:
  private[cde] def ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]

  /** The source locator of the site where this Cde was constructed
    */
  def sources: IndexedSeq[CdeSource]

  /** Returns the [[Cde]] resulting from stacking this Cde on top of another
    */
  def extend(mixin: Cde): Cde =
    Cde.make(sources ++ mixin.sources, ledger ++ mixin.ledger)

object Cde:
  /** Context that is passed to [[CdeElaborato]]s during elaboration
    *
    * This is just a view of the internal data structure of the [[Cde]]'s
    * ledger.
    */
  opaque type Context = IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]

  object Context:
    extension (ctx: Context)
      /** Returns a list of maps representing the list of [[Cde]]s that the
        * top-level [[Cde]] is comprised of
        */
      def ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]] = ctx

  private[cde] def make(src: IndexedSeq[CdeSource], l: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]): Cde =
    new Cde:
      val sources = src
      val ledger = l

  /** Elaborates a [[Cde]], given a [[CdeElaborator]] instance for the
    * elaborated type
    */
  def elaborate[T: CdeElaborator](node: Cde): Either[Seq[CdeError], T] =
    summon[CdeElaborator[T]].elaborate(node.ledger)

  /** Constructs a [[Cde]] by running a builder function in the context of a
    * [[CdeBuilder]]
    */
  def apply(fn: CdeBuilder ?=> Unit)(using src: CdeSource): Cde =
    val builder = new CdeBuilder:
      val source = src
    fn(using builder)
    builder.toNode

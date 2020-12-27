package cde

import cde.json.JValueEncoder

import scala.collection.mutable

sealed trait CdeBuilder:
  private[cde] def source: CdeSource

  private val ledger: mutable.ArrayBuffer[CdeCmd] = new mutable.ArrayBuffer[CdeCmd]()

  private[cde] def addCmd(cmd: CdeCmd): Unit =
    ledger += cmd

  private[cde] def toNode[T]: Cde =
    val mapLedger = mutable.LinkedHashMap[String, scala.collection.Seq[CdeCmd]]()
    val groups = ledger.groupBy(_.name)
    ledger.foreach {
      case cmd if !mapLedger.contains(cmd.name) => mapLedger(cmd.name) = groups(cmd.name)
      case _ =>
    }
    Cde.make(IndexedSeq(source), IndexedSeq(mapLedger))

sealed trait Cde:
  private[cde] def ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]

  def sources: IndexedSeq[CdeSource]

  def +(mixin: Cde): Cde =
    Cde.make(sources ++ mixin.sources, ledger ++ mixin.ledger)


object Cde:
  opaque type Context = IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]

  object Context:
    extension (ctx: Context)
      def ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]] = ctx

  private[cde] def make(src: IndexedSeq[CdeSource], l: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]): Cde =
    new Cde:
      val sources = src
      val ledger = l

  def elaborate[T: CdeElaborator](node: Cde): Either[Seq[CdeError], T] =
    summon[CdeElaborator[T]].elaborate(node.ledger)

  def apply(fn: CdeBuilder ?=> Unit)(using src: CdeSource): Cde =
    val builder = new CdeBuilder {
      val source = src
    }
    fn(using builder)
    builder.toNode

package cde

import cde.json.JValueEncoder

import scala.collection.mutable

sealed trait CdeBuilder:
  private[cde] def source: CdeSource.File
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
    Cde.make(CdeSource.Multiple(Seq(source)), IndexedSeq(mapLedger))

  extension [T: JValueEncoder : Tag](name: String)
    def := (v: T)(using CdeBuilder, CdeSource.File): Unit =
      OMField(name, v)

    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder, CdeSource.File): Unit =
      OMUpdate(name, fn)

sealed trait Cde:
  def source: CdeSource.Multiple
  private[cde] def ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]
  def +(mixin: Cde): Cde =
    Cde.make(CdeSource.Multiple(source.sources ++ mixin.source.sources), ledger ++ mixin.ledger)

object Cde:
  opaque type Context = IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]
  object Context:
    extension (ctx: Context)
      def ledger: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]] = ctx

  private[cde] def make(src: CdeSource.Multiple, l: IndexedSeq[collection.SeqMap[String, collection.Seq[CdeCmd]]]): Cde =
    new Cde:
      val source = src
      val ledger = l

  def elaborate[T: CdeElaborator](node: Cde): Either[Seq[CdeError], T] =
    summon[CdeElaborator[T]].elaborate(node.ledger)

  def apply(fn: CdeBuilder ?=> Unit)(using src: CdeSource.File): Cde =
    val builder = new CdeBuilder {
      val source = src
    }
    fn(using builder)
    builder.toNode

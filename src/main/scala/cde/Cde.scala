package cde

import cde.json.JValueEncoder

import scala.collection.mutable

sealed trait CdeBuilder:
  private[cde] def source: CdeSource.File
  private val ledger: mutable.ArrayBuffer[CdeCmd] = new mutable.ArrayBuffer[CdeCmd]()

  private[cde] def addCmd(cmd: CdeCmd): Unit =
    ledger += cmd

  private[cde] def toNode[T]: Cde =
    val mapLedger = mutable.LinkedHashMap[String, Seq[CdeCmd]]()
    ledger.reverse.map { case cmd =>
      mapLedger(cmd.name) = cmd +:mapLedger.getOrElse(cmd.name, Seq.empty)
    }
    Cde.make(CdeSource.Multiple(Seq(source)), Seq(mapLedger))

  extension [T: JValueEncoder : Tag](name: String)
    def := (v: T)(using CdeBuilder, CdeSource.File): Unit =
      OMField(name, v)

    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder, CdeSource.File): Unit =
      OMUpdate(name, fn)

sealed trait Cde:
  def source: CdeSource.Multiple
  private[cde] def ledger: Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]]
  def +(mixin: Cde): Cde =
    Cde.make(CdeSource.Multiple(source.sources ++ mixin.source.sources), ledger ++ mixin.ledger)

object Cde:
  opaque type Context = Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]]
  object Context:
    extension (ctx: Context)
      def ledger: Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]] = ctx

  private[cde] def make(src: CdeSource.Multiple, l: Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]]): Cde =
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

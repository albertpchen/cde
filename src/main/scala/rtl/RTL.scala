package cde

import scala.collection.mutable

sealed trait CdeBuilder:

  private val ledger: mutable.ArrayBuffer[CdeCmd] = new mutable.ArrayBuffer[CdeCmd]()

  private[cde] def addCmd(cmd: CdeCmd): Unit =
    ledger += cmd

  private[cde] def toNode[T]: Cde =
    val mapLedger = mutable.LinkedHashMap[String, Seq[CdeCmd]]()
    ledger.reverse.map { case cmd =>
      mapLedger(cmd.name) = cmd +:mapLedger.getOrElse(cmd.name, Seq.empty)
    }
    Cde.make(Seq(mapLedger))

  extension [T: JValueEncoder : Tag](name: String)
    def := (v: T)(using CdeBuilder): Unit =
      OMField(name, v)

    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder): Unit =
      OMUpdate(name, fn)

sealed trait Cde:
  private[cde] def ledger: Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]]
  def +(mixin: Cde): Cde =
    Cde.make(ledger ++ mixin.ledger)

object Cde:
  opaque type Context = Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]]
  object Context:
    extension (ctx: Context)
      def ledger: Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]] = ctx

  private[cde] def make(l: Seq[collection.SeqMap[String, collection.Seq[CdeCmd]]]): Cde =
    new Cde:
      val ledger = l

  def elaborate[T: CdeElaborator](node: Cde): Either[Seq[String], T] =
    summon[CdeElaborator[T]].elaborate(node.ledger)

  def apply(fn: CdeBuilder ?=> Unit): Cde =
    val builder = new CdeBuilder {}
    fn(using builder)
    builder.toNode

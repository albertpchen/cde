package cde

import scala.collection.mutable

sealed trait OMContext:

  private val ledger: mutable.ArrayBuffer[OMCmd] = new mutable.ArrayBuffer[OMCmd]()

  private[cde] def addCmd(cmd: OMCmd): Unit =
    ledger += cmd

  private[cde] def toNode[T]: Cde =
    val mapLedger = mutable.LinkedHashMap[String, Seq[OMCmd]]()
    ledger.reverse.map { case cmd =>
      mapLedger(cmd.name) = cmd +:mapLedger.getOrElse(cmd.name, Seq.empty)
    }
    Cde.make(Seq(mapLedger))

  extension [T: JValueEncoder : Tag](name: String)
    def := (v: T)(using OMContext): Unit =
      OMField(name, v)

    def :+= (fn: OMUpdateContext ?=> T)(using OMContext): Unit =
      OMUpdate(name, fn)

sealed trait Cde:
  private[cde] def ledger: Seq[collection.SeqMap[String, collection.Seq[OMCmd]]]
  def +(mixin: Cde): Cde =
    Cde.make(ledger ++ mixin.ledger)

object Cde:
  opaque type Context = Seq[collection.SeqMap[String, collection.Seq[OMCmd]]]
  object Context:
    extension (ctx: Context)
      def ledger: Seq[collection.SeqMap[String, collection.Seq[OMCmd]]] = ctx

  private[cde] def make(l: Seq[collection.SeqMap[String, collection.Seq[OMCmd]]]): Cde =
    new Cde:
      val ledger = l

  def elaborate[T: CdeElaborator](node: Cde): T =
    summon[CdeElaborator[T]].elaborate(node.ledger)

  def apply(fn: OMContext ?=> Unit): Cde =
    val ctx = new OMContext {}
    fn(using ctx)
    ctx.toNode

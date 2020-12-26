package cde

sealed trait OMCmd:
  def name: String

sealed trait SetField extends OMCmd:
  type Value
  def tag: Tag[Value]
  def encoder: JValueEncoder[Value]
  def value: Value
  override def toString: String = s"SetField($name := $value)"

def OMField[V: JValueEncoder : Tag](n: String, v: V)(using ctx: OMContext) =
  ctx.addCmd(new SetField {
    type Value = V
    val name = n
    val value = v
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[Value]]
  })

sealed trait UpdateField extends OMCmd:
  type Value
  def tag: Tag[Value]
  def updateFn: OMUpdateContext ?=> Value
  override def toString: String = s"UpdateField($name)"

def OMUpdate[V : Tag](n: String, fn: OMUpdateContext ?=> V)(using ctx: OMContext) =
  ctx.addCmd(new UpdateField {
    type Value = V
    val name = n
    val updateFn = fn
    val tag = summon[Tag[V]]
  })

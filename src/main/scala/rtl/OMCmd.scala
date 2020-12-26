package cde

sealed trait CdeCmd:
  def name: String

sealed trait SetField extends CdeCmd:
  type Value
  def tag: Tag[Value]
  def encoder: JValueEncoder[Value]
  def value: Value
  override def toString: String = s"SetField($name := $value)"

def OMField[V: JValueEncoder : Tag](n: String, v: V)(using builder: CdeBuilder) =
  builder.addCmd(new SetField {
    type Value = V
    val name = n
    val value = v
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[Value]]
  })

sealed trait UpdateField extends CdeCmd:
  type Value
  def tag: Tag[Value]
  def updateFn: CdeUpdateContext ?=> Value
  override def toString: String = s"UpdateField($name)"

def OMUpdate[V : Tag](n: String, fn: CdeUpdateContext ?=> V)(using builder: CdeBuilder) =
  builder.addCmd(new UpdateField {
    type Value = V
    val name = n
    val updateFn = fn
    val tag = summon[Tag[V]]
  })

package cde

import cde.json.JValueEncoder

sealed trait CdeCmd:
  def name: String
  def source: CdeSource

sealed trait SetField extends CdeCmd:
  type Value
  def tag: Tag[Value]
  def encoder: JValueEncoder[Value]
  def value: Value
  override def toString: String = s"SetField($name := $value)"

def OMField[V: JValueEncoder : Tag](n: String, v: V)(using builder: CdeBuilder, src: CdeSource) =
  builder.addCmd(new SetField {
    type Value = V
    val name = n
    val source = src
    val value = v
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[Value]]
  })

sealed trait UpdateField extends CdeCmd:
  type Value
  def tag: Tag[Value]
  def encoder: JValueEncoder[Value]
  def updateFn: CdeUpdateContext ?=> Value
  override def toString: String = s"UpdateField($name)"

def OMUpdate[V: JValueEncoder : Tag](n: String, fn: CdeUpdateContext ?=> V)(using builder: CdeBuilder, src: CdeSource) =
  builder.addCmd(new UpdateField {
    type Value = V
    val name = n
    val source = src
    val updateFn = fn
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[V]]
  })

package cde

import cde.json.JValueEncoder

/** A data representation of a Cde command
  */
sealed trait CdeCmd:
  /** The name of the field this command targets
    */
  def name: String

  /** The source locator of the site this command was issued at
    */
  def source: CdeSource

  /** The type of the value the field should map to
    */
  type Value

  /** Tag that identifies the value type
    */
  def tag: Tag[Value]


/** Represents a Cde field assignment operation
  */
sealed trait SetField extends CdeCmd:
  /** The encoder instance that will be used to encode this value into a
    * [[JValue]]
    */
  def encoder: JValueEncoder[Value]

  /** The value this field should be assigned to
    */
  def value: Value

  override def toString: String = s"SetField($name := $value)[${source.prettyPrint()}]"

/** Constructs and appends a [[SetField]] command to the enclosing [[CdeBuilder]]
  */
def OMField[V: JValueEncoder : Tag](n: String, v: V)(using builder: CdeBuilder, src: CdeSource) =
  builder.addCmd(new SetField {
    type Value = V
    val name = n
    val source = src
    val value = v
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[Value]]
  })


/** Represents a Cde field assignment operation that depends on recursive Cde
  * lookups
  */
sealed trait UpdateField extends CdeCmd:
  /** The encoder instance that will be used to encode this value into a
    * [[JValue]]
    */
  def encoder: JValueEncoder[Value]

  /** The update context function that will compute the value this field should
    * be assigned to
    */
  def updateFn: CdeUpdateContext ?=> Value

  override def toString: String = s"UpdateField($name)[${source.prettyPrint()}]"


/** Constructs and appends a [[SetField]] command to the enclosing [[CdeBuilder]]
  */
def OMUpdate[V: JValueEncoder : Tag](n: String, fn: CdeUpdateContext ?=> V)(using builder: CdeBuilder, src: CdeSource) =
  builder.addCmd(new UpdateField {
    type Value = V
    val name = n
    val source = src
    val updateFn = fn
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[V]]
  })

package cde

import cde.json.JValueEncoder

/** A data representation of a Cde command
  */
sealed trait CdeCmd derives CanEqual:
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

object CdeCmd:
  /** Represents a Cde field assignment operation
    */
  sealed trait Bind extends CdeCmd:
    /** The encoder instance that will be used to encode this value into a
      * [[JValue]]
      */
    def encoder: JValueEncoder[Value]

    /** The value this field should be assigned to
      */
    def value: Value

    override def toString: String = s"bind($name, $value)@${source.prettyPrint()}"


  /** Represents a Cde field assignment operation that depends on recursive Cde
    * lookups
    */
  sealed trait Update extends CdeCmd:
    /** The encoder instance that will be used to encode this value into a
      * [[JValue]]
      */
    def encoder: JValueEncoder[Value]

    /** The update context function that will compute the value this field should
      * be assigned to
      */
    def updateFn: CdeUpdateContext ?=> Value

    override def toString: String = s"update($name)@${source.prettyPrint()}"


/** Constructs and appends a [[Bind]] command to the enclosing [[CdeBuilder]]
  */
def bind[V: JValueEncoder : Tag](n: String, v: V)(using builder: CdeBuilder, src: CdeSource) =
  builder.addCmd(new CdeCmd.Bind {
    type Value = V
    val name = n
    val source = src
    val value = v
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[Value]]
  })


/** Constructs and appends a [[Update]] command to the enclosing [[CdeBuilder]]
  */
def update[V: JValueEncoder : Tag](n: String, fn: CdeUpdateContext ?=> V)(using builder: CdeBuilder, src: CdeSource) =
  builder.addCmd(new CdeCmd.Update {
    type Value = V
    val name = n
    val source = src
    val updateFn = fn
    val encoder = summon[JValueEncoder[Value]]
    val tag = summon[Tag[V]]
  })

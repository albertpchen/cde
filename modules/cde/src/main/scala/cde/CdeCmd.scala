package cde

enum CdeVisibility:
  case Hidden
  case Visible

  def isHidden: Boolean = this match
    case Hidden => true
    case _ => false

  def isVisible: Boolean = this match
    case Visible => true
    case _ => false

/** A data representation of a Cde command
  */
sealed trait CdeCmd derives CanEqual:
  /** The source locator of the site this command was issued at
    */
  def source: CdeSource

  /** The name of the field this command targets
    */
  def name: String

  def visibility: CdeVisibility


object CdeCmd:
  /** Represents a Cde field assignment operation
    */
  sealed trait Bind extends CdeCmd:
    /** The type of the value the field should map to
      */
    type Value

    /** The value this field should be assigned to
      */
    def value: Value

    /** Tag that identifies the value type
      */
    def tag: Tag[Value]

    override def toString: String = s"bind($name, $value)@${source.prettyPrint()}"

  private[cde] def bind[V](
    n: String,
    v: V,
    vis: CdeVisibility,
    src: CdeSource,
    t: Tag[V],
  )(using builder: CdeBuilder) =
    builder.addCmd(new Bind {
      type Value = V
      val name = n
      val source = src
      val value = v
      val visibility = vis
      val tag = t
    })

  /** Represents a Cde field assignment operation that depends on recursive Cde
    * lookups
    */
  sealed trait Update extends CdeCmd:
    /** The name of the field this command targets
      */
    def name: String

    /** The type of the value the field should map to
      */
    type Value

    /** Tag that identifies the value type
      */
    def tag: Tag[Value]

    /** The update context function that will compute the value this field should
      * be assigned to
      */
    def updateFn: CdeUpdateContext ?=> Value

    override def toString: String = s"update($name)@${source.prettyPrint()}"

  private[cde] def update[V](
    n: String,
    fn: CdeUpdateContext ?=> V,
    vis: CdeVisibility,
    src: CdeSource,
    t: Tag[V],
  )(using builder: CdeBuilder) =
    builder.addCmd(new Update {
      type Value = V
      val name = n
      val source = src
      val updateFn = fn
      val visibility = vis
      val tag = t
    })


/** Constructs and appends a [[Bind]] command to the enclosing [[CdeBuilder]]
  */
def bind[Value: Tag](name: String, value: Value)(using builder: CdeBuilder, src: CdeSource) =
  CdeCmd.bind[Value](
    name,
    value,
    CdeVisibility.Visible,
    src,
    summon[Tag[Value]],
  )

/** Constructs and appends a [[Bind]] command to the enclosing [[CdeBuilder]]
  */
def bindHidden[Value: Tag](name: String, value: Value)(using builder: CdeBuilder, src: CdeSource) =
  CdeCmd.bind[Value](
    name,
    value,
    CdeVisibility.Hidden,
    src,
    summon[Tag[Value]],
  )


/** Constructs and appends a [[Update]] command to the enclosing [[CdeBuilder]]
  */
def update[Value: Tag](name: String, updateFn: CdeUpdateContext ?=> Value)(using builder: CdeBuilder, src: CdeSource) =
  CdeCmd.update[Value](
    name,
    updateFn,
    CdeVisibility.Visible,
    src,
    summon[Tag[Value]],
  )

/** Constructs and appends a [[Update]] command to the enclosing [[CdeBuilder]]
  */
def updateHidden[Value : Tag](name: String, updateFn: CdeUpdateContext ?=> Value)(using builder: CdeBuilder, src: CdeSource) =
  CdeCmd.update[Value](
    name,
    updateFn,
    CdeVisibility.Hidden,
    src,
    summon[Tag[Value]],
  )

def self(using builder: CdeBuilder, src: CdeSource): CdeHandle =
  builder.getHandle(src)

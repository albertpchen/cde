package cde

/** Extension methods for arguably nicer syntax for building [[Cde]]s
  */
object syntax:
  extension [T: Tag](name: String)(using CdeBuilder, CdeSource)
    /** Operator for bind
      */
    def := (v: T): Unit = bind(name, v)

    /** Operator for update
      */
    def :+= (fn: CdeUpdateContext ?=> T): Unit = update(name, fn)

  extension [T : Tag](name: String)(using CdeBuilder, CdeSource)
    /** Operator for bindHidden
      */
    def ::= (v: T): Unit = bindHidden(name, v)

    /** Operator for updateHidden
      */
    def ::+= (fn: CdeUpdateContext ?=> T): Unit = updateHidden(name, fn)

  extension (cde: Cde)
    /** Operator for [[cde.extend]]
      */
    def + (overrides: Cde): Cde = cde.mixin(overrides)

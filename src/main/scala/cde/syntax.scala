package cde

import cde.json.JValueEncoder

/** Extension methods for arguably nicer syntax for building [[Cde]]s
  */
object syntax:
  extension [T: JValueEncoder : Tag](name: String)
    /** Operator for bind
      */
    def := (v: T)(using CdeBuilder, CdeSource): Unit = bind(name, v)

    /** Operator for update
      */
    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder, CdeSource): Unit = update(name, fn)

  extension (cde: Cde)
    /** Operator for [[cde.extend]]
      */
    def + (mixin: Cde): Cde = cde.extend(mixin)

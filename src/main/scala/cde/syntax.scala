package cde

import cde.json.JValueEncoder

/** Extension methods for arguably nicer syntax for building [[Cde]]s
  */
object syntax:
  extension [T: JValueEncoder : Tag](name: String)
    /** Operator for CdeSet
      */
    def := (v: T)(using CdeBuilder, CdeSource): Unit = CdeSet(name, v)

    /** Operator for CdeUpdate
      */
    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder, CdeSource): Unit = CdeUpdate(name, fn)

  extension (cde: Cde)
    /** Operator for [[cde.extend]]
      */
    def +(mixin: Cde): Cde = cde.extend(mixin)

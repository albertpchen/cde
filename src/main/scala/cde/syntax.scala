package cde

import cde.json.JValueEncoder

/** Extension methods for arguably nicer syntax for building [[Cde]]s
  */
object syntax:
  extension [T: JValueEncoder : Tag](name: String)
    def := (v: T)(using CdeBuilder, CdeSource): Unit = CdeSet(name, v)

    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder, CdeSource): Unit = CdeUpdate(name, fn)

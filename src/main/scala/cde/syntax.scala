package cde

import cde.json.JValueEncoder

/** Extension methods for arguably nicer syntax for building [[Cde]]s
  */
object syntax:
  extension [T: JValueEncoder : Tag](name: String)
    def := (v: T)(using CdeBuilder, CdeSource): Unit = OMField(name, v)

    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder, CdeSource): Unit = OMUpdate(name, fn)

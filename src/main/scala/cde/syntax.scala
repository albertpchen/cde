package cde

import cde.json.JValueEncoder

object syntax:
  extension [T: JValueEncoder : Tag](name: String)
    def := (v: T)(using CdeBuilder, CdeSource): Unit =
      OMField(name, v)

    def :+= (fn: CdeUpdateContext ?=> T)(using CdeBuilder, CdeSource): Unit =
      OMUpdate(name, fn)

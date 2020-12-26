import cde._

@main def Main(args: String*): Unit = {
  val om = Cde {
    addFields()
  } + Cde {
    "adf" := false
    "l" := Seq[Boolean](false)
  } + Cde {
    "foo" :+= (Site.adf[Boolean].toString + Up.foo[String] + Up.l[Seq[Boolean]].mkString(",") + "DLFKJ")
  }
  println(Cde.elaborate[JValue.JObject](om))
}

def addFields()(using OMContext): Unit =
  "foo" := "SLDFKJ"
  "adf" := 123

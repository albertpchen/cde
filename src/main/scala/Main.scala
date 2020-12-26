import cde._

@main def Main(args: String*): Unit = {
  val om = Cde {
    addFields()
  } + Cde {
    "adf" := false
    "l" := Seq[Boolean](false)
  } + Cde {
    "foo" :+= {
s"""
asdf_site: ${Site.adf[Boolean].toString}
foo_up: ${Up.foo[String]}
l_up: ${Up.l[Seq[Boolean]]}
"""
    }
  }
  println(Cde.elaborate[JValue.JObject](om))
}

def addFields()(using OMContext): Unit =
  "foo" := "SLDFKJ"
  "adf" := 123

import cde.{Cde, CdeBuilder, Site, Up}
import cde.json.JValue

@main def Main(args: String*): Unit = {
  val om = Cde {
    addFields()
  } + Cde {
    "adf" := false
    "l" := Seq[Boolean](false)
    "obj" := Cde {
      "a" := 0
      "b" := 1
    }
  } + Cde {
    "adf" := true
    "foo" :+= s"""
      |  asdf_site: ${Site.adf[Boolean]}
      |  asdf_up: ${Up.adf[Boolean]}
      |  foo_up: ${Up.foo[String]}
      |  l_up: ${Up.l[Seq[Boolean]]}
      |""".stripMargin
    "l" :+= Up[Seq[Boolean]] :+ true
    "obj" :+= (Up[Cde] + Cde {
      "c" := 2
      "d" := 3
    })
  }
  println(Cde.elaborate[JValue.JObject](om))
}

def addFields()(using CdeBuilder): Unit =
  "foo" := "SLDFKJ"
  "adf" := 123

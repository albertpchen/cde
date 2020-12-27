import cde.{Cde, CdeBuilder, Site, Up}
import cde.json.JValue

@main def Main(args: String*): Unit = {
  val om = Cde {
    //"ccc" :+= Up.ccc[String]
    addFields()
  } + Cde {
    //"lll" :+= Up.ccc[String]
    "adf" := false
    "l" := Seq[Boolean](false)
    "bbb" :+= Up.foo[String]
    "obj" := Cde {
      "a" := 0
      "b" := 1
    }
  } + Cde {
    "adf" := true
    "foo" :+= s"""
      |  asdf_site: ${Site.adf[Boolean]}
      |  asdf_up: ${Up.adf[Int]}
      |  foo_up: ${Up.foo[String]}
      |  l_up: ${Up.l[Seq[Boolean]]}
      |""".stripMargin
    "l" :+= Up[Seq[Int]].head
    "obj" :+= (Up[Cde] + Cde {
      "c" := 2
      "d" := 3
    })
  }
  println(Cde.elaborate[JValue.JObject](om).fold(_.mkString("\n\n"), _.toString))
}

def addFields()(using CdeBuilder): Unit =
  "foo" := "SLDFKJ"
  "adf" := 123

package cde

import cde.json.JValue
import cde.json.JValue._
import cde.syntax._

class CdeTests extends munit.FunSuite {
  test("setField") {
    val cde = Cde { "a" := "b" }
    assert(Cde.elaborate[JObject](cde) == Right(JObject(Seq("a" -> JString("b")))))
  }

  test("updateField Up") {
    val cde = Cde {
      "a" := "b"
    } + Cde {
      "a" :+= (Up.a[String] + "b")
    }
    assert(Cde.elaborate[JObject](cde) == Right(JObject(Seq("a" -> JString("bb")))))
  }

  test("updateField Site") {
    val cde = Cde {
      "a" :+= (Site.b.get[String] + "c")
    } + Cde {
      "b" := "c"
    }
    assert(Cde.elaborate[JObject](cde) == Right(JObject(Seq(
      "a" -> JString("cc"),
      "b" -> JString("c"),
    ))))
  }

  test("updateField with self") {
    val cde = Cde {
      val local = self
      "a" :+= (self.b[String] + "c")
    } + Cde {
      "b" := "c"
    }
    val elaborated = Cde.elaborate[JObject](cde)
    elaborated.fold(
      e => fail(e.toString),
      obj => assert(obj == JObject(Seq(
        "a" -> JString("cc"),
        "b" -> JString("c"),
      )), obj.toString)
    )
  }

  test("hidden fields") {
    val cde = Cde {
      "a" ::= "b"
    } + Cde {
      "b" ::+= Up.a[String] + "b"
    } + Cde {
      "c" ::+= Site.b.get[String] + "c"
    }
    assert(Cde.elaborate[JObject](cde) == Right(JObject(Seq())))
  }
/*
  test("duplicate fields") {
    val cde = Cde {
      "a" :+= (Site.b[String] + "c")
      "a" := "c"
    }
    val result = Cde.elaborate[JObject](cde)
    result match
      case Left(errors) =>
        errors.foreach { e =>
          assert(e.message.startsWith("""duplicate field "a" set at:"""))
        }
      case Right(obj) => fail(s"elaboration should not have been successful:\n${obj.prettyPrint()}")
  }*/

  test("missing field") {
    val cde = Cde {
      "a" :+= Site.b.get[String] + "c"
    }
    val result = Cde.elaborate[JObject](cde)
    result match
      case Left(errors) =>
        errors.foreach { e =>
          assert(e.message == """no field named "b" defined""", e.message)
        }
      case Right(obj) => fail(s"elaboration should not have been successful:\n${obj.prettyPrint()}")
  }

  test("Up with no super") {
    val cde = Cde {
      "a" :+= Up.a[String] + "c"
    }
    val result = Cde.elaborate[JObject](cde)
    result match
      case Left(errors) =>
        errors.foreach { e =>
          assert(e.message == """no super value for field "a"""", e.message)
        }
      case Right(obj) => fail(s"elaboration should not have been successful:\n${obj.prettyPrint()}")
  }

  test("undefined Up") {
    val cde = Cde {
    } + Cde {
      "a" :+= Up.a[String] + "c"
    }
    val result = Cde.elaborate[JObject](cde)
    result match
      case Left(errors) =>
        errors.foreach { e =>
          assert(e.message.contains("""no super value for field "a""""), e.message)
        }
      case Right(obj) => fail(s"elaboration should not have been successful:\n${obj.prettyPrint()}")
  }

  test("integration") {
    def addFields()(using CdeBuilder): Unit =
      "foo" := "SLDFKJ"
      "adf" := 123

    val cde = Cde {
      //"ccc" :+= Up.ccc[String]
      addFields()
    } + Cde {
      //"lll" :+= Up.ccc[String]
      "adf" := false
      "l" := List[Boolean](false)
      "bbb" :+= Up.foo[String]
      "obj" := Cde {
        "a" := 0
        "b" := 1
      }
    } + Cde {
      "adf" := true
      "foo" :+= s"""
        |  adf_site: ${Site.adf.get[Boolean]}
        |  adf_up: ${Up.adf[Boolean]}
        |  foo_up: ${Up.foo[String]}
        |  l_up: ${Up.l[List[Boolean]]}
        |""".stripMargin
      "l" :+= Up[List[Boolean]]().head
      "obj" :+= (Up[Cde]() + Cde {
        "c" := 2
        "d" := 3
      })
      "sdf" :+= Site.obj.c.get[Int]
    }
    val result = Cde.elaborate[JObject](cde)
    result match
      case Right(obj) =>
        obj.value.zip(Seq(
          "foo" -> JString(s"""
            |  adf_site: true
            |  adf_up: false
            |  foo_up: SLDFKJ
            |  l_up: List(false)
            |""".stripMargin),
          "adf" -> JBoolean(true),
          "l" -> JBoolean(false),
          "bbb" -> JString("SLDFKJ"),
          "obj" -> JObject(Seq(
            "a" -> JInteger(0),
            "b" -> JInteger(1),
            "c" -> JInteger(2),
            "d" -> JInteger(3),
          )),
          "sdf" -> JInteger(2),
        )).foreach { case (a, b) =>
          assert(a == b, s"expected $b, got $a")
        }
      case Left(e) => fail(s"elaboration failed:\n${e.mkString("\n")}")
  }

  test("example") {
    enum Location:
      case Center
      case BottomRight
      case BottomLeft
      case TopRight
      case TopLeft

    val base = Cde {
      import Location._
      "width" ::= 10
      "height" ::= 20
      "top_left" ::+= {
        // fails when using (Int, Int): "no implicit argument of type
        // cde.CdeBuilder was found for parameter x$2 of method ::+= in object
        // syntax"
        val (x: Int, y: Int) = Site.origin_x_y.get[(Int, Int)]
        val height = Site.height.get[Int]
        val width = Site.width.get[Int]
        Site.origin_location.get[Location] match
          case Center => (x + width / 2, y + height / 2)
          case BottomRight => (x - width, y + height)
          case BottomLeft => (x, y + height)
          case TopRight => (x - width, y)
          case TopLeft => (x, y)
      }
      "top" :+= Site.top_left.get[(Int, Int)]._2
      "left" :+= Site.top_left.get[(Int, Int)]._1
    }

    def checkTopLeft(x: Int, y: Int, cde: Cde)(using munit.Location): Unit =
      val result = Cde.elaborate[JObject](cde)
      result match
        case Right(obj) =>
          obj.value.zip(Seq(
            "top" -> JInteger(y),
            "left" -> JInteger(x),
          )).foreach { case (a, b) =>
            assert(a == b, s"expected $b, got $a")
          }
        case Left(e) => fail(s"elaboration failed:\n${e.mkString("\n")}")

    checkTopLeft(5, 10, base + Cde {
      "origin_x_y" ::= (0, 0)
      "origin_location" ::= Location.Center
    })

    checkTopLeft(6, 11, base + Cde {
      "origin_x_y" ::= (1, 1)
      "origin_location" ::= Location.Center
    })

    checkTopLeft(-10, 20, base + Cde {
      "origin_x_y" ::= (0, 0)
      "origin_location" ::= Location.BottomRight
    })

    checkTopLeft(-100, 200, base + Cde {
      "width" ::= 100
      "height" ::= 200
      "origin_x_y" ::= (0, 0)
      "origin_location" ::= Location.BottomRight
    })

    checkTopLeft(-100, 200, base + Cde {
      "origin_x_y" ::= (1, 1)
      "origin_location" ::= Location.BottomRight
    } + Cde {
      "width" ::= 100
      "height" ::= 200
    } + Cde {
      "origin_x_y" ::= (0, 0)
    })

    def translate(dx: Int, dy: Int)(cde: Cde): Cde =
      cde + Cde {
        "origin_x_y" ::+= {
          val (x: Int, y: Int) = Up.origin_x_y[Tuple2[Int, Int]]
          (x + dx, y + dy)
        }
      }

    checkTopLeft(0, 15, translate(-5, 5)(base + Cde {
      "origin_x_y" ::= (0, 0)
      "origin_location" ::= Location.Center
    }))
  }
}

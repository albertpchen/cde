package cde

import munit.{FunSuite, Location}

import cde.json.JValue
import cde.json.JValue._
import cde.syntax._

class CdeTests extends FunSuite {
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
      "a" :+= (Site.b[String] + "c")
    } + Cde {
      "b" := "c"
    }
    assert(Cde.elaborate[JObject](cde) == Right(JObject(Seq(
      "a" -> JString("cc"),
      "b" -> JString("c"),
    ))))
  }

  test("hidden fields") {
    val cde = Cde {
      "a" ::= "b"
    } + Cde {
      "b" ::+= Up.a[String] + "b"
    } + Cde {
      "c" ::+= Site.b[String] + "c"
    }
    assert(Cde.elaborate[JObject](cde) == Right(JObject(Seq())))
  }

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
  }

  test("missing field") {
    val cde = Cde {
      "a" :+= Site.b[String] + "c"
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
          assert(e.message == """no super value for field "a", no super Cde""", e.message)
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
        |  adf_site: ${Site.adf[Boolean]}
        |  adf_up: ${Up.adf[Boolean]}
        |  foo_up: ${Up.foo[String]}
        |  l_up: ${Up.l[List[Boolean]]}
        |""".stripMargin
      "l" :+= Up[List[Boolean]].head
      "obj" :+= (Up[Cde] + Cde {
        "c" := 2
        "d" := 3
      })
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
          ))
        )).foreach { case (a, b) =>
          assert(a == b, s"expected $b, got $a")
        }
      case Left(e) => fail(s"elaboration failed:\n${e.mkString("\n")}")
  }
}

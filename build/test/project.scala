//> using dep "com.disneystreaming::weaver-cats:0.8.4"
//> using testFramework "weaver.framework.CatsEffect"

package cde

import weaver._
import cats.effect._
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}

object Tests extends weaver.FunSuite {
  test("asserts") {
    val cde = Cde { "a" := "b" }
    expect.eql(cde.a.![String], "b")
  }

  test("update super") {
    val cde = Cde {
      "a" := "b"
    } {
      "a" := (Super.![String] + "b")
    }
    expect.eql(cde.a.![String], "bb")
  }

  test("update self") {
    val cde = Cde {
      "a" := Self.b.![String] + "c"
    } {
      "b" := "c"
    }
    expect.eql(cde.a.![String], "cc")
    expect.eql(cde.b.![String], "c")
  }

  inline def expectError(inline thunk: Any)(inline fn: Throwable => Expectations) =
    try
      val result = thunk
      failure(s"expected exception, got result $result")
    catch
      fn

  val StringTag: Tag[String] = summon

  test("missing field") {
    val TestCde = Cde {
      "a" := Self.b.![String] + "c"
    }
    expectError(TestCde.a.![String]) {
      case Cde.LookupError(TestCde, "b", StringTag) => success
      case e => failure(s"unexpected exception $e")
    }
  }

  test("missing super") {
    val TestCde = Cde {
      "a" := Super.a.![String] + "c"
    }
    expectError(TestCde.a.![String]) {
      case Cde.SuperLookupError(Cde.Ctx(TestCde, "a", _)) => success
      case e => failure(s"unexpected exception $e")
    }
  }

  test("missing field in super") {
    val SuperCde = Cde { }
    val TestCde = SuperCde {
      "a" := Super.a.![String] + "c"
    }
    expectError(TestCde.a.![String]) {
      case Cde.LookupError(SuperCde, "a", StringTag) => success
      case e => failure(s"unexpected exception $e")
    }
  }

  test("integration") {
    def addFields()(using Cde.Builder): Unit =
      "foo" := "SLDFKJ"
      "adf" := 123

    val cde = Cde {
      //"ccc" :+= Up.ccc[String]
      addFields()
    } {
      //"lll" :+= Up.ccc[String]
      "adf" := false
      "l" := List[Boolean](false)
      "bbb" := Super.foo.![String]
      "obj" := Cde {
        "a" := 0
        "b" := 1
      }
      "hidden_default" ::= Self.hidden.![Any]
    } {
      "adf" := true
      "foo" := s"""adf_site: ${Self.adf.![Boolean]}
                  |adf_up: ${Super.adf.![Boolean]}
                  |foo_up: ${Super.foo.![String]}
                  |l_up: ${Super.l.![List[Boolean]]}
                  |""".stripMargin
      "l" := Super.l.![List[Boolean]].head
      "obj" := Super.obj.![Cde].apply {
        "c" := 2
        "d" := 3
      }
      "sdf" := Self.obj.c.![Int]

      "hidden" ::= Self.hidden.![String]
      "hidden_default" := Self.hidden.![String]
    }
    println(writeToString(cde, WriterConfig.withIndentionStep(2)))
    expect.eql(
      cde.foo.![String],
      s"""adf_site: true
         |adf_up: false
         |foo_up: SLDFKJ
         |l_up: List(false)
         |""".stripMargin
    ) &&
      expect.eql(cde.adf.![Boolean], true) &&
      expect.eql(cde.l.![Boolean], false) &&
      expect.eql(cde.bbb.![String], "SLDFKJ")
      expect.eql(cde.obj.a.![Int], 0) &&
      expect.eql(cde.obj.b.![Int], 1) &&
      expect.eql(cde.obj.c.![Int], 2) &&
      expect.eql(cde.obj.d.![Int], 3) &&
      expect.eql(cde.sdf.![Int], 2)
  }

  enum Location:
    case Center
    case BottomRight
    case BottomLeft
    case TopRight
    case TopLeft

  test("example") {
    val base = Cde {
      import Location._
      "width" := 10
      "height" := 20
      "top_left" := {
        val (x, y) = Self.origin_x_y.![(Int, Int)]
        val height = Self.height.![Int]
        val width = Self.width.![Int]
        Self.origin_location.![Location] match
          case Center => (x + width / 2, y + height / 2)
          case BottomRight => (x - width, y + height)
          case BottomLeft => (x, y + height)
          case TopRight => (x - width, y)
          case TopLeft => (x, y)
      }
      "top" := Self.top_left.![(Int, Int)]._2
      "left" := Self.top_left.![(Int, Int)]._1
    }

    inline def checkTopLeft(x: Int, y: Int, cde: Cde): Expectations =
      expect.eql(cde.top.![Int], y) && expect.eql(cde.left.![Int], x)

    checkTopLeft(5, 10, base {
      "origin_x_y" := (0, 0)
      "origin_location" := Location.Center
    })

    checkTopLeft(6, 11, base {
      "origin_x_y" := (1, 1)
      "origin_location" := Location.Center
    })

    checkTopLeft(-10, 20, base {
      "origin_x_y" := (0, 0)
      "origin_location" := Location.BottomRight
    })

    checkTopLeft(-100, 200, base {
      "width" := 100
      "height" := 200
      "origin_x_y" := (0, 0)
      "origin_location" := Location.BottomRight
    })

    checkTopLeft(-100, 200, base {
      "origin_x_y" := (1, 1)
      "origin_location" := Location.BottomRight
    } {
      "width" := 100
      "height" := 200
    } {
      "origin_x_y" := (0, 0)
    })

    def translate(dx: Int, dy: Int)(cde: Cde): Cde =
      cde {
        "origin_x_y" := {
          val (x: Int, y: Int) = Super.origin_x_y.![Tuple2[Int, Int]]
          (x + dx, y + dy)
        }
      }

    checkTopLeft(0, 15, translate(-5, 5)(base {
      "origin_x_y" := (0, 0)
      "origin_location" := Location.Center
    }))
  }
}

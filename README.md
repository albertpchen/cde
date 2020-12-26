## sbt project cross-compiled with Dotty and Scala 2

### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL. For more information on
cross-compilation in sbt, see <https://www.scala-sbt.org/1.x/docs/Cross-Build.html>.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).


```scala
enum Tree[T]:
  case Leaf(value: T)
  case Branch(value: Option[T], children: Seq[Tree[T]])
  def value: Option[T] =
    this match
      case Leaf(value) => Some(value)
      case Branch(value, _) => value

val a = OM {
  "a" := 0
  "b" := "foo"
  "c" := Tree.Leaf(false)
}

val b = a + {
  "c" :+= (Branch(om[Tree].value, Seq(om[Tree])))
}

a.elaborate[JValue]()
"""
{
  "a": 0,
  "b": "foo",
  "c": {
    value: false
  }
}
"""

b.elaborate[JValue]()
"""
{
  "a": 0,
  "b": "foo",
  "c": {
    value: false,
    children: [
      {
        value: false
      }
    ]
  }
}
"""
```

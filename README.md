# Context-Dependent Environments

This is a Scala 3 configuration library similar to
[Config](https://github.com/chipsalliance/api-config-chipsalliance) used by
[rocket-chip](https://github.com/chipsalliance/rocket-chip). This is also very
similar to [Jsonnet](https://jsonnet.org/).

## Overview
The user API of this library revolves around `Cde` objects. These objects may
contain mappings of string field names to arbitrary values.
```scala
import cde._
val boxConfig = Cde {
  bind("width", 100)
  bind("height", 200)
}
```

They may also contain logic to compute field values by performing recursive
lookups of other field values.
```scala
import cde._
val boxConfig = Cde {
  bind("width", 100)
  bind("height", 200)
  update("area", Site.width[Int] * Site.height[Int])
}
```

`Cde` objects may also (mix-in)[http://www.bracha.org/oopsla90.pdf] other `Cde`
objects to override values.
```scala
import cde._
val baseBoxConfig = Cde {
  bind("width", 100)
  bind("height", 200)
  update("area", Site.width[Int] * Site.height[Int])
}

val boxConfig = baseBoxConfig.mixin(Cde {
  bind("width", 300)
  update("height", Up.height[Int] * 2)
})
```

## Syntax
This library provides a small DSL for contructing `Cde`s that uses the [builder
pattern](https://dotty.epfl.ch/docs/reference/contextual/context-functions.html#example-builder-pattern).
Operator extension methods can be made available by importing `cde.syntax._`.
`Cde`s are constructed using the `Cde { ... }` method which builds a `Cde`
according to the builder methods executed within the `{ ... }` block. These
builder methods require a `given` instance of `CdeBuilder` (provided by the
`Cde.apply` method). These are the builder methods (and their operator syntax):
- `bind` (`:=`): binds a static value to a field
- `bindHidden` (`::=`): like `bind` but also makes it so that the field will
  not appear in the elaborated JSON
- `update` (`:+=`): updates a field with a value that may be computed from
  recursive field lookups
- `updateHidden` (`::+=`): like `update` but also makes it so that the field
  will not appear in the elaborated JSON

`Cde` objects may be mixed-in using the `mixin` method or the `+` operator.
This will return a new `Cde` object with the field values of the RHS overriding
the those in the LHS.

The `update` and `updateHidden` methods can accept values computed from field
lookups. These lookups can be performed using the `Up` and `Site` objects.
These are named after the `up` and `site` variable names used by rocket-chip
for `Config` views, because they provide the same functionality. The methods in
the `Up` and `Site` objects require a `given` instance of `CdeUpdateContext`
(provided by `update` and `updateHidden`).
- `Up`: Looks up a field value in the parent of the current `Cde` object.
  Lookups can be performed calling the `apply` method with the field name and
  its expected value type e.g. `Up[Int]("width")` or by using method syntax
  `Up.width[Int]`. If no name is provided to the `apply` method e.g.
  `Up[Int]()`, the current field name of the enclosing `update`/`updateHidden`
  is used.
- `Site`: Looks up a field in the top-level `Cde`. i.e. the field lookup is
  performed from the view of the final `Cde` after all other `Cde`s have been
  mixed-in. Lookups can be performed calling the `apply` method with the field
  name and its expected value type e.g. `Site[Int]("width")` or by using method
  syntax `Site.width[Int]`.  If no name is provided to the `apply` method e.g.
  `Site[Int]()`, the current field name of the enclosing
  `update`/`updateHidden` is used.

## Elaboration
`Cde` objects aren't very useful on their own. They need to be elaborated to be
converted into useful formats like JSON objects. Elaboration is done using the
`Cde.elaborate` method. This method requires a given instance of the
`CdeElaborator` type class. This library includes a simple JSON AST with an
associated `CdeElaborator` to produce JSON from `Cde`s. The `elaborate` method
returns type `Either[Seq[CdeError], T]`.
```scala
val box = Cde {
  bind("width", 100)
  bind("height", 200)
}
Cde.elaborate[JObject](box)
  .foreach(o => println(o.prettyPrint()))
// {
//   "width": 10,
//   "height": 20
// }
```

## Example
Here is a simple box configuration example that uses the features of this
library to create box configurations that dynamiclly updates box coordinates
when other fields are updated.
```scala
import cde._
import cde.syntax._ // operator extension methods
import cde.json.JValue.JObject

enum Location:
  case Center
  case BottomRight
  case BottomLeft
  case TopRight
  case TopLeft

val baseBoxConfig = Cde {
  import Location._
  "origin_x_y" ::= (0, 0)

  // lazily calculates the coordinates of the top-left corner based on the
  // values of other fields
  "top_left" ::+= {
    val (x: Int, y: Int) = Site.origin_x_y[Tuple2[Int, Int]]
    val height = Site.height[Int]
    val width = Site.width[Int]
    Site.origin_location[Location] match
      case Center => (x + width / 2, y + height / 2)
      case BottomRight => (x - width, y + height)
      case BottomLeft => (x, y + height)
      case TopRight => (x - width, y)
      case TopLeft => (x, y)
  }
  "top" :+= Site.top_left[Tuple2[Int, Int]]._2
  "left" :+= Site.top_left[Tuple2[Int, Int]]._1
}

val smallBoxConfig = Cde {
  "width" := 10
  "height" := 20
  "origin_location" ::= Location.Center
}

Cde.elaborate[JObject](smallBoxConfig)
  .foreach(o => println(o.prettyPrint()))
// {
//   "width": 10,
//   "height": 20,
//   "top": 10,
//   "left": 5
// }


val bottomLeftConfig = Cde {
  "origin_location" ::= Location.Center
}

Cde.elaborate[JObject](smallBoxConfig + bottomLeftConfig)
  .foreach(o => println(o.prettyPrint()))
// {
//   "width": 10,
//   "height": 20,
//   "top": 10,
//   "left": 5
// }


// creates a new box config with its origin translated
def translate(dx: Int, dy: Int)(cde: Cde): Cde =
  cde + Cde {
    "origin_x_y" ::+= {
      val (x: Int, y: Int) = Up.origin_x_y[Tuple2[Int, Int]]
      (x + dx, y + dy)
    }
  }

Cde.elaborate[JObject](translate(5, -5)(smallBoxConfig))
  .foreach(o => println(o.prettyPrint()))
// {
//   "width": 10,
//   "height": 20,
//   "top": 5,
//   "left": 0
// }
```

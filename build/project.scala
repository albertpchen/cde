//> using scala "3.5.0-RC4"
//> using dep "org.typelevel::cats-effect:3.5.4"
//> using dep "co.fs2::fs2-core:3.10.2"
//> using dep "com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-core:2.30.6"
//> using dep "com.github.plokhotnyuk.jsoniter-scala::jsoniter-scala-macros:2.30.6"

package cde

val Project = Cde {}

def BaseProject(name: String, hasTestProject: Boolean = true): Cde = Cde {
  "name" := name
  "sources" := List(s"modules/${Self.name.![String]}/src")
  "libraries" := List.empty[String]
  "dependencies" := List.empty[Cde]
  "scalaVersion" := "3.5.0"
  "jvm" := {
    Project + Self + Cde {
      "dependencies" := Self.dependencies.![List[String]]
      "platform" := "jvm"
    }
  }

  if hasTestProject then
    "test" := BaseProject(s"$name-test", hasTestProject = false) {
      val base = Self
      "dependencies" := List(base)
      "sources" := List(s"modules/${base.name.![String]}/test")
      "scalaVersion" := base.scalaVersion.![String]
    }
}

val build = Cde {
  "nix" := Cde {
    "revision" := "3e644bd6248"
    "sha256" := "1bkqdwcmap2km4dpib0pzgmj66w74xvr8mrvsshp7y569lj40qxi"
  }
}

package test

import cde._
import cde.syntax._

def project(version: String) = Cde {
  "version" := version
  "out" :+= s"${Site.workspaceDir[String]}/${Site.buildDir[String]}"
}

import coursier._

val resolution = Resolve()
  .addDependencies(
    Dependency(
      Module(Organization("org.typelevel"), ModuleName("cats-core_2.11")),
      "0.6.0"
    )
  )
  .run()

val scala2Version = "2.13.4"
val scala3Version = "3.0.0-M3"

val scala3CrossSettings = Seq(
  // To make the default compiler and REPL use Dotty
  scalaVersion := scala3Version,

  // To cross compile with Dotty and Scala 2
  crossScalaVersions := Seq(scala3Version, scala2Version)
)

lazy val cde = project
  .in(file("."))
  .settings(scala3CrossSettings)
  .settings(
    version := "0.1.0",
    useScala3doc := true,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.20" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

val scala3Version = "3.0.0-M3"

lazy val cde = project
  .in(file("."))
  .settings(
    version := "0.1.0",
    useScala3doc := true,
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.20" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

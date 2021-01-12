val scala3Version = "3.0.0-M3"

// settings common to JVM and Js builds
val cdeCommonSettings = Seq(
  version := "0.1.0",
  useScala3doc := true,
  scalaVersion := scala3Version,
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "0.7.20" % Test,
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  Compile / scalaSource := (ThisBuild / baseDirectory).value / "src"/"main"/"scala",
  Test / scalaSource := (ThisBuild / baseDirectory).value / "src"/"test"/"scala",
)

// project for JVM build (default)
lazy val cde = project
  .in(file("."))
  .settings(cdeCommonSettings)

// project for Js build
lazy val cdeJs = project
  .enablePlugins(ScalaJSPlugin)
  .in(file("target/cdeJs")) // sbt does not allow two projects to share the same directory so just use a dummy one here
  .settings(cdeCommonSettings)
  .settings(
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  )

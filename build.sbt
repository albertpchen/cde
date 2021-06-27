val scala3Version = "3.0.0"

// settings common to JVM and Js builds
val cdeCommonSettings = Seq(
  version := "0.1.0",
  scalaVersion := scala3Version,
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "0.7.26" % Test,
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  Compile / scalaSource := (ThisBuild / baseDirectory).value/"modules"/"cde"/"src"/"main"/"scala",
  Test / scalaSource := (ThisBuild / baseDirectory).value/"modules"/"cde"/"src"/"test"/"scala",
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

lazy val test = project
  .in(file("modules/test"))
  .dependsOn(cde)
  .settings(
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      ("io.get-coursier" %% "coursier" % "2.0.16").cross(CrossVersion.for3Use2_13),
    ),
  )

lazy val docs = project
  .in(file("target/docs"))
  .enablePlugins(MdocPlugin)
  .dependsOn(cde)
  .settings(
    scalaVersion := scala3Version,
    mdocIn := file("docs/src"),
    mdocOut := file("docs"),
    mdocExtraArguments := Seq("--cwd", "docs"),
    mdocVariables := Map(
      // build dir for mdoc programs to dump temp files
      "BUILD_DIR" -> "target/docs"
    )
  )

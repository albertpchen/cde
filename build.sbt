val scala3Version = "3.0.0-M3"

onLoad in Global := (onLoad in Global).value andThen (Command.process("project cde", _))

lazy val cde = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    version := "0.1.0",
    useScala3doc := true,
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "0.7.20" % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework"),
  )
  .jsSettings(
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  )

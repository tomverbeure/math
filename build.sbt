val spinalVersion = "1.10.1"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.spinalhdl",
      scalaVersion := "2.12.18",
      version      := "1.0.0"
    )),
    libraryDependencies ++= Seq(
      spinalCore, spinalLib, spinalIdslPlugin,
      "org.scalatest" %% "scalatest" % "3.2.18" % "test"
    ),
    name := "Fpxx"
  ).dependsOn()

fork := true

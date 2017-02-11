scalaVersion := "2.12.1"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.mpilquist" %% "simulacrum" % "0.10.0",
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "com.lihaoyi" % "ammonite" % "0.8.2" % "test" cross CrossVersion.full
)

initialCommands in (Test, console) := """ammonite.Main().run()"""


import sbt.addCompilerPlugin

name := "exercises"

version := "0.1"

inThisBuild {
  List(
    scalaVersion := "2.13.12",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixScalaBinaryVersion := (ThisBuild / scalaBinaryVersion).value,
    scalafixDependencies ++= Seq(
      "com.github.vovapolu" %% "scaluzzi" % "0.1.23"
    )
  )
}

val libraries = Seq(
  "org.scalatest"     %% "scalatest"       % "3.2.3"         % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.3.0.0-SNAP3" % Test
)

// exercises
lazy val exercises01 = project in file("exercises01") settings (libraryDependencies ++= libraries)
lazy val exercises02 = project in file("exercises02") settings (libraryDependencies ++= libraries)
lazy val exercises03 = project in file("exercises03") settings (libraryDependencies ++= libraries)
lazy val exercises04 = project in file("exercises04") settings (libraryDependencies ++= libraries)
lazy val exercises05 = project in file("exercises05") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions")
)
lazy val exercises06 = project in file("exercises06") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  addCompilerPlugin(kindProjectorDep)
)

lazy val exercises07 = project in file("exercises07") settings (
  libraryDependencies ++= libraries,
  scalacOptions ++= Seq("-feature", "-language:implicitConversions"),
  addCompilerPlugin(kindProjectorDep)
)


// lectures
lazy val lecture02 = project in file("lecture02") settings (libraryDependencies ++= libraries)
lazy val lecture03 = project in file("lecture03") settings (libraryDependencies ++= libraries)
lazy val lecture04 = project in file("lecture04") settings (libraryDependencies ++= libraries)
lazy val lecture05 = project in file("lecture05") settings (libraryDependencies ++= libraries)
lazy val lecture06 = project in file("lecture06") settings (libraryDependencies ++= libraries) settings addCompilerPlugin(kindProjectorDep)
lazy val lecture07 = project in file("lecture07") settings (libraryDependencies ++= libraries) settings addCompilerPlugin(kindProjectorDep)

lazy val kindProjectorDep = "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full

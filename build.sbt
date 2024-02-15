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

lazy val exercises01 = project in file("exercises01") settings (libraryDependencies ++= libraries)

val kindProjectorDep = "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full

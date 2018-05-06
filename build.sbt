val circeVersion = "0.9.3"
lazy val root = (project in file("."))
  .settings(
    name := "rows",
    version := "0.1",
    scalaVersion := "2.12.6",
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.1.0"
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser",
      "io.circe" %% "circe-generic-extras"
    ) map (_ % circeVersion),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
  )

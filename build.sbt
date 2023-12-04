val scala3Version = "3.3.0"

inThisBuild(
  List(
    version           := "1.0.0",
    scalaVersion      := scala3Version,
    scalafmtOnCompile := true
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(shared, d1)
  .settings(
    name := "Advent of Code 2023"
  )

lazy val scaffolding = project
  .in(file("scaffolding"))
  .dependsOn(shared)
  .settings(
    name                                      := "Scaffolding",
    libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "3.1.0"
  )

lazy val shared = project
  .in(file("shared"))
  .settings(
    name := "Shared"
  )

lazy val d1 = project
  .in(file("d1"))
  .dependsOn(shared)
  .settings(
    name := "Day 1"
  )

lazy val d2 = project
  .in(file("d2"))
  .dependsOn(shared)
  .settings(
    name := "Day 2"
  )

lazy val d3 = project
  .in(file("d3"))
  .dependsOn(shared)
  .settings(
    name := "Day 3"
  )

lazy val d4 = project
  .in(file("d4"))
  .dependsOn(shared)
  .settings(
    name := "Day 4"
  )

addCommandAlias("cd", "project")
addCommandAlias("ls", "projects")
addCommandAlias("c", "compile")
addCommandAlias("rel", "reload")
addCommandAlias("r", "run")

// *****************************************************************************
// Projects
// *****************************************************************************

lazy val Version = new {
  val scala   = "3.0.0-M3"
  val zio     = "1.0.4-2"
  val parsers = "1.2.0-M1"
}

lazy val root =
  project
    .in(file("."))
    .settings(settings)
    .settings(
        libraryDependencies ++= Seq(
          library.zio
        , library.parsers
        , library.zioTest    % Test
        , library.zioTestSbt % Test
      )
      , publishArtifact := false
      , testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    )

// *****************************************************************************
// Library dependencies
// *****************************************************************************

lazy val library =
  new {
    val zio        = "dev.zio"                %% "zio"                      % Version.zio
    val zioTest    = "dev.zio"                %% "zio-test"                 % Version.zio
    val zioTestSbt = "dev.zio"                %% "zio-test-sbt"             % Version.zio
    val parsers    = "org.scala-lang.modules" %% "scala-parser-combinators" % Version.parsers
  }

// *****************************************************************************
// Settings
// *****************************************************************************

lazy val settings =
  commonSettings ++
    scalafmtSettings ++
    commandAliases

lazy val commonSettings =
  Seq(
      name := "advent2018"
    , scalaVersion := Version.scala
    , organization := "savinien"
    , scalacOptions ++= Seq(
        "-explain"
      , "-indent"
      , "-new-syntax"
    )
  )

lazy val scalafmtSettings =
  Seq(
    // scalafmtOnCompile := true
  )

lazy val commandAliases =
  addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt") ++
    addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

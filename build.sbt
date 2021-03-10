// *****************************************************************************
// Projects
// *****************************************************************************

lazy val Version = new {
  val scala     = "3.0.0-RC1"
  val zio       = "1.0.5"
  val scalatest = "3.2.6"
}

lazy val root =
  project
    .in(file("."))
    .settings(settings)
    .settings(
        libraryDependencies ++= Seq(
          library.zio
        , library.zioTest    % Test
        , library.zioTestSbt % Test
        , library.scalatest  % Test
      )
      , publishArtifact := false
      , testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
    )

// *****************************************************************************
// Library dependencies
// *****************************************************************************

lazy val library =
  new {
    val zio        = "dev.zio"       %% "zio"          % Version.zio
    val zioTest    = "dev.zio"       %% "zio-test"     % Version.zio
    val zioTestSbt = "dev.zio"       %% "zio-test-sbt" % Version.zio
    val scalatest  = "org.scalatest" %% "scalatest"    % Version.scalatest
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
        "-deprecation"
      , "-explain"
      , "-indent"
      , "-new-syntax"
      , "-Xfatal-warnings"
      // , "-Ycheck-init"
      , "-Yrequire-targetName"
      , "-Yexplicit-nulls"
      , "-Yindent-colons"
    )
  )

lazy val scalafmtSettings =
  Seq(
    // scalafmtOnCompile := true
  )

lazy val commandAliases =
  addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt") ++
    addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
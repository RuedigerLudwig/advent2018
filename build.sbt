// *****************************************************************************
// Projects
// *****************************************************************************

lazy val Version = new {
  val scala     = "3.0.0-RC3"
  val zio       = "1.0.7"
  val scalatest = "3.2.8"
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
      name := "aoc18"
    , scalaVersion := Version.scala
    , organization := "savinien"
    , scalacOptions ++= Seq(
        "-deprecation"
      , "-explain"
      , "-explain-types"
      , "-feature"
      , "-indent"
      // , "-language:strictEquality"
      , "-new-syntax"
      , "-unchecked"
      , "-Xfatal-warnings"
      , "-Xmigration"
      // , "-Ysave-init"
      , "-Yexplicit-nulls"
      , "-Ykind-projector"
      , "-Yrequire-targetName"
      , "-source:future"
    )
  )

lazy val scalafmtSettings =
  Seq(
    // scalafmtOnCompile := true
  )

lazy val commandAliases =
  addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt") ++
    addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v", "-s")

lazy val root = (project in file("."))
  .settings(
    name := "scalaNotes",
    libraryDependencies ++= Seq(
      "com.typesafe.akka"        %% "akka-actor"         % "2.6.9",
      "com.typesafe.akka"        %% "akka-testkit"       % "2.6.9"     % Test,
      "com.novocode"             % "junit-interface"     % "0.11"      % Test,
      "org.scalatest"            %% "scalatest"          % "3.2.2"     % Test,
      "org.scalacheck"           %% "scalacheck"         % "1.14.1"    % Test
    )
  )


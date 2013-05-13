import scalariform.formatter.preferences._

name := "scala-parsers-experiments"

version := "0.1"

description := "My experiments with Scala combinator parsers"

startYear := Some(2013)

scalaVersion := "2.10.1"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-encoding", "UTF-8"
)

libraryDependencies ++= Seq(
  "org.scala-lang"  %  "scala-reflect"  % "2.10.1"  % "compile",
  "org.specs2"      %% "specs2"         % "1.14"    % "test"
)

scalariformSettings

ScalariformKeys.preferences := FormattingPreferences()
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
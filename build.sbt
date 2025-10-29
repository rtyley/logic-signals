import ReleaseTransformations.*
import sbtversionpolicy.withsbtrelease.ReleaseVersion

organization := "com.madgag"
licenses := Seq(License.Apache2)

scalaVersion := "3.3.6"

scalacOptions := Seq("-deprecation", "-release:21")

Test / testOptions +=
  Tests.Argument(TestFrameworks.ScalaTest, "-u", s"test-results/scala-${scalaVersion.value}", "-o")

libraryDependencies ++= Seq(
  "com.gu.duration-formatting" %% "core" % "0.0.2",
  "dev.optics" %% "monocle-core"  % "3.3.0",
  "dev.optics" %% "monocle-macro" % "3.3.0",
  "com.madgag" %% "scala-collection-plus" % "1.0.0",
  "com.github.tototoshi" %% "scala-csv" % "2.0.0",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "org.scodec" %% "scodec-bits" % "1.2.4",
  "org.scodec" %% "scodec-core" % "2.3.3",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "spire" % "0.18.0",
  "com.lihaoyi" %% "upickle" % "4.4.0",
  "com.lihaoyi" %% "os-lib" % "0.11.6",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

releaseVersion := ReleaseVersion.fromAssessedCompatibilityWithLatestRelease().value
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion
)

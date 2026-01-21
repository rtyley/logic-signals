import ReleaseTransformations.*
import sbtversionpolicy.withsbtrelease.ReleaseVersion

organization := "com.madgag"
licenses := Seq(License.Apache2)

scalaVersion := "3.3.7"

scalacOptions := Seq("-deprecation", "-release:21")

Test / testOptions +=
  Tests.Argument(TestFrameworks.ScalaTest, "-u", s"test-results/scala-${scalaVersion.value}", "-o")

val spireVersion = "0.18.0"
libraryDependencies ++= Seq(
  "com.gu.duration-formatting" %% "spire-intervals" % "0.0.2",
  "dev.optics" %% "monocle-core"  % "3.3.0",
  "dev.optics" %% "monocle-macro" % "3.3.0",
  "com.madgag" %% "k-way-merge" % "0.0.2",
  "com.madgag" %% "scala-collection-plus" % "1.0.0",
  "com.github.tototoshi" %% "scala-csv" % "2.0.0",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "org.scodec" %% "scodec-bits" % "1.2.4",
  "org.scodec" %% "scodec-core" % "2.3.3",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "spire" % spireVersion,
  "org.typelevel" %% "spire-laws" % spireVersion % Test,
  "com.lihaoyi" %% "upickle" % "4.4.2",
  "com.lihaoyi" %% "os-lib" % "0.11.6",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
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

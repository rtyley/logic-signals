scalaVersion := "3.3.6"

Test / testOptions +=
  Tests.Argument(TestFrameworks.ScalaTest, "-u", s"test-results/scala-${scalaVersion.value}", "-o")

libraryDependencies ++= Seq(
  "com.madgag" %% "scala-collection-plus" % "1.0.0",
  "com.github.tototoshi" %% "scala-csv" % "2.0.0",
  "com.lihaoyi" %% "fastparse" % "3.1.1",
  "org.scodec" %% "scodec-bits" % "1.2.4",
  "org.scodec" %% "scodec-core" % "2.3.2",
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.typelevel" %% "spire" % "0.18.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)
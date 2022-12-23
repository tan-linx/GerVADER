lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "3.2.1",
      crossScalaVersions := Seq("2.13.6", "3.2.1")
    )),
    name := "scalatest-example"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

// sentence splitter
libraryDependencies += "org.apache.opennlp" % "opennlp-tools" % "1.9.2"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.apache.spark" %% "spark-core" % "3.3.1",
      "org.apache.spark" %% "spark-sql" % "3.3.1",
      "org.apache.spark" %% "spark-mllib" % "3.3.1",
    )
    case _ => Seq.empty
  }
}

// show deprecations
scalacOptions += "-deprecation"

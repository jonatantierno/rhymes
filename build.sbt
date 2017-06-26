lazy val root = (project in file(".")).enablePlugins(PlayScala)
.settings(
        version := "1.0",
        name := "rhymes",
        scalaVersion := "2.11.7",
        libraryDependencies ++= Seq(
            "org.scalaz" %% "scalaz-core" % "7.2.7",
            "org.scalatest" %% "scalatest" % "3.0.0",
            jdbc,
            cache,
            "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
            ws
            ),
        libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )
        )

scalaVersion := "2.12.1"
    
lazy val root = (project in file("."))
.settings(
        version := "1.0",
        name := "rhymes",
        scalaVersion := "2.12.1",
        libraryDependencies ++= Seq(
            "org.scalaz" %% "scalaz-core" % "7.2.7",
            "org.scalatest" %% "scalatest" % "3.0.0",
            "com.typesafe.akka" %% "akka-http" % "10.0.7" 
            )
        )

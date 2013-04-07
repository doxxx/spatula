name := "spatula"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-feature")

resolvers ++= Seq(
    "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
    "spray repo" at "http://repo.spray.io"
)

libraryDependencies ++= Seq(
    "ch.qos.logback" % "logback-classic" % "1.0.10",
    "org.clapper" % "grizzled-slf4j_2.10" % "1.0.1",
    "com.typesafe.akka" %% "akka-actor" % "2.1.2",
    "io.spray" % "spray-client" % "1.1-M7",
    "io.spray" %% "spray-json" % "1.2.3"
)

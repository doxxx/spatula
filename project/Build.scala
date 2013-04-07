import sbt._
import Keys._

/**
 * Defines project ID to be something nicer than default-hexstring.
 */
object Build extends Build {
  lazy val root = Project(
    id = "spatula",
    base = file("."),
    settings = Project.defaultSettings
  )
}

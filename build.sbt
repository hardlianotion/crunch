ThisBuild / scalaVersion := "3.1.0"

val ScalaTestVersion = "3.2.9"
val Matplotlib4jVersion = "0.5.0"

fork / run := true

lazy val crunch =
  project
    .in (file("."))
    .settings (
      scalaVersion := "3.1.0",
      libraryDependencies ++= Seq (
        "com.github.sh0nk" % "matplotlib4j" % Matplotlib4jVersion,
        "org.scalatest" %% "scalatest" % ScalaTestVersion % Test
      ))

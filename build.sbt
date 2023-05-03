ThisBuild / scalaVersion := "3.2.2"

val ScalaTestVersion = "3.2.14"
val Matplotlib4jVersion = "0.5.0"
val ScalaXmlVersion = "2.1.0"
val ZioJsonVersion = "0.5.0"

fork / run := true

lazy val crunch =
  project
    .in (file("."))
    .settings (
      scalaVersion := "3.2.2",
      libraryDependencies ++= Seq (
        "com.github.sh0nk" % "matplotlib4j" % Matplotlib4jVersion,
        "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
        "dev.zio" %% "zio-json" % ZioJsonVersion,
        "org.scala-lang.modules" %% "scala-xml" % ScalaXmlVersion
))



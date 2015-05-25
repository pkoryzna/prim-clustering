name := "rsi-prim-distributed"

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6" % "test")


resolvers ++= Seq(
  "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
)

scalacOptions in Test ++= Seq(
  "-Yrangepos"
)

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.3" % "test"

libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.6"

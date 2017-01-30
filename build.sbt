name := "ScalaExpriments"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats" % "0.9.0"
)

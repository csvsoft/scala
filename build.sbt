name := "scalaSandbox"
version := "0.1"
scalaVersion := "2.12.7"
lazy val akkaVersion = "2.5.16"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

lazy val monixDeps = Seq(
  "io.monix" %% "monix" % "3.0.0-RC2"
)

lazy val shapeLessDeps = Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

val circeVersion = "0.10.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
libraryDependencies += "io.circe" %% "circe-optics" % circeVersion


libraryDependencies ++= monixDeps
libraryDependencies ++= shapeLessDeps

libraryDependencies ++=  Seq(
    "co.fs2" %% "fs2-core" % "1.0.0",
    "co.fs2" %% "fs2-io" % "1.0.0",
    "com.opencsv" % "opencsv" % "4.3.2",
     "com.lihaoyi" %% "fastparse" % "2.0.4" ,// SBT

    "org.typelevel" %% "cats-effect" % "1.0.0",
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
  

    //"org.scalatest" % "scalatest_2.11" % "2.2.5" % "test"
     "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-language:higherKinds",
  "-encoding",
  "UTF-8",
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // For usages of features that should be imported explicitly.
  "-unchecked",
  "-Xlint",
  "-Ypartial-unification",
  "-Yresolve-term-conflict:package",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)





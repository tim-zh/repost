name := "repost"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  "org.squeryl" %% "squeryl" % "0.9.5-6",
  "com.typesafe.slick" %% "slick" % "2.0.1",
  "com.h2database" % "h2" % "1.4.178"
)

play.Project.playScalaSettings

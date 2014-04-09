name := "repost"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  "org.squeryl" %% "squeryl" % "0.9.5-6"
)     

play.Project.playScalaSettings

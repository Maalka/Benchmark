import play.Project._

name := "play-angular-require-seed"

version := "2.2"

libraryDependencies ++= Seq(
  "org.webjars" % "underscorejs" % "1.5.1",
  "org.webjars" % "jquery" % "1.10.2",
  "org.webjars" % "bootstrap" % "3.0.0" exclude("org.webjars", "jquery"),
  "org.webjars" % "angularjs" % "1.0.7" exclude("org.webjars", "jquery"),
  "org.webjars" % "requirejs" % "2.1.1",
  "org.webjars" % "webjars-play_2.10" % "2.2.0"
)

playScalaSettings

resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/repo"

// This tells Play to optimize this file and its dependencies
requireJs += "mainProd.js"

// This tells Play to read the RequireJS "shim" configuration from mainProd.js
requireJsShim += "mainProd.js"

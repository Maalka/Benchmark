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

// The main config file
// See http://requirejs.org/docs/optimization.html#mainConfigFile
requireJsShim := "build.js"

// To completely override the optimization process, use this config option:
//requireNativePath := Some("node r.js -o name=main out=javascript-min/main.min.js")

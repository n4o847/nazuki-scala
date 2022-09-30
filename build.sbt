enablePlugins(ScalaJSBundlerPlugin)

name := "Nazuki"

scalaVersion := "3.1.0"

// Run
// ```
// ncu --packageFile target/scala-3.1.0/scalajs-bundler/main/package.json
// ```
// to check updates.

Compile / npmDependencies += "react" -> "18.2.0"
Compile / npmDependencies += "react-dom" -> "18.2.0"
Compile / npmDependencies += "react-proxy" -> "1.1.8"

Compile / npmDevDependencies += "file-loader" -> "6.2.0"
Compile / npmDevDependencies += "style-loader" -> "2.0.0"
Compile / npmDevDependencies += "css-loader" -> "5.2.7"
Compile / npmDevDependencies += "html-webpack-plugin" -> "4.5.2"
Compile / npmDevDependencies += "copy-webpack-plugin" -> "6.4.1"
Compile / npmDevDependencies += "webpack-merge" -> "5.8.0"

libraryDependencies += "me.shadaj" %%% "slinky-web" % "0.7.0"
libraryDependencies += "me.shadaj" %%% "slinky-hot" % "0.7.0"
libraryDependencies += "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.0"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.9" % Test

scalacOptions += "-Ymacro-annotations"

webpack / version := "4.46.0"
startWebpackDevServer / version := "3.11.3"

webpackResources := baseDirectory.value / "webpack" * "*"

fastOptJS / webpackConfigFile := Some(
  baseDirectory.value / "webpack" / "webpack-fastopt.config.js"
)
fullOptJS / webpackConfigFile := Some(
  baseDirectory.value / "webpack" / "webpack-opt.config.js"
)
Test / webpackConfigFile := Some(
  baseDirectory.value / "webpack" / "webpack-core.config.js"
)

fastOptJS / webpackDevServerExtraArgs := Seq("--inline", "--hot")
fastOptJS / webpackBundlingMode := BundlingMode.LibraryOnly()

Test / requireJsDomEnv := true

addCommandAlias("dev", ";fastOptJS::startWebpackDevServer;~fastOptJS")

addCommandAlias("build", "fullOptJS::webpack")

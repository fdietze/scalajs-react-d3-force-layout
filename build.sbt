enablePlugins(ScalaJSPlugin)

name := "scalajs-react-d3-force-layout"

scalaVersion := "2.11.8"

organization := "com.github.fdietze"

scalaJSUseRhino in Global := false // execute js with node

// workaround for: https://github.com/scala-js/scala-js/issues/2491
scalaJSOptimizerOptions in (Compile, fullOptJS) ~= { _.withUseClosureCompiler(false) }

libraryDependencies ++= (
  "org.scala-js" %%% "scalajs-dom" % "0.9.1" ::
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1" ::
  "org.singlespaced" %%% "scalajs-d3" % "0.3.3" ::
  "com.assembla.scala-incubator" %%% "graph-core" % "1.11.0" ::
  Nil
)

// // React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
// jsDependencies ++= Seq(
//   "org.webjars.bower" % "react" % "15.1.0"
//     / "react-with-addons.js"
//     minified "react-with-addons.min.js"
//     commonJSName "react",

//   "org.webjars.bower" % "react" % "15.1.0"
//     / "react-dom.js"
//     minified "react-dom.min.js"
//     dependsOn "react-with-addons.js"
//     commonJSName "ReactDOM",

//   "org.webjars.bower" % "react" % "15.1.0"
//     / "react-dom-server.js"
//     minified "react-dom-server.min.js"
//     dependsOn "react-dom.js"
//     commonJSName "ReactDOMServer"
// )

// scalaxy (faster collection operations)
scalacOptions += "-Xplugin-require:scalaxy-streams"

scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

autoCompilerPlugins := true

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  // "-Xdisable-assertions" ::
  // "-optimize" ::
  // "-Yopt:_" :: // enables all 2.12 optimizations
  // "-Yinline" :: "-Yinline-warnings" ::
  Nil
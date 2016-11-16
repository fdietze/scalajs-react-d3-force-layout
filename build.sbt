enablePlugins(ScalaJSPlugin)

name := "scalajs-react-d3-force-layout"
organization := "com.github.fdietze"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.0"
libraryDependencies ++= (
  "org.scala-js" %%% "scalajs-dom" % "0.9.1" ::
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3" ::
  "org.singlespaced" %%% "scalajs-d3" % "0.3.4-SNAPSHOT" ::
  "com.github.fdietze" %%% "pharg" % "0.1.0-SNAPSHOT" ::
  "com.github.fdietze" %%% "vectory" % "0.1.0-SNAPSHOT" ::
  Nil
)

// scalaxy (faster collection operations)
// scalacOptions += "-Xplugin-require:scalaxy-streams"
// scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))
// scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"
// autoCompilerPlugins := true
// addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

scalaJSUseRhino in Global := false // execute js with node

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  // "-Xlint:_" ::
  // "-Ywarn-unused" ::
  // "-Xdisable-assertions" ::
  // "-optimize" ::
  // "-Yopt:_" :: // enables all 2.12 optimizations
  // "-Yinline" :: "-Yinline-warnings" ::
  Nil

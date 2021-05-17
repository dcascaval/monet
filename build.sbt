enablePlugins(ScalaJSPlugin)

name := "monet"
scalaVersion := "3.0.0-RC3"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.1.0").cross(
  CrossVersion.for3Use2_13
)

// Add support for the DOM in `run` and `test`
jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

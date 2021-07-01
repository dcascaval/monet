// name := "monet"

lazy val root = project
  .in(file("."))
  .aggregate(monet.js, monet.jvm)
  .settings(
    publish := {},
    publishLocal := {}
  )

lazy val monet = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "monet",
    version := "0.1-SNAPSHOT",
    scalaVersion := "3.0.0",
    scalacOptions ++= Seq(
      "-feature",
      "-language:implicitConversions"
    )
  )
  .jvmSettings(
    // Add JVM-specific settings her
  )
  .jsSettings(
    // Add JS-specific settings here
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += ("org.scala-js" %%% "scalajs-dom" % "1.1.0").cross(
      CrossVersion.for3Use2_13
    ),
    // Add support for the DOM in `run` and `test`
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()
  )

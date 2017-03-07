val scalaV = "2.11.8"

lazy val server = (project in file("server")).settings(
  scalaVersion := scalaV,
  scalaJSProjects := Seq(client),
  pipelineStages in Assets := Seq(scalaJSPipeline),
  pipelineStages := Seq(digest, gzip),
  // triggers scalaJSPipeline when using compile or continuous compilation
  compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
  resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",

  libraryDependencies ++= Seq(
    "com.vmunier" %% "scalajs-scripts" % "1.0.0",
    "com.typesafe.akka" %% "akka-actor" % "2.4.12",
    ws

  ),
  // Compile the project before generating Eclipse files, so that generated .scala or .class files for views and routes are present
  EclipseKeys.preTasks := Seq(compile in Compile)
).enablePlugins(PlayScala).
  dependsOn(sharedJvm)

lazy val client = (project in file("client")).settings(
  scalaVersion := scalaV,
  persistLauncher := true,
  persistLauncher in Test := false,
  //mainClass in Compile := Some("ScalaJSExample.main"),

  libraryDependencies ++= Seq(
    "com.github.japgolly.scalacss" %%% "ext-react" % "0.5.1",
    "org.scala-js" %%% "scalajs-dom" % "0.9.1",
    "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3",
    "com.github.japgolly.scalajs-react" %%% "extra" % "0.11.3",
    "org.scalaz" %% "scalaz-core" % "7.2.8",
    "io.suzaku" %%% "diode" % "1.1.1",
    "io.suzaku" % "diode-react_sjs0.6_2.11" % "1.1.1",
    "com.lihaoyi" %%% "upickle" % "0.4.3"
  ),
  jsDependencies ++=Seq(
    "org.webjars.bower" % "react" % "15.3.2" / "react-with-addons.js" minified "react-with-addons.min.js" commonJSName "React",
    "org.webjars.bower" % "react" % "15.3.2" / "react-dom.js" minified "react-dom.min.js" dependsOn "react-with-addons.js" commonJSName "ReactDOM",
    "org.webjars.bower" % "react" % "15.3.2" / "react-dom-server.js" minified "react-dom-server.min.js" dependsOn "react-dom.js" commonJSName "ReactDOMServer"
  ),
  skip in packageJSDependencies := false // creates app-jsdeps.js with the react JS lib inside
).enablePlugins(ScalaJSPlugin, ScalaJSWeb).
  dependsOn(sharedJs)



lazy val shared = (crossProject.crossType(CrossType.Pure) in file("shared")).
  settings(scalaVersion := scalaV).
  jsConfigure(_ enablePlugins ScalaJSWeb)

lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

// loads the server project at sbt startup
onLoad in Global := (Command.process("project server", _: State)) compose (onLoad in Global).value

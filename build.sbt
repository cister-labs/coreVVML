lazy val caos = project.in(file("lib/caos"))
  .enablePlugins(ScalaJSPlugin)
  .settings(scalaVersion := "3.1.2")

lazy val rootProject = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
        name := "Core VVML",
        scalaVersion := "3.1.2", // 2.13 or higher
        // ...,
        scalaJSUseMainModuleInitializer := true,
        // replace rootProject.Main by the correct path to your Main.scala class
        Compile / mainClass := Some("cVVML.frontend.Main"),
        Compile / fastLinkJS / scalaJSLinkerOutputDirectory := 
            baseDirectory.value / "lib" / "caos"/ "tool" / "js" / "gen",
        libraryDependencies ++= Seq(
          "org.typelevel" %%% "cats-parse" % "0.3.7" //).cross(CrossVersion.for3Use2_13)
        )
  ).dependsOn(caos)


  // , "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2" // needed for newers scala versions
  /// new parser
  //
  // , "org.scalactic" %% "scalactic" % "3.2.7"
  // , "org.scalatest" %% "scalatest" % "3.2.7" % "test"

  // Last stable release
  // ,"org.scalanlp" %% "breeze" % "1.1",
  // Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes.
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  // "org.scalanlp" %% "breeze-natives" % "1.1",
  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  // "org.scalanlp" %% "breeze-viz" % "1.1",

  // Optimus to solve quadratic programming problem
  // "com.github.vagmcs" %% "optimus" % "3.2.4",
  // "com.github.vagmcs" %% "optimus-solver-oj" % "3.2.4"
// )

// tests cannot be run in parallel, because of the Choco solver
// parallelExecution in Test := false

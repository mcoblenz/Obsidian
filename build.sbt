name := "Obsidian"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

/* testing dependencies */
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

/* settings for assembly */

// exclude jcodemodel
assemblyExcludedJars in assembly := {
    val classPath = (fullClasspath in assembly).value
    classPath filter {
        _.data.getName == "jcodemodel-3.0.0-SNAPSHOT.jar"
    }
}
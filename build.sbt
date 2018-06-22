name := "Obsidian"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

/* Don't throw TrapExitSecurityException, and return
 * proper status code when doing "sbt run". */
trapExit := false

/* testing dependencies */
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.5.1"

/* settings for assembly */

// exclude jcodemodel
assemblyExcludedJars in assembly := {
    val classPath = (fullClasspath in assembly).value
    classPath filter {
        _.data.getName == "jcodemodel-3.0.0-SNAPSHOT.jar"
    }
}

// don't get confused about multiple META-INF/MANIFEST.MF
assemblyMergeStrategy in assembly := {
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
}

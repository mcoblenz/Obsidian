name := "Obsidian"

version := ".1"

scalaVersion := "2.12.6"

organization := "edu.cmu.cs.obsidian"

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"

/* Don't throw TrapExitSecurityException.
 * Also, allow passing return code through SBT without
 * SBT throwing an exception. */
trapExit := false

/* testing dependencies */
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.5.1"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

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
    case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
    	oldStrategy(x)
}
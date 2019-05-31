name := "Obsidian"

version := "0.1"

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
libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.7.0"
libraryDependencies += "org.hyperledger.fabric-chaincode-java" % "fabric-chaincode-shim" % "1.4.0"
libraryDependencies += "org.hyperledger.fabric-chaincode-java" % "fabric-chaincode-protos" % "1.4.0"
libraryDependencies += "com.helger" % "jcodemodel" % "3.0.3"
libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"


/* settings for assembly */

// don't get confused about multiple META-INF/MANIFEST.MF
assemblyMergeStrategy in assembly := {
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
    case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
    	oldStrategy(x)
}
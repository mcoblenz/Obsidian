name := "Obsidian"

version := "0.1"

scalaVersion := "2.13.2"

organization := "edu.cmu.cs.obsidian"

// For dependencies of fabric-chaincode.
// We previously used Bintray, but it seemed unreliable; Travis builds failed occasionally.
resolvers += Resolver.jcenterRepo

scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

assemblyJarName in assembly := "obsidianc.jar"

unmanagedResourceDirectories in Compile += baseDirectory.value / "Obsidian_Runtime/src/main/java/Runtime/edu/cmu/cs/obsidian"
includeFilter in (Compile, unmanagedResources) := "*.obs"

/* Don't throw TrapExitSecurityException.
 * Also, allow passing return code through SBT without
 * SBT throwing an exception. */
trapExit := false

/* testing dependencies */
libraryDependencies += "junit" % "junit" % "4.12" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.2" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.2" % "test"
libraryDependencies += "org.scalatestplus" %% "scalatestplus-junit" % "1.0.0-M2"


libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.11.4"
libraryDependencies += "org.hyperledger.fabric-chaincode-java" % "fabric-chaincode-shim" % "1.4.4"
libraryDependencies += "com.helger" % "jcodemodel" % "3.3.0"
libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

/* settings for assembly */

// don't get confused about multiple META-INF/MANIFEST.MF
assemblyMergeStrategy in assembly := {
    case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
    case x if x.contains("module-info.class") => MergeStrategy.discard // https://stackoverflow.com/questions/54834125/sbt-assembly-deduplicate-module-info-class
    case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
    case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
    	oldStrategy(x)
}


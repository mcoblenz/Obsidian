package edu.cmu.cs.obsidian.tests

import java.io.File
import java.nio.file.{Files, Paths, Path}
import org.junit.Assert.assertTrue

case class Tx(txName: String, args: Seq[String])

// TODO
/* We need some way of specifying an assertion to be run at
 * the end of a test. This will likely require some changes
 * to code-gen so that we can return things from a function.
 * We also should support many different intermingling contracts
 * in the future */
case class TestDeployment(file: String, transactions: Seq[Tx])

object TestHarness {

    def recDelete(f: File): Unit = {
        if (f.isDirectory) {
            for (sub_f <- f.listFiles()) {
                recDelete(sub_f)
            }
        }

        f.delete()
    }

    /* [assert]s that the exit code is 0 */
    def run(executableJar: Path, args: String*): Unit = {
        val compilerCmd: Array[String] =
            ("java" +: "-jar" +: executableJar.toString +: args).toArray
        val proc = Runtime.getRuntime().exec(compilerCmd)
        proc.waitFor()
        assertTrue(proc.exitValue == 0)
    }

    def doTransaction(tx: Tx, port: Int): Unit = {
        // TODO : communicate with the server
    }

    /* returns a handle to the (potentially still running) process */
    def start(executableJar: Path, args: String*): Process = {
        val compilerCmd: Array[String] =
            ("java" +: "-jar" +: executableJar.toString +: args).toArray
        Runtime.getRuntime().exec(compilerCmd)
    }

    private val testTmpPath = Paths.get("test_tmp")

    def runTest(test: TestDeployment, obsCompiler: String, mainClass: String): Unit = {
        /* create temp directory */
        val tmpDir = Files.createDirectory(testTmpPath)

        val compilerPath = Paths.get(obsCompiler)

        /* compile the transactions */
        run(compilerPath, "--output-path", tmpDir.toString)

        val jarPath = tmpDir.resolve(s"$mainClass.jar")

        val serverProc = start(jarPath)

        for (tx <- test.transactions) {
            doTransaction(tx, 4000)
        }

        serverProc.destroy()
        serverProc.waitFor()

        recDelete(tmpDir.toFile)
    }
}

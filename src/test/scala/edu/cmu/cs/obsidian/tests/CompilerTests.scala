package edu.cmu.cs.obsidian.tests


import java.nio.file.Files

import org.scalatest.junit.JUnitSuite
import org.junit.Assert.{assertTrue, fail}
import _root_.org.junit.Test

import scala.sys.process._
import edu.cmu.cs.obsidian._

class CompilerTests extends JUnitSuite {

  def testContract(contractName : String) = {
    var result = true
    val inputArgs: Array[String] = Array(s"--output-path", s"obs_output/", s"resources/tests/compilerTests/$contractName.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
    val gradleCmd = s"gradle compileJava -b obs_output/$contractName/build.gradle"
    val gradleResult = gradleCmd.!
    assertTrue(gradleResult == 0)
    val deleteCmd = s"rm -rf obs_output/$contractName"
    deleteCmd.!
  }

  @Test def intContainer(): Unit = {
    testContract("IntContainer")
  }

  @Test def primitiveTypes(): Unit = {
    testContract("PrimitiveTypes")
  }

  @Test def simple(): Unit = {
    testContract("Simple")
  }

  @Test def simple3(): Unit = {
    testContract("Simple3")
  }

  @Test def simpleVerification(): Unit = {
    testContract("SimpleVerification")
  }

  @Test def constructorWithArgs(): Unit = {
    testContract("ConstructorWithArgs")
  }

  @Test def stateTransactions(): Unit = {
    testContract("StateTransactions")
  }

  @Test def negativeNumbers(): Unit = {
    testContract("NegativeNumber")
  }

  @Test def giftCertificate(): Unit = {
    testContract("GiftCertificate")
  }

  @Test def testFFI(): Unit = {
    testContract(contractName = "TestFFI")
  }
}

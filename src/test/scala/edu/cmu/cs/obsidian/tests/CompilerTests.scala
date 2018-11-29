package edu.cmu.cs.obsidian.tests


import org.scalatest.junit.JUnitSuite
import org.junit.Assert.{assertTrue, fail}
import _root_.org.junit.Test
import edu.cmu.cs.obsidian._

class CompilerTests extends JUnitSuite {
  @Test def intContainer(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/IntContainer.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def nestedContracts(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/nestedContracts.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def primitiveTypes(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/PrimitiveTypes.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def simple(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/simple.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def simple3(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/simple3.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def simpleVerification(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/simpleVerification.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def constructorWithArgs(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/ConstructorWithArgs.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def stateTransactions(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/StateTransactions.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }

  @Test def negativeNumbers(): Unit = {
    var result = true
    val inputArgs: Array[String] = Array("--dump-debug", "obs_output", "resources/tests/compilerTests/NegativeNumbers.obs")
    result = Main.compileProgram(inputArgs)
    assertTrue(result)
  }
}

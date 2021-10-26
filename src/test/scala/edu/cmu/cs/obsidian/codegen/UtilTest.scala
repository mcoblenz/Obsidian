package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck._
import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.{assertTrue, fail}

import java.io.FileInputStream
import scala.collection.mutable.ArrayBuffer

class UtilTest extends JUnitSuite {



    @Test def sizeOfOneInt() : Unit = {
        val simple : ContractType = new ContractType("Simple", Seq(IntType()))
        assertTrue(Util.sizeOfContractType(simple) == 32)
    }

    @Test def sizeOfTwoInts() : Unit = {
        val simple : ContractType = new ContractType("Simple", Seq(IntType(),IntType()))
        assertTrue(Util.sizeOfContractType(simple) == 64)
    }
}

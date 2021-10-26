package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck._
import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.{assertTrue, fail}

import java.io.FileInputStream
import scala.collection.mutable.ArrayBuffer

class UtilTest extends JUnitSuite {

    private def runTest(file: String, expectedSize: Int): Unit = {
        var prog: Program = null
        try {
            prog = Parser.parseFileAtPath(file, new FileInputStream(file), printTokens = false)
        }
        catch {
            case p: Parser.ParseException =>
                val errMsg = p.message
                fail(s"Failed with parser message $errMsg")
        }

        val (importsProcessedAst, _) = ImportProcessor.processImports(file, List(), prog)
        val fieldsLiftedAst = StateFieldTransformer.transformProgram(importsProcessedAst)

        val table = new SymbolTable(fieldsLiftedAst)
        val (globalTable: SymbolTable, transformErrors) = StateNameValidator.transformProgram(table)

        val checker = new Checker(globalTable)
        val errs = (checker.checkProgram()._1 ++ transformErrors).sorted

        assertTrue(s"The size of the contracts in ${file} did not match the expected sizes", true)
    }

    @Test def sizeOfOneInt() : Unit = {
        val simple : ContractType = new ContractType("Simple", Seq(IntType()))
        assertTrue(Util.sizeOfContractType(simple) == 32)
    }

    @Test def sizeOfTwoInts() : Unit = {
        val simple : ContractType = new ContractType("Simple", Seq(IntType(),IntType()))
        assertTrue(Util.sizeOfContractType(simple) == 64)
    }
}

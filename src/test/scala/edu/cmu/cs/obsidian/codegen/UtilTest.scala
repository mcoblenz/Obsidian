package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck._
import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.{assertTrue, fail}

import java.io.FileInputStream
import scala.collection.mutable.ArrayBuffer

class UtilTest extends JUnitSuite {

    private def runTest(file: String, expectedSizes: Map[String,Int]): Unit = {
        // every AST has a contract called "Contract" in it, so we add that to the expected sizes manually
        val expected: Map[String,Int] = expectedSizes + ("Contract" -> 0)

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

        if (errs.nonEmpty) {
            fail(s"Type checking errors: ${errs.toString}")
        }

        // todo this is just for debugging; remove it when it works.
        for(c <- globalTable.ast.contracts){
            println(s"contract: ${c.toString}\nbound:${c.bound.toString()}")
        }

        // todo this is wrong; the contract type stored in bound is to do with subtyping, it's not the type of the contract.
        //   that information must be in the Checker some where, but perhaps is not emitted in the same way that expression ASTs
        //   were not adorned with their typed until I changed it.
        val contractSizes: Map[String, Int] = Map.from(globalTable.ast.contracts.map(c => (c.name, Util.sizeOfContractType(c.bound))))

        assertTrue(s"The size of the contracts in ${file} did not match the expected sizes: ${contractSizes.toString} vs ${expected.toString}",
            contractSizes.equals(expected))
    }

    @Test def sizeOfOneInt() : Unit = {
        val simple : ContractType = new ContractType("Simple", Seq(IntType()))
        assertTrue(Util.sizeOfContractType(simple) == 32)
    }

    @Test def sizeOfTwoInts() : Unit = {
        val simple : ContractType = new ContractType("Simple", Seq(IntType(),IntType()))
        assertTrue(Util.sizeOfContractType(simple) == 64)
    }

    @Test def sizeOfSetGetNoArgsNoConstructNoInit() : Unit = {
        runTest("resources/tests/GanacheTests/SetGetNoArgsNoConstructNoInit.obs",
            Map("SetGetNoArgsNoConstructNoInit" -> 0, "IntContainer" -> 32))
    }
}

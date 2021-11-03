package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck._
import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertTrue

import java.io.FileInputStream

class UtilTest extends JUnitSuite {

    private def pullSymbolTable(file: String): SymbolTable = {
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

        checker.checkProgram()._2
    }

    private def runSizeTest(file: String, expectedSizes: Map[String, Int]): Unit = {
        // every AST has a contract called "Contract" in it, so we add that to the expected sizes manually
        val expected: Map[String, Int] = expectedSizes + ("Contract" -> 0)

        // stash the symbol table that we get from the type checker so that we don't have to rerun
        // checkProgram every time below
        val symbols = pullSymbolTable(file)

        // given a contract, return its name paired with the size of its type
        def contractPair(c: Contract): (String, Int) = {
            symbols.contract(c.name) match {
                case Some(value) => (c.name, Util.sizeOfContract(value))
                case None => assertTrue("the type checker produced a symbol table missing contracts", false); ("", -1)
            }
        }

        // iterate over the contracts in the symbol table's AST and compute all the sizes
        val contractSizes: Map[String, Int] = Map.from(symbols.ast.contracts.map(contractPair))

        // the test passes if the map of expected sizes agrees with the computed one.
        assertTrue(s"The size of the contracts in ${file} did not match the expected sizes: ${contractSizes.toString} vs ${expected.toString}",
            contractSizes.equals(expected))
    }

    private def runOffsetTest(file: String, contractName: String, fieldName: String, expectedOffset: Int): Unit = {
        val symbols = pullSymbolTable(file)
        symbols.contract(contractName) match {
            case Some(value) => assertTrue("offset did not match expected offset", Util.offsetOfField(value, fieldName) == expectedOffset)
            case None => assertTrue("type checker produced a symbol table missing a contract", false)
        }
    }

    @Test def sizeOfSetGetNoArgsNoConstructNoInit(): Unit = {
        runSizeTest("resources/tests/GanacheTests/SetGetNoArgsNoConstructNoInit.obs",
            Map("SetGetNoArgsNoConstructNoInit" -> 0, "IntContainer" -> 32))
    }

    @Test def sizeOfSetGetTwoInts(): Unit = {
        runSizeTest("resources/tests/GanacheTests/SG.obs",
            Map("SG" -> 0, "IntContainer" -> 4*32))
    }

    @Test def offsetOfXSetGetTwoInts(): Unit = {
        runOffsetTest("resources/tests/GanacheTests/SG.obs", "IntContainer", "x", 0)
    }

    @Test def offsetOfYSetGetTwoInts(): Unit = {
        runOffsetTest("resources/tests/GanacheTests/SG.obs", "IntContainer", "y", 32)
    }

    @Test def offsetOfZSetGetTwoInts(): Unit = {
        runOffsetTest("resources/tests/GanacheTests/SG.obs", "IntContainer", "z", 64)
    }

    @Test def offsetOfFSetGetTwoInts(): Unit = {
        runOffsetTest("resources/tests/GanacheTests/SG.obs", "IntContainer", "f", 96)
    }
}

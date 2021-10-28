package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck._
import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.{assertTrue, fail}

import java.io.FileInputStream

class UtilTest extends JUnitSuite {

    private def runTest(file: String, expectedSizes: Map[String, Int]): Unit = {
        // every AST has a contract called "Contract" in it, so we add that to the expected sizes manually
        val expected: Map[String, Int] = expectedSizes + ("Contract" -> 0)

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

        // stash the symbol table that we get from the type checker so that we don't have to rerun
        // checkProgram every time below
        val symbols = checker.checkProgram()._2

        // given a contract, return its name paired with the size of its type
        def contractPair(c: Contract): (String, Int) = {
            val ct = symbols.contract(c.name)
            ct match {
                case Some(value) => (c.name, Util.sizeOfContract(value))
                case None => assertTrue("the typechecker produced a symbol table missing contracts", false); ("", -1)
            }
        }

        // iterate over the contracts in the symboltable's AST and compute all the sizes
        val contractSizes: Map[String, Int] = Map.from(symbols.ast.contracts.map(contractPair))

        // the test passes if the map of expected sizes agrees with the computed one.
        assertTrue(s"The size of the contracts in ${file} did not match the expected sizes: ${contractSizes.toString} vs ${expected.toString}",
            contractSizes.equals(expected))
    }

    @Test def sizeOfOneInt(): Unit = {
        //val simple : ContractType = new ContractType("Simple", Seq(IntType()))
        val con: Contract = new Contract("Simple", "")
        val sym: SymbolTable = new SymbolTable(Program(Seq(), Seq()))
        val simple: ContractTable = new ContractTable(con, sym, None)
        assertTrue(Util.sizeOfContract(simple) == 32)
    }
    //
    //    @Test def sizeOfTwoInts() : Unit = {
    //        val simple : ContractType = new ContractType("Simple", Seq(IntType(),IntType()))
    //        assertTrue(Util.sizeOfContractType(simple) == 64)
    //    }

    @Test def sizeOfSetGetNoArgsNoConstructNoInit(): Unit = {
        runTest("resources/tests/GanacheTests/SetGetNoArgsNoConstructNoInit.obs",
            Map("SetGetNoArgsNoConstructNoInit" -> 0, "IntContainer" -> 32))
    }
}

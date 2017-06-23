package edu.cmu.cs.obsidian.tests


import org.junit.Assert.assertTrue
import org.junit.Test
import org.scalatest.junit.JUnitSuite
import edu.cmu.cs.obsidian.typecheck._
import edu.cmu.cs.obsidian.parser._

class TypeCheckerTests extends JUnitSuite {
    type LineNumber = Int
    private def runTest(file: String, expectedErrors: Seq[(Error, LineNumber)]): Unit = {
        val prog = Parser.parseFileAtPath(file, printTokens = false)
        val checker = new Checker()
        val errs = checker.checkProgram(prog)
        var remaining = expectedErrors
        for (err <- errs) {
            val pred = (expected: (Error, LineNumber)) => {
                expected._1 == err && expected._2 == err.loc.line
            }
            assertTrue(s"Nothing matches $err", remaining.exists(pred))
            remaining = remaining.filterNot(pred)
        }
        val msg = s"The following errors weren't found when checking: $remaining"
        assertTrue(msg, remaining.isEmpty)
    }

    @Test def basicTest(): Unit = {
        runTest("resources/tests/ExampleTypeFailure.obs",
            (SubTypingError(BoolType(), IntType()), 18)
                ::(WrongArityError(1, 0, "createC"), 19)
                ::(LeakReturnValueError("createC"), 19)
                ::(SubTypingError(BoolType(), IntType()), 20)
                ::Nil
        )
    }
}

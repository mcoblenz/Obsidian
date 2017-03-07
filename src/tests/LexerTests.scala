package edu.cmu.cs.obsidian.tests

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertTrue
import edu.cmu.cs.obsidian.lexer._

class LexerTests extends JUnitSuite {

    private def shouldSucceed(src: String): Unit = {
        val result = Lexer.tokenize(src)
        assertTrue(result.isRight)
    }
    private def shouldFail(src: String): Unit = {
        val result = Lexer.tokenize(src)
        assertTrue(result.isLeft)
    }

    private def testAndPrint(src: String): Unit = {
        println(Lexer.tokenize(src))
    }

    private def shouldEqual(src: String, tokens: Seq[Token]): Unit = {
        val result = Lexer.tokenize(src)
        assertTrue(result.isRight && result.right.get == tokens)
    }

    private def shouldEqual(src1: String, src2: String): Unit = {
        val result1 = Lexer.tokenize(src1)
        val result2 = Lexer.tokenize(src2)
        assertTrue((result1.isRight && result2.isRight) || (result1.isLeft && result2.isLeft))
        (result1, result2) match {
            case (Right(res1), Right(res2)) => assertTrue(res1 == res2)
            case _ => ()
        }
    }

    @Test def keywords(): Unit = {
        shouldEqual(
            """
              | contract Contract if else transaction trans function function1 type state try
              | catch throw not and or return linear new newa
            """.stripMargin,
            List(
                ContractT(), IdentifierT("Contract"), IfT(), ElseT(), TransactionT(), IdentifierT("trans"),
                FunctionT(), IdentifierT("function1"), TypeT(), StateT(), TryT(), CatchT(), ThrowT(), NotT(),
                AndT(), OrT(), ReturnT(), LinearT(), NewT(), IdentifierT("newa")
            )
        )
    }

    @Test def simpleProgram(): Unit = {
        shouldSucceed(
            """
              | contract C { state S { transaction a() { this.call(); this = that; }}}
            """.stripMargin
        )
    }

    @Test def simpleProgramWithWeirdWhitespace(): Unit = {
        shouldEqual(
            """
              | contract C{state S{transaction a  (  ){this . call(  );this=  that ;}} }
            """.stripMargin,
            """
              | contract C { state S { transaction a() { this.call(); this = that; }}}
            """.stripMargin
        )
    }

    /*
    /* TODO: devise a parsing scheme where these fail */
    @Test def integerValues(): Unit = {
        shouldFail("0000")
        shouldFail("0aaa")
    }*/
}

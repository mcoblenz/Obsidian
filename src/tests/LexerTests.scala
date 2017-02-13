package tests

import org.scalatest.junit.JUnitSuite
import org.junit.Test

import Lexer._

class LexerTests extends JUnitSuite {

    private def shouldSucceed(src : String) : Unit = {
        Lexer.tokenize(src) match {
            case Right(_) => ()
            case Left(_) => assert(false)
        }
    }
    private def shouldFail(src : String) : Unit = {
        Lexer.tokenize(src) match {
            case Right(_) => assert(false)
            case Left(_) => ()
        }
    }

    private def testAndPrint(src : String) : Unit = {
        println(Lexer.tokenize(src))
    }

    private def shouldEqual(src : String, tokens : Seq[Token]) : Unit = {
        Lexer.tokenize(src) match {
            case Right(res) => assert(res == tokens)
            case Left(_) => assert(false)
        }
    }

    private def shouldEqual(src1 : String, src2 : String) : Unit = {
        (Lexer.tokenize(src1), Lexer.tokenize(src2)) match {
            case (Right(res1), Right(res2)) => assert(res1 == res2)
            case _ => ()
        }
    }

    @Test def keywords() : Unit = {
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

    @Test def simpleProgram() : Unit = {
        shouldSucceed(
            """
              | contract C { state S { transaction a() { this.call(); this = that; }}}
            """.stripMargin
        )
    }

    @Test def simpleProgramWithWeirdWhitespace() : Unit = {
        shouldEqual(
            """
              | contract C{state S{transaction a  (  ){this . call(  );this=  that ;}} }
            """.stripMargin,
            """
              | contract C { state S { transaction a() { this.call(); this = that; }}}
            """.stripMargin
        )
    }

    /* TODO: devise a parsing scheme where these fail */
    @Test def integerValues() : Unit = {
        shouldFail("0000")
        shouldFail("0aaa")
    }
}

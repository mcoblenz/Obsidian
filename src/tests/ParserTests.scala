package edu.cmu.cs.obsidian.tests

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Assert.assertTrue
import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.parser._

class ParserTests extends JUnitSuite {

    private def parse(src: String): Either[String, AST] = {
        val result = Lexer.tokenize(src)
        val tokens: Seq[Token] = result match {
            case Left(msg) => {
                println(s"Lexing Failed: $msg")
                return Left("Doesn't lex")
            }
            case Right(res) => res
        }
        assertTrue(result.isRight)
        Parser.parseProgram(tokens)
    }

    private def shouldSucceed(src: String): Unit = {
        val result = parse(src)
        assertTrue(result.isRight)
    }
    private def shouldFail(src: String): Unit = {
        val result = parse(src)
        assertTrue(result.isLeft)
    }

    private def testAndPrint(src: String): Unit = {
        println(parse(src))
    }

    private def shouldEqual(src: String, ast: AST): Unit = {
        val result = parse(src)
        assertTrue(result.isRight && result.right.get == ast)
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

    @Test def simpleContracts() = {
        shouldSucceed(
            """
              | main contract C { transaction f() { hello = 1; world = 2; } }
            """.stripMargin)
        shouldSucceed(
            """
              | main contract C {
              |     state S1 { }
              | }
            """.stripMargin)
    }

    @Test def goodExpressions() = {
        shouldSucceed(
            """
              | main contract C {
              |     state S1 {
              |         transaction t1() {
              |             x = (x.f.y.z((((5)))));
              |             (x).f = (new A()).f;
              |         }
              |     }
              | }
            """.stripMargin
        )
    }

    @Test def goodStatements() = {
        shouldSucceed(
            """
              | main contract C {
              |     state S1 {
              |         transaction t1() {
              |             x.x = x.f1.f2.f3();
              |             x = x();
              |             x();
              |             x().f = x();
              |             x.f();
              |             x.f.f();
              |             new A();
              |             a = new A();
              |             A a = new A();
              |             T x;
              |             T x = x;
              |             T x = x.f();
              |             return;
              |             return x;
              |             return x.f;
              |         }
              |     }
              | }
            """.stripMargin
        )
    }

    @Test def badStatements() = {
        shouldFail("""main contract C { state S { transaction t() {
              | new;
              | }}}""".stripMargin)
        shouldFail("""main contract C { state S { transaction t() {
              | return (;
              | }}}""".stripMargin)
    }

    @Test def goodFuncArgs() = {
        shouldSucceed(
            """
              | main contract C {
              |     state S1 {
              |         function f() { return x; }
              |         function f(T x) { return x; }
              |         function f(T1 x, linear T2 y, T3 z) { return x; }
              |
              |         transaction t() { return x; }
              |         transaction t(T x) { return x; }
              |         transaction t(T1 x, linear T2 y, T3 z) {
              |             f(x, y, z);
              |             f();
              |             x.f(x, y, z);
              |             f(x);
              |             f(x.f);
              |             f(5, x, f());
              |             f(g(g(5)));
              |         }
              |     }
              | }
            """.stripMargin
        )
    }

    @Test def badFuncArgs() = {
        shouldFail("""main contract C { state S {
              | transaction t(x) { return x; }
              | }}""".stripMargin)
        shouldFail("""main contract C { state S {
              | function t(x) { return x; }
              | }}""".stripMargin)
        shouldFail("""main contract C { state S {
              | transaction t(x, T x) { return x; }
              | }}""".stripMargin)
        shouldFail("""main contract C { state S { transaction t() {
              | f(T x);
              | }}}""".stripMargin)
    }

    @Test def transitions() = {
        shouldSucceed(
            """ main contract C { state S {
              | transaction t(T x) { ->S({x = y, y = x}); }
              | }}
            """.stripMargin)
        shouldSucceed(
            """ main contract C { state S {
              | transaction t(T x) { ->S({}); }
              | }}
            """.stripMargin)
        shouldSucceed(
            """ main contract C { state S {
              | transaction t(T x) { ->S; }
              | }}
            """.stripMargin)
        shouldSucceed(
            """ main contract C { state S {
              | transaction t(T x) { ->S({x = y}); }
              | }}
            """.stripMargin)
    }
}

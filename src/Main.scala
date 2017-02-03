import Lexer._
import ObsidianParser._

object Main {
    val cases = List(
        """contract { transaction f() { hello = 1; world = 2; } }""",
        """if (x == y) { 5 } else { 3 }""",
        """ contract C {
                state S1 {
                    type s;
                    transaction t1(T1 a, T2 b) {
                        do;
                        stuff;
                        call();
                    }
                }""",
        """try { external.call(); }"""
    )

    val exprCases = List(
        """(x)""",
        """x + y * z.f""",
        """x * y + x * y""",
        """x * (y + x) * y""",
        """x + y * x + y""",
        """x == y.f and z != x + 3""",
        """(x == (y.f * (1 + ((x))))) and z == 3"""
    )

    val statementCases = List(
        """if x == y { throw; } else { x = y }""",
        """
           if x == y and y == z { throw; }
           x.f = y;
           y.f = x;
           try { throw; } catch { x = y; }
        """
    )

    def printAST(src : String) : Unit = {
        val tokens : Seq[Token] = tokenize(src) match {
            case Left(msg) => println(msg); return
            case Right(ts) => ts
        }
        val ast : AST = parseAST(tokens) match {
            case Left(msg) => println(msg); return
            case Right(tree) => tree
        }
        println(ast)
    }

    def main(args: Array[String]) : Unit = {
        statementCases.map(printAST)
    }
}
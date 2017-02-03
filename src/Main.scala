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
        """try { external.call();"""
    )

    val exprCases = List(
        """(x + y)"""
        /*
        """x + y * z.f""",
        """x * y + x * y""",
        """x * (y + x) * y""",
        """x + y * x + y""",
        """x == y.f and z != x + 3"""
        */
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
        exprCases.map(printAST)
    }
}
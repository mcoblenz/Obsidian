import Lexer._
import Parser._

object Main {
    val cases = List(
        """
contract C { transaction f() { hello = 1; world = 2; } }
        """,
        """
contract C {
    state S1 { }
}
        """,
        """
contract C {
    state S1 {
        transaction t1() {
            x.x = x.x.x.x();
            x = x.x.x.x();
            x();
            x().f = x();
            x.f.f();
            new A();
            a = new A();
            T x;
            T x = x;
            T x = x.f();
            T x = new T();
        }
    }
}
        """,
        """
contract C {
    state S1 {
        transaction t1() {
            return f(;
        }
    }
}
        """
    )

    def printAST(ast : AST) : Unit = {
        println(ast.toString().replaceAll(",", "\n,"))
    }

    def run(src : String) : Unit = {
        val tokens : Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => println(msg); return
            case Right(ts) => ts
        }

        val ast : AST = Parser.parseAST(tokens) match {
            case Left(msg) => println(msg); return
            case Right(tree) => tree
        }

        printAST(ast)
        println()
    }

    def main(args: Array[String]) : Unit = {
        cases.map(run)
    }
}
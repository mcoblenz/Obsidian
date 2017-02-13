import Lexer._
import Parser._

object Main {

    def run(src : String) : Unit = {
        val tokens : Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => println(msg); return
            case Right(ts) => ts
        }

        val ast : AST = Parser.parseAST(tokens) match {
            case Left(msg) => println(msg); return
            case Right(tree) => tree
        }

        println(ast)
    }

    def main(args : Array[String]) : Unit = {
        if (args.length == 0) {
            println("Provide at least one file as an argument")
            return
        }

        for (fileName <- args) {
            val source = scala.io.Source.fromFile(fileName)
            val srcString = try source.getLines() mkString "\n" finally source.close()
            run(srcString)
            println()
        }
    }
}
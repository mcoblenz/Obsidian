import Lexer._
import Parser._
import CodeGen._

object Main {

    def run(src : String) : Unit = {
        val sep = "============================================================================"
        val tokens : Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => println(msg); return
            case Right(ts) => ts
        }

        println("Tokens")
        println(sep)
        println()
        println(tokens)
        println()
        println(sep)

        val ast : Program = Parser.parseProgram(tokens) match {
            case Left(msg) => println(msg); return
            case Right(tree) => tree
        }

        println("AST")
        println(sep)
        println()
        println(ast)
        println()
        println(sep)

        val codeGen = new CodeGen()
        codeGen.translateProgram(ast)

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
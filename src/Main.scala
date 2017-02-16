import java.io.File
import java.nio.file.{Files, Paths}

import Lexer._
import _root_.Parser._
import CodeGen._

class CompilerOptions (val printTokens: Boolean,
                       val printAST: Boolean) {
}



object Main {

    def compile(srcPath: String, options: CompilerOptions): Unit = {
        val bufferedSource = scala.io.Source.fromFile(srcPath)
        val src = try bufferedSource.getLines() mkString "\n" finally bufferedSource.close()

        val sep = "============================================================================"
        val tokens: Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => println(msg); return
            case Right(ts) => ts
        }

        if (options.printTokens) {
            println("Tokens:")
            println(sep)
            println()
            println(tokens)
            println()
            println(sep)
        }

        val ast: Program = Parser.parseProgram(tokens) match {
            case Left(msg) => println(msg); return
            case Right(tree) => tree
        }

        if (options.printAST) {
            println("AST")
            println(sep)
            println()
            println(ast)
            println()
            println(sep)
        }

        val codeGen = new CodeGen()
        val javaModel = codeGen.translateProgram(ast)
        val where = "out/generated_java"
        Files.createDirectories(Paths.get(where))
        javaModel.build(new File(where))
    }

    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            println("Provide at least one file as an argument")
            return
        }

        var printTokens = false
        var printAST = false
        var inputFiles: List[String] = Nil

        def parseOptions(list: List[String]): Unit = {
            def isSwitch (s: String) = s(0) == '-'

            list match {
                case Nil =>
                case "--print-tokens" :: tail =>
                    printTokens = true
                    parseOptions (tail)

                case "--print-ast" :: tail =>
                    printAST = true
                    parseOptions (tail)

                case option :: tail =>
                    if (option.startsWith("--") || option.startsWith("-")) {
                        println("Unknown option " + option)
                        sys.exit(1)
                    }
                    else if (option.endsWith(".obs")) {
                        // This is an input file.
                        inputFiles = option :: inputFiles
                        parseOptions (tail)
                    }
                    else {
                        println("Unknown argument " + option)
                        sys.exit(1)
                    }

            }
        }

        parseOptions(args.toList)

        val options = new CompilerOptions(printTokens, printAST)


        for (filename <- inputFiles) {
            compile(filename, options)
        }
    }
}
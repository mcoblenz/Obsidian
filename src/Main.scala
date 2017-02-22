import java.io.File
import java.nio.file.{Files, Paths}

import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.protobufgen._
import com.sun.codemodel.internal.JCodeModel

class CompilerOptions (val printTokens: Boolean,
                       val printAST: Boolean) {
}

class ParseException (val message : String) extends Exception {

}



object Main {
    val sep = "============================================================================"

    def parse(srcPath: String, options: CompilerOptions): Program = {
        val bufferedSource = scala.io.Source.fromFile(srcPath)
        val src = try bufferedSource.getLines() mkString "\n" finally bufferedSource.close()

        val tokens: Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => throw new ParseException(msg)
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
            case Left(msg) => println(msg); throw new ParseException(msg)
            case Right(tree) => tree
        }

        ast
    }

    def compile (ast : Program) : JCodeModel = {
        val codeGen = new CodeGen()
        codeGen.translateProgram(ast)
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

        val protobufOutputDir = "out/generated_protobuf"
        val javaOutputDir = "out/generated_java"

        Files.createDirectories(Paths.get(protobufOutputDir))
        Files.createDirectories(Paths.get(javaOutputDir))

        for (filename <- inputFiles) {
            try {
                val ast = parse(filename, options)

                if (options.printAST) {
                    println("AST")
                    println(sep)
                    println()
                    println(ast)
                    println()
                    println(sep)
                }

                val javaModel = compile(ast)
                javaModel.build(new File(javaOutputDir))

                val p : Protobuf = ProtobufGen.translateProgram(ast)
                val lastSlash = filename.lastIndexOf("/")

                val sourceFilename = if (lastSlash < 0) filename else filename.substring(lastSlash + 1)

                val protobufFilename: String = sourceFilename.replace(".obs", ".proto")
                p.build(new File(protobufOutputDir, protobufFilename))
            } catch {
                case e: ParseException => println(e.message)
            }

        }
    }
}
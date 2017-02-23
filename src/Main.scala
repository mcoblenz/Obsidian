import java.io.File
import java.nio.file.{Files, Paths}
import java.util.Scanner

import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.protobuf._
import com.sun.codemodel.internal.JCodeModel

import scala.sys.process._

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

    def findMainContractName(prog: Program): String = {
        for (aContract <- prog.contracts) {
            if (aContract.mod == Some(IsMain)) {
                return aContract.name
            }
        }
        throw new RuntimeException("No main contract found")
    }

    def compile (ast: Program): JCodeModel = {
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
        var printJavacOutput = false
        var inputFiles: List[String] = Nil

        def parseOptions(list: List[String]): Unit = {
            def isSwitch (s: String) = s(0) == '-'

            list match {
                case Nil =>
                case "--print-javac" :: tail =>
                    printJavacOutput = true
                    parseOptions (tail)
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
        val javaSrcOutputDir = "out/generated_java"
        val javaJarOutputDir = "out/generated_jars"

        Files.createDirectories(Paths.get(protobufOutputDir))
        Files.createDirectories(Paths.get(javaJarOutputDir))
        Files.createDirectories(Paths.get(javaSrcOutputDir))

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
                javaModel.build(new File(javaSrcOutputDir))

                val lastSlash = filename.lastIndexOf("/")
                val sourceFilename = if (lastSlash < 0) filename else filename.substring(lastSlash + 1)
                val protobufFilename: String = sourceFilename.replace(".obs", ".proto")
                val protobufPath = protobufOutputDir + "/" + protobufFilename

                val p : Protobuf = ProtobufGen.translateProgram(ast)
                p.build(new File(protobufOutputDir, protobufFilename))

                // Invoke protoc to compile from protobuf to Java.
                val protocInvocation : String = "protoc --java_out=" + javaSrcOutputDir + " " + protobufPath

                try {
                    val exitCode = protocInvocation.!
                    if (exitCode != 0) {
                        println("`" + protocInvocation + "` exited abnormally: " + exitCode)
                    }
                } catch {
                    case e: Throwable => println("Error running protoc: " + e)
                }

                /* compile the java code */
                val mainName = findMainContractName(ast)
                val classPath = "Obsidian Runtime/src/Runtime/:out/generated_java/"
                val srcFile = s"out/generated_java/edu/cmu/cs/obsidian/generated_code/$mainName.java"
                val compileCmd: Array[String] = Array("javac", "-classpath", classPath, srcFile)
                val proc: java.lang.Process = Runtime.getRuntime().exec(compileCmd)

                if (printJavacOutput) {
                    val compilerOutput = proc.getErrorStream()
                    val untilEOF = new Scanner(compilerOutput).useDelimiter("\\A")
                    val result = if (untilEOF.hasNext()) {
                        untilEOF.next()
                    } else {
                        ""
                    }

                    print(result)
                }

                proc.waitFor()
                val exitCode = proc.exitValue()
                if (printJavacOutput) {
                    println("javac exited with value " + exitCode)
                }

                if (exitCode == 0) {

                    val classFile = s"out/generated_java/edu/cmu/cs/obsidian/generated_code/$mainName.class"
                    val outputJar = s"out/generated_jars/$mainName.jar"
                    val jarCmd: Array[String] = Array("jar", "-cf", outputJar, classFile)
                    val procJar: java.lang.Process = Runtime.getRuntime().exec(jarCmd)
                    procJar.waitFor()
                }

            } catch {
                case e: ParseException => println(e.message)
            }

        }
    }
}
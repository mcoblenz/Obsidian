import java.io.File
import java.nio.file.{Files, Paths}
import java.util.{Locale, Scanner}

import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.protobuf._
import edu.cmu.cs.obsidian.util._
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

    def compile (ast: Program, protobufOuterClassName: String): JCodeModel = {
        val codeGen = new CodeGen(protobufOuterClassName)
        codeGen.translateProgram(ast)
    }

    /* returns the exit code of the javac process */
    def compileCode(printJavacOutput: Boolean, mainName: String): Int  = {
        val classPath = "Obsidian Runtime/src/Runtime/:out/generated_java/:lib/protobuf-java-3.2.0.jar"
        val srcFile = s"out/generated_java/edu/cmu/cs/obsidian/generated_code/$mainName.java"
        val compileCmd: Array[String] = Array("javac", "-d", "out/generated_bytecode",
                                                       "-classpath", classPath,
                                                        srcFile)
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
        proc.exitValue()
    }

    /* returns the exit code of the jar process */
    def makeJar(printJavacOutput: Boolean, mainName: String): Int  = {
        val outputJar = s"out/generated_jars/$mainName.jar"
        val manifest = s"Obsidian Runtime/protobuf_manifest.mf"
        val entryClass = s"edu.cmu.cs.obsidian.generated_code.$mainName"
        val jarCmd: Array[String] =
            Array("jar", "-cmfe", manifest, outputJar, entryClass, "-C",
                  "out/generated_bytecode", "edu")
        val procJar = Runtime.getRuntime().exec(jarCmd)

        if (printJavacOutput) {
            val compilerOutput = procJar.getErrorStream()
            val untilEOF = new Scanner(compilerOutput).useDelimiter("\\A")
            val result = if (untilEOF.hasNext()) {
                untilEOF.next()
            } else {
                ""
            }

            print(result)
        }

        procJar.waitFor()
        procJar.exitValue()
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
        val javaClassOutputDir = "out/generated_bytecode"

        Files.createDirectories(Paths.get(protobufOutputDir))
        Files.createDirectories(Paths.get(javaJarOutputDir))
        Files.createDirectories(Paths.get(javaSrcOutputDir))
        Files.createDirectories(Paths.get(javaClassOutputDir))

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


                val lastSlash = filename.lastIndexOf("/")
                val sourceFilename = if (lastSlash < 0) filename else filename.substring(lastSlash + 1)

                val protobufOuterClassName = Util.protobufOuterClassNameForClass(sourceFilename.replace(".obs", ""))
                val initialOuterClassName = sourceFilename.replace(".obs", "") + "OuterClass"
                val protobufFilename = protobufOuterClassName + ".proto"

                val protobufPath = protobufOutputDir + "/" + protobufFilename

                val p : Protobuf = ProtobufGen.translateProgram(ast)
                p.build(new File(protobufOutputDir, protobufFilename), protobufOuterClassName)

                val javaModel = compile(ast, protobufOuterClassName)
                javaModel.build(new File(javaSrcOutputDir))

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

                val mainName = findMainContractName(ast)
                val javacExit = compileCode(printJavacOutput, mainName)
                if (printJavacOutput) {
                    println("javac exited with value " + javacExit)
                }
                if (javacExit == 0) {
                    val jarExit = makeJar(printJavacOutput, mainName)
                    if (printJavacOutput) {
                        println("jar exited with value " + jarExit)
                    }
                }

            } catch {
                case e: ParseException => println(e.message)
            }

        }
    }
}
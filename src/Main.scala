import java.nio.file.{Files, Paths, Path}
import java.io.File
import java.util.Scanner

import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.protobuf._
import edu.cmu.cs.obsidian.util._
import com.sun.codemodel.internal.JCodeModel

import scala.sys.process._

class ParseException (val message : String) extends Exception {

}


case class CompilerOptions (outputJar: Option[String],
                            inputFiles: List[String],
                            verbose: Boolean,
                            printTokens: Boolean,
                            printAST: Boolean)

object Main {


    val usage: String =
        """Usage: obsidian [options] file.obs
          |Options:
          |    --output-jar path/to/output.jar    outputs the jar at the given directory
          |    --verbose                          print error codes and messages for jar and javac
          |    --print-tokens                     print output of the lexer
          |    --print-ast                        print output of the parser
        """.stripMargin

    def parseOptions(args: List[String]): CompilerOptions = {
        var outputJar: Option[String] = None
        var inputFiles: List[String] = List.empty
        var verbose = false
        var printTokens = false
        var printAST = false

        def parseOptionsRec(remainingArgs: List[String]) : Unit = {
            remainingArgs match {
                case Nil => ()
                case "--verbose" :: tail =>
                    verbose = true
                    parseOptionsRec(tail)
                case "--print-tokens" :: tail =>
                    printTokens = true
                    parseOptionsRec(tail)
                case "--print-ast" :: tail =>
                    printAST = true
                    parseOptionsRec(tail)
                case "--output-jar" :: jarName :: tail =>
                    outputJar = Some(jarName)
                    parseOptionsRec(tail)
                case option :: tail =>
                    if (option.startsWith("--") || option.startsWith("-")) {
                        println("Unknown option " + option)
                        sys.exit(1)
                    }
                    else if (option.endsWith(".obs")) {
                        // This is an input file.
                        inputFiles = option :: inputFiles
                        parseOptionsRec(tail)
                    }
                    else {
                        println("Unknown argument " + option)
                        sys.exit(1)
                    }
            }
        }

        parseOptionsRec(args)

        if (inputFiles.isEmpty) {
            println("Must pass at least one file")
            sys.exit(1)
        }

        if (inputFiles.length > 1) {
            println("For now: contracts can only consist of a single file")
        }

        CompilerOptions(outputJar, inputFiles, verbose, printTokens, printAST)
    }

    def parseToAST(srcPath: String, options: CompilerOptions): Program = {
        val bufferedSource = scala.io.Source.fromFile(srcPath)
        val src = try bufferedSource.getLines() mkString "\n" finally bufferedSource.close()

        val tokens: Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => throw new ParseException(msg)
            case Right(ts) => ts
        }

        if (options.printTokens) {
            println("Tokens:")
            println()
            println(tokens)
            println()
        }

        val ast: Program = Parser.parseProgram(tokens) match {
            case Left(msg) => println(msg); throw new ParseException(msg)
            case Right(tree) => tree
        }

        ast
    }

    def findMainContractName(prog: Program): String = {
        for (aContract <- prog.contracts) {
            if (aContract.mod.contains(IsMain)) {
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
    def compileCode(
            printJavacOutput: Boolean,
            mainName: String,
            sourceDir: Path,
            compileTo: Path): Int  = {

        val sourcePath = sourceDir.toString
        val classPath = s"Obsidian Runtime/src/Runtime/:$sourcePath:lib/protobuf-java-3.2.0.jar"
        val srcFile = sourceDir.resolve(s"edu/cmu/cs/obsidian/generated_code/$mainName.java")
        val compileCmd: Array[String] = Array("javac", "-d", compileTo.toString,
                                                       "-classpath", classPath,
                                                        srcFile.toString)

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
    def makeJar(
            printJavacOutput: Boolean,
            mainName: String,
            outputJar: Path,
            bytecode: Path): Int  = {

        val manifest = s"Obsidian Runtime/protobuf_manifest.mf"
        val entryClass = s"edu.cmu.cs.obsidian.generated_code.$mainName"
        val jarCmd: Array[String] =
            Array("jar", "-cmfe", manifest, outputJar.toString, entryClass, "-C",
                  bytecode.toString, "edu")
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

    def recDelete(f: File): Unit = {
        if (f.isDirectory) {
            for (sub_f <- f.listFiles()) {
                recDelete(sub_f)
            }
        }

        f.delete()
    }

    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            println(usage)
            sys.exit(1)
        }

        val options = parseOptions(args.toList)

        val tmpDir = Files.createTempDirectory("obsidian")
        val tmpPath = tmpDir.toAbsolutePath

        val protobufDir = tmpPath.resolve("generated_protobuf")
        val srcDir = tmpPath.resolve("generated_java")
        val bytecodeDir = tmpPath.resolve("generated_bytecode")

        Files.createDirectories(protobufDir)
        Files.createDirectories(srcDir)
        Files.createDirectories(bytecodeDir)

        /* we just look at the first file because we don't have a module system yet */
        val filename = options.inputFiles.head

        try {
            val ast = parseToAST(filename, options)

            if (options.printAST) {
                println("AST")
                println()
                println(ast)
                println()
            }


            val lastSlash = filename.lastIndexOf("/")
            val sourceFilename = if (lastSlash < 0) filename else filename.substring(lastSlash + 1)

            val protobufOuterClassName = Util.protobufOuterClassNameForClass(sourceFilename.replace(".obs", ""))
            val initialOuterClassName = sourceFilename.replace(".obs", "") + "OuterClass"
            val protobufFilename = protobufOuterClassName + ".proto"

            // TODO
            /* We construct the protobuf file in the current working directory. This
             * construction cannot be done in the tmp directory because protoc doesn't
             * accept absolute paths. Not sure how to work around this... */
            val protoPath = Paths.get(protobufFilename)

            val p : Protobuf = ProtobufGen.translateProgram(ast)
            p.build(protoPath.toFile, protobufOuterClassName)

            val javaModel = compile(ast, protobufOuterClassName)
            javaModel.build(srcDir.toFile)

            // Invoke protoc to compile from protobuf to Java.
            val protocInvocation : String =
                "protoc --java_out=" + srcDir + " " + protobufFilename

            try {
                val exitCode = protocInvocation.!
                if (exitCode != 0) {
                    println("`" + protocInvocation + "` exited abnormally: " + exitCode)
                }
            } catch {
                case e: Throwable => println("Error running protoc: " + e)
            }

            Files.delete(protoPath)

            // invoke javac and make a jar from the result
            val mainName = findMainContractName(ast)
            val javacExit = compileCode(options.verbose, mainName, srcDir, bytecodeDir)
            if (options.verbose) {
                println("javac exited with value " + javacExit)
            }
            if (javacExit == 0) {
                val jarPath = Paths.get(options.outputJar.getOrElse(s"mainName.jar"))
                val jarExit = makeJar(options.verbose, mainName, jarPath, bytecodeDir)
                if (options.verbose) {
                    println("jar exited with value " + jarExit)
                }
            }

        } catch {
            case e: ParseException => println(e.message)
        }

        recDelete(tmpDir.toFile)
    }
}
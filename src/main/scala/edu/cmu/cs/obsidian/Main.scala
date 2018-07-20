package edu.cmu.cs.obsidian

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.util.Scanner

import com.helger.jcodemodel.JCodeModel
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.protobuf._
import edu.cmu.cs.obsidian.typecheck.{AstTransformer, Checker}
import edu.cmu.cs.obsidian.util._

import scala.sys.process._

case class CompilerOptions (outputPath: Option[String],
                            debugPath: Option[String],
                            inputFiles: List[String],
                            verbose: Boolean,
                            typeCheckerDebug: Boolean,
                            printTokens: Boolean,
                            printAST: Boolean,
                            buildClient: Boolean,
                            mockChaincode: Boolean)

object Main {

    val usage: String =
        """Usage: obsidian [options] file.obs
          |Options:
          |    --output-path path/to/dir    outputs jar and proto files at the given directory
          |    --dump-debug path/to/dir     save intermediate files at the given directory
          |    --verbose                    print error codes and messages for jar and javac
          |    --type-checker-debug         print stack trace of errors as they are logged
          |    --print-tokens               print output of the lexer
          |    --print-ast                  print output of the parser
          |    --build-client               build a client application rather than a server
          |    --hyperledger                use Hyperledger Fabric (rather than a local mock blockchain)
        """.stripMargin

    def parseOptions(args: List[String]): CompilerOptions = {
        var outputPath: Option[String] = None
        var debugPath: Option[String] = None
        var inputFiles: List[String] = List.empty
        var verbose = false
        var checkerDebug = false
        var printTokens = false
        var printAST = false
        var buildClient = false
        var mockChaincode = true

        def parseOptionsRec(remainingArgs: List[String]) : Unit = {
            remainingArgs match {
                case Nil => ()
                case "--verbose" :: tail =>
                    verbose = true
                    parseOptionsRec(tail)
                case "--type-checker-debug" :: tail =>
                    checkerDebug = true
                    parseOptionsRec(tail)
                case "--print-tokens" :: tail =>
                    printTokens = true
                    parseOptionsRec(tail)
                case "--print-ast" :: tail =>
                    printAST = true
                    parseOptionsRec(tail)
                case "--output-path" :: path :: tail =>
                    outputPath = Some(path)
                    parseOptionsRec(tail)
                case "--dump-debug" :: path :: tail =>
                    debugPath = Some(path)
                    parseOptionsRec(tail)
                case "--build-client" :: tail =>
                    buildClient = true
                    parseOptionsRec(tail)
                case "--hyperledger" :: tail =>
                    mockChaincode = false
                    parseOptionsRec(tail)
                case option :: tail =>
                    if (option.startsWith("--") || option.startsWith("-")) {
                        println("Unknown option " + option)
                        sys.exit(2)
                    }
                    else if (option.endsWith(".obs")) {
                        // This is an input file.
                        inputFiles = option :: inputFiles
                        parseOptionsRec(tail)
                    }
                    else {
                        println("Unknown argument " + option)
                        sys.exit(2)
                    }
            }
        }

        parseOptionsRec(args)

        if (inputFiles.isEmpty) {
            println("Must pass at least one file")
            sys.exit(2)
        }

        if (inputFiles.length > 1) {
            println("For now: contracts can only consist of a single file")
            sys.exit(2)
        }

        CompilerOptions(outputPath, debugPath, inputFiles, verbose, checkerDebug,
                        printTokens, printAST, buildClient, mockChaincode)
    }

    def findMainContractName(prog: Program): String = {
        val mainContractOption = findMainContract(prog)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        return mainContractOption.get.name
    }

    def translateServerASTToJava (ast: Program, protobufOuterClassName: String, mockChaincode: Boolean): JCodeModel = {
        val codeGen = new CodeGen(Server(), mockChaincode)
        codeGen.translateProgram(ast, protobufOuterClassName)
    }

    def findMainContract(ast: Program): Option[Contract] = {
        ast.contracts.find((c: Contract) => c.modifiers.contains(IsMain()))
    }

    def translateClientASTToJava (ast: Program, protobufOuterClassName: String, mockChaincode: Boolean): JCodeModel = {
        // Client programs must have a main contract.
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val codeGen = new CodeGen(Client(mainContractOption.get), mockChaincode)
        codeGen.translateProgram(ast, protobufOuterClassName)
    }

    /* returns the exit code of the javac process */
    def invokeJavac(
            printJavacOutput: Boolean,
            mainName: String,
            sourceDir: Path,
            compileTo: Path): Int  = {

        val sourcePath = sourceDir.toString
        val classPath =
            s"Obsidian Runtime/src/Runtime/:$sourcePath:lib/protobuf-java-3.5.1.jar:lib/json-20160810.jar:lib/shim-client-1.0.jar"
        val srcFile = sourceDir.resolve(s"edu/cmu/cs/obsidian/generated_code/$mainName.java")
        val compileCmd: Array[String] = Array("javac", "-d", compileTo.toString,
                                                       "-classpath", classPath,
                                                        srcFile.toString)

        val proc: java.lang.Process = Runtime.getRuntime().exec(compileCmd)
        val compilerOutput = proc.getErrorStream()
        val untilEOF = new Scanner(compilerOutput).useDelimiter("\\A")
        val result = if (untilEOF.hasNext()) {
            untilEOF.next()
        } else {
            ""
        }
        print(result)

        proc.waitFor()
        proc.exitValue()
    }

    /* returns the exit code of the jar process */
    def makeJar(
            printJavacOutput: Boolean,
            mainName: String,
            outputJar: Path,
            bytecode: Path): Int  = {

        val manifest = s"Obsidian Runtime/manifest.mf"
        val entryClass = s"edu.cmu.cs.obsidian.generated_code.$mainName"
        val jarCmd: Array[String] =
            Array("jar", "-cvfme", mainName+".jar", manifest, entryClass, "-C", bytecode.toString, "edu")
        val procJar = Runtime.getRuntime().exec(jarCmd)
        val compilerOutput = procJar.getErrorStream()
        val untilEOF = new Scanner(compilerOutput).useDelimiter("\\A")
        val result = if (untilEOF.hasNext()) {
            untilEOF.next()
        } else {
            ""
        }
        print(result)

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


    private def protobufOuterClassNameForClass(className: String): String = {
        className.substring(0, 1).toUpperCase(java.util.Locale.US) + className.substring(1) + "OuterClass"
    }

    // For input foo.obs, we generate foo.proto, from which we generate FooOuterClass.java.
    //    We also generate a jar at the specified directory, containing the generated classes.
    // The compiler returns an exit status of 0 if everything went well, 1 if there was an error
    //    compiling the contract, or 2 if it failed to parse the command-line options.
    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            println(usage)
            sys.exit(0)
        }
        if (compileProgram(args)) {
            sys.exit(0)
        } else {
            sys.exit(1)
        }
    }
    def compileProgram(args: Array[String]): Boolean = {
        val options = parseOptions(args.toList)

        val tmpPath: Path = options.debugPath match {
            case Some(p) =>
                val path = Paths.get(p)
                /* create the dir if it doesn't exist */
                Files.createDirectories(path)
                path.toAbsolutePath
            case None => Files.createTempDirectory("obsidian").toAbsolutePath
        }

        val shouldDelete = options.debugPath.isEmpty

        val srcDir = tmpPath.resolve("generated_java")
        val bytecodeDir = tmpPath.resolve("generated_bytecode")

        /* if an output path is specified, use it; otherwise, use working directory */
        val outputPath = options.outputPath match {
            case Some(p) =>
                val path = Paths.get(p)
                /* create the dir if it doesn't exist */
                Files.createDirectories(path)
                path
            case None => Paths.get(".")
        }


        Files.createDirectories(srcDir)
        Files.createDirectories(bytecodeDir)

        /* we just look at the first file because we don't have a module system yet */
        val filename = options.inputFiles.head

        try {
            val ast = Parser.parseFileAtPath(filename, options.printTokens)

            if (options.printAST) {
                println("AST:")
                println(ast)
                println()
            }

            val importsProcessedAst = ImportProcessor.processImports(filename, ast)
            val fieldsLiftedAst = StateFieldTransformer.transformProgram(importsProcessedAst)

            val table = new SymbolTable(fieldsLiftedAst)
            val (globalTable: SymbolTable, transformErrors) = AstTransformer.transformProgram(table)

            if (options.printAST) {
                println("Transformed AST:")
                println(globalTable.ast)
                println()
            }

            val checker = new Checker(globalTable, options.typeCheckerDebug)
            val typecheckingErrors = checker.checkProgram()

            val allSortedErrors = (transformErrors ++ typecheckingErrors).sorted

            for (error <- allSortedErrors) {
                error.printMessage()
            }

            if (!allSortedErrors.isEmpty) {
                return false
            }

            val lastSlash = filename.lastIndexOf("/")
            val sourceFilename = if (lastSlash < 0) filename else filename.substring(lastSlash + 1)

            val protobufOuterClassName = Util.protobufOuterClassNameForFilename(sourceFilename)

            val javaModel = if (options.buildClient) translateClientASTToJava(globalTable.ast, protobufOuterClassName, options.mockChaincode)
            else translateServerASTToJava(globalTable.ast, protobufOuterClassName, options.mockChaincode)
            javaModel.build(srcDir.toFile)

            val protobufs: Seq[(Protobuf, String)] = ProtobufGen.translateProgram(globalTable.ast, sourceFilename)

            // Each import results in a .proto file, which needs to be compiled.
            for (p <- protobufs) {
                val protobuf = p._1
                val filename = p._2
                val protobufOuterClassName = Util.protobufOuterClassNameForFilename(filename)
                val protobufFilename = protobufOuterClassName + ".proto"

                val protobufPath = outputPath.resolve(protobufFilename)

                protobuf.build(protobufPath.toFile, protobufOuterClassName)


                // Invoke protoc to compile from protobuf to Java.
                val protocInvocation: String =
                    "protoc --java_out=" + srcDir + " " + protobufPath.toString

                try {
                    val exitCode = protocInvocation.!
                    if (exitCode != 0) {
                        println("`" + protocInvocation + "` exited abnormally: " + exitCode)
                        return false
                    }
                } catch {
                    case e: Throwable => println("Error running protoc: " + e)
                }
            }

            val mainName = findMainContractName(globalTable.ast)

            if (options.mockChaincode) {
                // Build mock client
                // invoke javac and make a jar from the result
                val javacExit = invokeJavac(options.verbose, mainName, srcDir, bytecodeDir)
                if (options.verbose) {
                    println("javac exited with value " + javacExit)
                }
                if (javacExit == 0) {
                    val jarPath = outputPath.resolve(s"$mainName.jar")
                    val jarExit = makeJar(options.verbose, mainName, jarPath, bytecodeDir)
                    if (options.verbose) {
                        println("jar exited with value " + jarExit)
                    }
                }
                else
                    return false
            }
            else {
                // Build Hyperledger
                val gradleInvoke = if (options.buildClient) {
                    s"gradle build -b buildscript/build-hyperledger-client.gradle -Pmain=$mainName -PcodeDirectory=$tmpPath/generated_java"
                }
                else {
                    s"gradle build -b buildscript/build-chaincode.gradle -Pmain=$mainName -PcodeDirectory=$tmpPath/generated_java"
                }

                // invoke gradle buildscript to produce a jar file
                val gradleExit = gradleInvoke.!
                if (gradleExit != 0) {
                    println("`" + gradleInvoke + "` exited abnormally: " + gradleExit)
                    return false
                }

                if (options.verbose) {
                    println("gradle exited with value " + gradleExit)
                }

            }

        } catch {
            case e:
                Parser.ParseException => println (e.message);
                return false
        }

        if (shouldDelete) {
            recDelete(tmpPath.toFile)
        }
        return true
    }
}

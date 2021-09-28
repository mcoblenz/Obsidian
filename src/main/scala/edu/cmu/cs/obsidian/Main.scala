package edu.cmu.cs.obsidian

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.Scanner

import org.apache.commons.io.FileUtils

import scala.collection.mutable.HashSet
import com.helger.jcodemodel.JCodeModel
import com.helger.jcodemodel.writer.JCMWriter
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.protobuf._
import edu.cmu.cs.obsidian.typecheck.{Checker, DuplicateContractError, ErrorRecord, InferTypes, StateNameValidator}
import edu.cmu.cs.obsidian.util._

object Target extends Enumeration {
    type Target = Value
    val fabric, yul = Value
}

case class CompilerOptions (outputPath: Option[String],
                            debugPath: Option[String],
                            inputFiles: List[String],
                            verbose: Boolean,
                            typeCheckerDebug: Boolean,
                            printTokens: Boolean,
                            printAST: Boolean,
                            buildClient: Boolean,
                            target: Target.Target,
                            includePaths: List[Path])

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
          |    --yul                        outputs yul code
          |    -L <dir>                     resolve import statements using <dir>
        """.stripMargin

    def parseOptions(args: List[String]): CompilerOptions = {
        var outputPath: Option[String] = None
        var debugPath: Option[String] = None
        var inputFiles: List[String] = List.empty
        var includePaths: List[Path] = List.empty
        var verbose = false
        var checkerDebug = false
        var printTokens = false
        var printAST = false
        var buildClient = false
        var target: Target.Target = Target.fabric

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
                case "--yul" :: tail =>
                    target = Target.yul
                    parseOptionsRec(tail)

                case "-L" :: dir :: tail =>
                    val path = Paths.get(dir)
                    if (Files.exists(path) && Files.isDirectory(path)) {
                        includePaths = path :: includePaths
                        parseOptionsRec(tail)
                    }
                    else {
                        println("import path directory '" + dir + "' does not exist or is not a directory")
                        sys.exit(2)
                    }

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

        // We want to check include paths in the order they are specified on
        // the command line, so we reverse them to get them in the right order
        CompilerOptions(outputPath, debugPath, inputFiles, verbose, checkerDebug,
                        printTokens, printAST, buildClient, target, includePaths.reverse)
    }

    def findMainContractName(prog: Program): String = {
        val mainContractOption = findMainContract(prog)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        return mainContractOption.get.name
    }



    /* match on c to get either javaFFIcontract or ObdisianFFIContract */
    def findMainContract(ast: Program): Option[Contract] = {
        ast.contracts.find((c: Contract) => c match {
            case c: ObsidianContractImpl => c.modifiers.contains(IsMain())
            case c: JavaFFIContractImpl => false
        })
    }

    def recDelete(f: File): Unit = {
        if (f.isDirectory) {
            for (sub_f <- f.listFiles()) {
                recDelete(sub_f)
            }
        }

        f.delete()
    }

    def compilerPath(): Path = {
        val compilerDir = System.getenv("OBSIDIAN_COMPILER_DIR")

        if (compilerDir != null) {
            Paths.get(compilerDir).toAbsolutePath
        } else {
            Paths.get("").toAbsolutePath
        }
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
        val protoDir = tmpPath.resolve("generated_proto")

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
        Files.createDirectories(protoDir)

        /* we just look at the first file because we don't have a module system yet */
        val filename = options.inputFiles.head

        try {
            val ast = Parser.parseFileAtPath(filename, new FileInputStream(filename), options.printTokens)

            if (options.printAST) {
                println("AST:")
                println(ast)
                println()
            }

            val (importsProcessedAst, importErrors) = ImportProcessor.processImports(filename, options.includePaths, ast)
            val fieldsLiftedAst = StateFieldTransformer.transformProgram(importsProcessedAst)

            val set = new HashSet[String]
            var duplicateErrors = Seq.empty[ErrorRecord]
            for (contract <- fieldsLiftedAst.contracts) {
                if (set.contains(contract.name)) {
                    duplicateErrors = duplicateErrors :+ ErrorRecord(DuplicateContractError(contract.name), contract.loc, contract.sourcePath)
                }
                set.add(contract.name)
            }

            val table = new SymbolTable(fieldsLiftedAst)
            val (transformedTable: SymbolTable, transformErrors) = StateNameValidator.transformProgram(table)

            //val inferTypes = new InferTypes(transformedTable)
            //val inferredTypesProgram = inferTypes.inferTypesInProgram()
            // TODO: dispense with unnecessary symbol table re-creation
//            val inferredTable = new SymbolTable(transformedTable.ast)

            val checker = new Checker(transformedTable, options.typeCheckerDebug)
            val (typecheckingErrors, checkedTable) = checker.checkProgram()

            // todo: after the above call, transformedTable.contractLookup.size = 3 on a program with two contracts
            //  because a contract called Contract with nothing in it gets added. I think this is an error; it doesn't
            //  appear in the program text and we have to special case to ignore it later in Yul generation. I don't
            //  know what role it might play in other parts of the compiler. but this is where it first appears,
            //  and therefore near where to remove it.
            
            val allSortedErrors = (duplicateErrors ++ importErrors ++ transformErrors ++ typecheckingErrors).sorted

            if (!allSortedErrors.isEmpty) {
                val errorCount = allSortedErrors.size
                println(s"Found $errorCount errors:")
            }
            for (error <- allSortedErrors) {
                error.printMessage()
            }

            if (!allSortedErrors.isEmpty) {
                return false
            }

            if (options.target == Target.fabric) {
                if (!CodeGenJava.gen(filename, srcDir, outputPath, protoDir, options,checkedTable,transformedTable)){
                    return false
                }
            }

            if (options.target == Target.yul) {
                CodeGenYul.gen(filename, srcDir, outputPath, protoDir, options,checkedTable,transformedTable)
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

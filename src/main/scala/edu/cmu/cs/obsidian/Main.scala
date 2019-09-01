package edu.cmu.cs.obsidian

import java.io.{File, FileInputStream}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.Scanner

import org.apache.commons.io.FileUtils

import scala.collection.mutable.HashSet
import com.helger.jcodemodel.JCodeModel
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.protobuf._
import edu.cmu.cs.obsidian.typecheck.{Checker, DuplicateContractError, ErrorRecord, InferTypes, StateNameValidator}
import edu.cmu.cs.obsidian.util._

import scala.sys.process._

case class CompilerOptions (outputPath: Option[String],
                            debugPath: Option[String],
                            inputFiles: List[String],
                            verbose: Boolean,
                            typeCheckerDebug: Boolean,
                            printTokens: Boolean,
                            printAST: Boolean,
                            buildClient: Boolean)

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
                        printTokens, printAST, buildClient)
    }

    def findMainContractName(prog: Program): String = {
        val mainContractOption = findMainContract(prog)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        return mainContractOption.get.name
    }

    def translateServerASTToJava (ast: Program, protobufOuterClassName: String, table: SymbolTable): Either[String, JCodeModel] = {
        // Server must have a main contract.
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val codeGen = new CodeGen(Server(mainContractOption.get), table)
        codeGen.translateProgram(ast, protobufOuterClassName)
    }

    /* match on c to get either javaFFIcontract or ObdisianFFIContract */
    def findMainContract(ast: Program): Option[Contract] = {
        ast.contracts.find((c: Contract) => c match {
            case c: ObsidianContractImpl => c.modifiers.contains(IsMain())
            case c: JavaFFIContractImpl => false
        })
    }

    def translateClientASTToJava (ast: Program, protobufOuterClassName: String, table: SymbolTable): Either[String, JCodeModel] = {
        // Client programs must have a main contract.
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val codeGen = new CodeGen(Client(mainContractOption.get), table)
        codeGen.translateProgram(ast, protobufOuterClassName)
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

    def generateFabricCode(mainName: String, outputPath: Option[String], srcDir: Path): Unit = {
        try {
            //what we need to do now is move the .java class and the outerclass to a different folder
            /* if an output path is specified, use it; otherwise, use working directory */
            val path = outputPath match {
                case Some(p) =>
                    Paths.get(p + mainName)
                case None =>
                    Paths.get(mainName)
            }
            /* create the dir if it doesn't exist */
            Files.createDirectories(path)

            //copy the content of the fabric/java/ folder into a folder with the class name
            //have to add the trailing separator to avoid copying the java directory too
            
            
            val fabricPath = compilerPath().resolve("fabric").resolve("java")
            val buildPath = fabricPath.resolve("build.gradle")
            val settingsPath = fabricPath.resolve("settings.gradle")
            val srcPath = fabricPath.resolve("src")

            Files.copy(buildPath, path.resolve("build.gradle"), StandardCopyOption.REPLACE_EXISTING)
            Files.copy(settingsPath, path.resolve("settings.gradle"), StandardCopyOption.REPLACE_EXISTING)
            FileUtils.copyDirectory(srcPath.toFile, path.resolve("src").toFile)

            val tmpGeneratedCodePath = srcDir.resolve(Paths.get("org", "hyperledger", "fabric", "example"))
            val javaTargetLocation = Paths.get(path.toString, "src", "main", "java", "org", "hyperledger", "fabric", "example")

            FileUtils.copyDirectory(tmpGeneratedCodePath.toFile, javaTargetLocation.toFile)

            //place the correct class name in the build.gradle
            val gradlePath = Paths.get(path.toString, "build.gradle")
            val replaceClassNameInGradleBuild: String =
                "sed -i.backup s/{{CLASS_NAME}}/" + mainName + "/g " + gradlePath.toString

            //sed automatically creates a backup of the original file, has to be deleted
            val gradleBackupPath = Paths.get(path.toString, "build.gradle.backup")

            replaceClassNameInGradleBuild.!
            new File(gradleBackupPath.toString).delete()
            println("Successfully generated Fabric chaincode at " + path)
        } catch {
            case e: Throwable => println("Error generating Fabric code: " + e)
        }
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

            val (importsProcessedAst, importErrors) = ImportProcessor.processImports(filename, ast)
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

            val allSortedErrors = (duplicateErrors ++ importErrors ++ transformErrors ++ typecheckingErrors)//.sorted

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

            val lastSlash = filename.lastIndexOf(File.separator)
            val sourceFilename = if (lastSlash < 0) filename else filename.substring(lastSlash + 1)

            val protobufOuterClassName = Util.protobufOuterClassNameForFilename(sourceFilename)

            val errorOrJavaModel =
                if (options.buildClient) {
                    translateClientASTToJava(checkedTable.ast, protobufOuterClassName, transformedTable)
                } else {
                    translateServerASTToJava(checkedTable.ast, protobufOuterClassName, transformedTable)
                }

            errorOrJavaModel match {
                case Left(errorMessage) => {
                    println(errorMessage)
                    return false
                }
                case Right(javaModel) =>
                    javaModel.build(srcDir.toFile, srcDir.toFile, null)
            }


            val mainName = findMainContractName(checkedTable.ast)

            val protobufs: Seq[(Protobuf, String)] = ProtobufGen.translateProgram(checkedTable.ast, sourceFilename)
            val protobufOutputPath = outputPath.resolve("protos")

            // Each import results in a .proto file, which needs to be compiled.
            for (p <- protobufs) {
                val protobuf = p._1
                val filename = p._2
                val protobufOuterClassName = Util.protobufOuterClassNameForFilename(filename)
                val protobufFilename = protobufOuterClassName + ".proto"

                val protobufPath = protoDir.resolve(protobufFilename)

                protobuf.build(protobufPath.toFile, protobufOuterClassName)

                // Invoke protoc to compile from protobuf to Java.
                val protoPath = protobufPath.getParent.toString
                val protocInvocation: String =
                    "protoc --java_out=" + srcDir + " -I=" + protoPath + " " + protobufPath.toString

                try {
                    val exitCode = protocInvocation.!
                    if (exitCode != 0) {
                        println("`" + protocInvocation + "` exited abnormally: " + exitCode)
                        return false
                    }
                } catch {
                    case e: Throwable => println("Error running protoc: " + e)
                }

                // Copy the proto file to the output path for use by clients.

                val outputPath = options.outputPath match {
                    case Some(p) =>
                        Paths.get(p + mainName)
                    case None =>
                        Paths.get(mainName)
                }
                val temp = protobufOutputPath.toFile
                if (temp.exists()) {
                    FileUtils.deleteDirectory(temp)
                }
                Files.createDirectories(protobufOutputPath)

                val sourceFile = (Paths.get(s"$protobufPath"))
                val destFile = Paths.get(s"$protobufOutputPath")
                val newDest = Paths.get(destFile.toString() + File.separator + sourceFile.getFileName())
                Files.copy(sourceFile, newDest, StandardCopyOption.REPLACE_EXISTING)

            }

            // Compile the wrapper protobuf file.
            val wrapperProtoPath = compilerPath().resolve("resources")
                .resolve("protos")
            // Invoke protoc to compile from protobuf to Java.
            val wrapperProtocInvocation: String =
                "protoc --java_out=" + srcDir + " -I=" + wrapperProtoPath + " InterfaceImplementerWrapper.proto"

            try {
                val exitCode = wrapperProtocInvocation.!
                if (exitCode != 0) {
                    println("`" + wrapperProtocInvocation + "` exited abnormally: " + exitCode)
                    return false
                }
            } catch {
                case e: Throwable => println("Error running protoc: " + e)
            }

            // Copy the InterfaceImplementerWrapper.proto file.
            val wrapperProtoPathSrc = compilerPath().resolve("resources")
                .resolve("protos")
                .resolve("InterfaceImplementerWrapper.proto")
            val wrapperProtoPathDest = protobufOutputPath.resolve("InterfaceImplementerWrapper.proto")
            Files.copy(wrapperProtoPathSrc, wrapperProtoPathDest, StandardCopyOption.REPLACE_EXISTING)


            generateFabricCode(mainName, options.outputPath, srcDir)
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

import java.nio.file.{Files, Path, Paths}
import java.io.File
import java.util
import java.util.Scanner

import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.codegen._
import edu.cmu.cs.obsidian.protobuf._
import edu.cmu.cs.obsidian.util._
import com.sun.codemodel.internal.JCodeModel

import scala.sys.process._



case class CompilerOptions (outputPath: Option[String],
                            debugPath: Option[String],
                            inputFiles: List[String],
                            verbose: Boolean,
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
          |    --print-tokens               print output of the lexer
          |    --print-ast                  print output of the parser
          |    --build-client               build a client application rather than a server
        """.stripMargin

    def parseOptions(args: List[String]): CompilerOptions = {
        var outputPath: Option[String] = None
        var debugPath: Option[String] = None
        var inputFiles: List[String] = List.empty
        var verbose = false
        var printTokens = false
        var printAST = false
        var buildClient = false

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

        CompilerOptions(outputPath, debugPath, inputFiles, verbose, printTokens, printAST, buildClient)
    }

    def findMainContractName(prog: Program): String = {
        for (aContract <- prog.contracts) {
            if (aContract.mod.contains(IsMain)) {
                return aContract.name
            }
        }
        throw new RuntimeException("No main contract found")
    }

    def translateServerASTToJava (ast: Program, protobufOuterClassName: String): JCodeModel = {
        val codeGen = new CodeGen(Server())
        codeGen.translateProgram(ast, protobufOuterClassName)
    }

    def translateClientASTToJava (ast: Program, protobufOuterClassName: String): JCodeModel = {
        val codeGen = new CodeGen(Client())
        codeGen.translateProgram(ast, protobufOuterClassName)
    }

    def classPathForGeneratedSource (sourceDirectoryPath: Path) : Seq[String] =  {
        List("Obsidian Runtime/src/Runtime/",
            s"$sourceDirectoryPath",
            "lib/protobuf-java-3.2.0.jar",
            "lib/json-20160810.jar")
    }

    /* returns the exit code of the javac process */
    def invokeJavac(
            printJavacOutput: Boolean,
            sourceFilePath: Path,
            sourceDir: Path,
            compileTo: Path): Int  = {

        val classPath = classPathForGeneratedSource(sourceDir).reduce((a: String, b: String) => a + ":" + b)
        val compileCmd: Array[String] = Array("javac", "-d", compileTo.toString,
                                                       "-classpath", classPath,
                                                        sourceFilePath.toString)

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


    private def protobufOuterClassNameForClass(className: String): String = {
        className.substring(0, 1).toUpperCase(java.util.Locale.US) + className.substring(1) + "OuterClass"
    }

    // For input foo.obs, we generate foo.proto, from which we generate FooOuterClass.java.
    //    We also generate a jar at the specified directory, containing the generated classes.
    def main(args: Array[String]): Unit = {
        if (args.length == 0) {
            println(usage)
            sys.exit(1)
        }

        val options = parseOptions(args.toList)

        val tmpPath: Path = options.debugPath match {
            case Some(p) =>
                val path = Paths.get(p)
                /* create the dir if it doesn't exist */
                Files.createDirectories(path)
                path
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
                println("AST")
                println()
                println(ast)
                println()
            }


            val lastSlash = filename.lastIndexOf("/")
            val sourceFilename = if (lastSlash < 0) filename else filename.substring(lastSlash + 1)

            val protobufOuterClassName = Util.protobufOuterClassNameForFilename(sourceFilename)

            val javaModel = if (options.buildClient) translateClientASTToJava(ast, protobufOuterClassName)
                                else translateServerASTToJava(ast, protobufOuterClassName)
            javaModel.build(srcDir.toFile)

            val protobufs: Seq[(Protobuf, String)] = ProtobufGen.translateProgram(ast, sourceFilename)

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
                    }
                } catch {
                    case e: Throwable => println("Error running protoc: " + e)
                }
            }


            // invoke javac and make a jar from the result
            val mainName = findMainContractName(ast)

            val sourceFilePath = srcDir.resolve(s"edu/cmu/cs/obsidian/generated_code/$mainName.java")
            val javacExit = invokeJavac(options.verbose, sourceFilePath, srcDir, bytecodeDir)
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

            checkJavaFileWithKey(srcDir.toFile, classPathForGeneratedSource(srcDir))

        } catch {
            case e: Parser.ParseException => println(e.message)
        }

        if (shouldDelete) {
            recDelete(tmpPath.toFile)
        }
    }

    private def checkJavaFileWithKey(file: File, classpath: Seq[String]): Unit = {
        import de.uka.ilkd.key.control.KeYEnvironment
        import de.uka.ilkd.key.java.abstraction.KeYJavaType
        import de.uka.ilkd.key.logic.op.IObserverFunction
        import de.uka.ilkd.key.proof.Proof
        import de.uka.ilkd.key.proof.init.ProofInputException
        import de.uka.ilkd.key.proof.io.ProblemLoaderException
        import de.uka.ilkd.key.settings.ChoiceSettings
        import de.uka.ilkd.key.settings.ProofSettings
        import de.uka.ilkd.key.strategy.StrategyProperties
        import de.uka.ilkd.key.util.KeYTypeUtil
        import de.uka.ilkd.key.util.MiscTools
        import de.uka.ilkd.key.speclang.Contract
        import scala.collection.JavaConverters._

        // Path to the source code folder/file or to a *.proof file
        val fileClassPath = classpath.map ((s: String) => Paths.get(s).toFile()).asJava
        // Optionally: Additional specifications for API classes
        val bootClassPath = null
        // Optionally: Different default specifications for Java API
        val includes = null // Optionally: Additional includes to consider
        try { // Ensure that Taclets are parsed
            if (!ProofSettings.isChoiceSettingInitialised) {
                val env = KeYEnvironment.load(file, fileClassPath, bootClassPath, includes)
                env.dispose()
            }
            // Set Taclet options
            val choiceSettings = ProofSettings.DEFAULT_SETTINGS.getChoiceSettings
            val oldSettings = choiceSettings.getDefaultChoices
            val newSettings = new util.HashMap[String, String](oldSettings)
            newSettings.putAll(MiscTools.getDefaultTacletOptions)
            choiceSettings.setDefaultChoices(newSettings)
            // Load source code
            val env = KeYEnvironment.load(file, fileClassPath, bootClassPath, includes) // env.getLoadedProof() returns performed proof if a *.proof file is loaded
            try { // List all specifications of all types in the source location (not classPaths and bootClassPath)
                val proofContracts = new util.LinkedList[Contract]()
                val kjts = env.getJavaInfo.getAllKeYJavaTypes
                import scala.collection.JavaConversions._
                for (javaType <- kjts) {
                    if (!KeYTypeUtil.isLibraryClass(javaType)) {
                        val targets = env.getSpecificationRepository.getContractTargets(javaType)
                        for (target <- targets) {
                            val contracts = env.getSpecificationRepository.getContracts(javaType, target)
                            for (contract <- contracts) {
                                proofContracts.add(contract)
                            }
                        }
                    }
                }
                // Perform proofs
                import scala.collection.JavaConversions._
                for (contract <- proofContracts) {
                    var proof: Proof = null
                    try { // Create proof
                        proof = env.createProof(contract.createProofObl(env.getInitConfig, contract))
                        // Set proof strategy options
                        val sp = proof.getSettings.getStrategySettings.getActiveStrategyProperties
                        sp.setProperty(StrategyProperties.METHOD_OPTIONS_KEY, StrategyProperties.METHOD_CONTRACT)
                        sp.setProperty(StrategyProperties.DEP_OPTIONS_KEY, StrategyProperties.DEP_ON)
                        sp.setProperty(StrategyProperties.QUERY_OPTIONS_KEY, StrategyProperties.QUERY_ON)
                        sp.setProperty(StrategyProperties.NON_LIN_ARITH_OPTIONS_KEY, StrategyProperties.NON_LIN_ARITH_DEF_OPS)
                        sp.setProperty(StrategyProperties.STOPMODE_OPTIONS_KEY, StrategyProperties.STOPMODE_NONCLOSE)
                        proof.getSettings.getStrategySettings.setActiveStrategyProperties(sp)
                        // Make sure that the new options are used
                        val maxSteps = 10000
                        ProofSettings.DEFAULT_SETTINGS.getStrategySettings.setMaxSteps(maxSteps)
                        ProofSettings.DEFAULT_SETTINGS.getStrategySettings.setActiveStrategyProperties(sp)
                        proof.getSettings.getStrategySettings.setMaxSteps(maxSteps)
                        proof.setActiveStrategy(proof.getServices.getProfile.getDefaultStrategyFactory.create(proof, sp))
                        // Start auto mode
                        env.getUi.getProofControl.startAndWaitForAutoMode(proof)
                        // Show proof result
                        val closed = proof.openGoals.isEmpty
                        System.out.println("Contract '" + contract.getDisplayName + "' of " + contract.getTarget + " is " + (if (closed) "verified"
                        else "still open") + ".")
                    } catch {
                        case e: ProofInputException =>
                            System.out.println("Exception at '" + contract.getDisplayName + "' of " + contract.getTarget + ":")
                            e.printStackTrace
                    } finally if (proof != null) proof.dispose() // Ensure always that all instances of Proof are disposed
                }
            } finally env.dispose() // Ensure always that all instances of KeYEnvironment are disposed
        } catch {
            case e: ProblemLoaderException =>
                System.out.println("Exception at '" + file + "':")
                e.printStackTrace()
        }
    }

}
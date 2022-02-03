package edu.cmu.cs.obsidian.codegen

import java.io.File
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import com.helger.jcodemodel.JCodeModel
import com.helger.jcodemodel.writer.JCMWriter
import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.Main.{compilerPath, findMainContract, findMainContractName}
import edu.cmu.cs.obsidian.parser.{Program, SymbolTable}
import edu.cmu.cs.obsidian.protobuf.{Protobuf, ProtobufGen}
import edu.cmu.cs.obsidian.util.Util
import org.apache.commons.io.FileUtils
import scala.sys.process._

object CodeGenJava extends CodeGenerator{

    def translateServerASTToJava(ast: Program, protobufOuterClassName: String, table: SymbolTable): Either[String, JCodeModel] = {
        // Server must have a main contract.
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val codeGen = new CodeGen(Server(mainContractOption.get), table)
        codeGen.translateProgram(ast, protobufOuterClassName)
    }

    def translateClientASTToJava(ast: Program, protobufOuterClassName: String, table: SymbolTable): Either[String, JCodeModel] = {
        // Client programs must have a main contract.
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val codeGen = new CodeGen(Client(mainContractOption.get), table)
        return codeGen.translateProgram(ast, protobufOuterClassName)
    }

    def generateFabricCode(mainName: String, outputPath: Path, srcDir: Path): Unit = {
        try {
            //what we need to do now is move the .java class and the outerclass to a different folder
            /* create the dir if it doesn't exist */
            Files.createDirectories(outputPath)

            //copy the content of the fabric/java/ folder into a folder with the class name
            //have to add the trailing separator to avoid copying the java directory too

            val fabricPath = compilerPath().resolve("fabric").resolve("java")
            val buildPath = fabricPath.resolve("build.gradle")
            val settingsPath = fabricPath.resolve("settings.gradle")
            val srcPath = fabricPath.resolve("src")

            Files.copy(buildPath, outputPath.resolve("build.gradle"), StandardCopyOption.REPLACE_EXISTING)
            Files.copy(settingsPath, outputPath.resolve("settings.gradle"), StandardCopyOption.REPLACE_EXISTING)
            FileUtils.copyDirectory(srcPath.toFile, outputPath.resolve("src").toFile)

            val tmpGeneratedCodePath = srcDir.resolve(Paths.get("org", "hyperledger", "fabric", "example"))
            val javaTargetLocation = Paths.get(outputPath.toString, "src", "main", "java", "org", "hyperledger", "fabric", "example")

            FileUtils.copyDirectory(tmpGeneratedCodePath.toFile, javaTargetLocation.toFile)

            //place the correct class name in the build.gradle
            val gradlePath = Paths.get(outputPath.toString, "build.gradle")
            val replaceClassNameInGradleBuild: String =
                "sed -i.backup s/{{CLASS_NAME}}/" + mainName + "/g " + gradlePath.toString

            //sed automatically creates a backup of the original file, has to be deleted
            val gradleBackupPath = Paths.get(outputPath.toString, "build.gradle.backup")

            replaceClassNameInGradleBuild.!
            new File(gradleBackupPath.toString).delete()
            println("Successfully generated Fabric chaincode at " + outputPath.toAbsolutePath)
        } catch {
            case e: Throwable => println("Error generating Fabric code: " + e)
        }
    }

    def gen(filename: String, srcDir: Path, outputPath: Path, protoDir: Path,
            options: CompilerOptions, checkedTable: SymbolTable, transformedTable: SymbolTable): Boolean = {

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
                val writer = new JCMWriter(javaModel)
                writer.build(srcDir.toFile)
        }


        val mainName = findMainContractName(checkedTable.ast)

        val protobufs: Seq[(Protobuf, String)] = ProtobufGen.translateProgram(checkedTable.ast, sourceFilename)
        val protobufOutputPath = outputPath.resolve(mainName).resolve("protos")

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

        val finalOutputPath = options.outputPath match {
            case Some(p) =>
                Paths.get(p).resolve(mainName)
            case None =>
                Paths.get(mainName)
        }

        generateFabricCode(mainName, finalOutputPath, srcDir)
        if (options.buildClient) {
            // Run Gradle shadowJar to make client.jar.
            val buildFilePath = finalOutputPath.resolve("build.gradle")
            val gradleInvocation = s"gradle shadowJar -b ${buildFilePath.toAbsolutePath}"
            try {
                val exitCode = gradleInvocation.!
                if (exitCode != 0) {
                    println("`" + gradleInvocation + "` exited abnormally: " + exitCode)
                    return false
                }
            }
            catch {
                case e: Throwable => {
                    println("Error running gradle: " + e)
                    return false
                }
            }
        }
        return true
    }
}

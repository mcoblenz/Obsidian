package edu.cmu.cs.obsidian.parser

import java.nio.file.{Files, Path, Paths}

import com.google.common.base.{MoreObjects, Objects}
import edu.cmu.cs.obsidian.typecheck.{ErrorRecord, ImportError}

import scala.collection.mutable
import scala.util.parsing.input.Position

object ImportProcessor {
    val filesAlreadyProcessed = new mutable.HashSet[String]

    class ImportException (val message : String) extends Exception {}

    def compilerPath(): Path =
        List("TRAVIS_BUILD_DIR","OBSIDIAN_COMPILER_DIR")
            .map(System.getenv)
            .find(_ != null)
            .map(Paths.get(_).toAbsolutePath)
            .getOrElse(Paths.get("").toAbsolutePath)

    def standardLibraryPath(): Path = {
        val envPathStr = System.getenv("OBSIDIAN_STDLIB")
        if (envPathStr == null) {
            compilerPath().resolve("Obsidian_Runtime/src/main/java/Runtime/edu/cmu/cs/obsidian/stdlib")
        } else {
            Paths.get(envPathStr).toAbsolutePath
        }
    }

    private def relativePath(importingFile: String): Path =
        Paths.get(importingFile).toAbsolutePath.getParent

    private def searchPaths(importingFile: String): Seq[Path] =
        List(relativePath(importingFile), standardLibraryPath(), compilerPath())

    def searchImportFile(importingFile: String, fileName: String): Option[Path] =
        searchPaths(importingFile)
            .map(_.resolve(fileName))
            .find(Files.exists(_))

    def processImports(inFile: String, ast: Program): (Program, Seq[ErrorRecord]) = {
        if (filesAlreadyProcessed.contains(inFile)) {
            (ast, Seq.empty)
        } else {
            filesAlreadyProcessed.add(inFile)

            var allContracts = ast.contracts
            var importErrors: Seq[ErrorRecord] = Seq.empty[ErrorRecord]

            ast.imports.foreach(i => {
                val contracts = processImport(i.name, filesAlreadyProcessed, i.loc, inFile)

                contracts match {
                    case Left(error) =>
                        val newErrors = importErrors :+ error
                        importErrors = newErrors
                    case Right(cs) => allContracts = cs ++ allContracts
                }
            })

            (Program(Seq.empty, allContracts).setLoc(ast), importErrors)
        }
    }

    def processImport(importPath: String, seenFiles: mutable.HashSet[String], importPos: Position, importFilename: String) : Either[ErrorRecord, Seq[Contract]] = {
        searchImportFile(importFilename, importPath) match {
            case Some(path) => {
                val resolvedImportPath = path.toAbsolutePath.toString
                if (!seenFiles.contains(resolvedImportPath)) {
                    val importedProgram = Parser.parseFileAtPath(resolvedImportPath, printTokens = false)

                    var contracts = filterTags(importedProgram.contracts)

                    seenFiles.add(resolvedImportPath)

                    importedProgram.imports.foreach(i => {
                        processImport(i.name, seenFiles, i.loc, resolvedImportPath) match {
                            case Left(err) => return Left(err)
                            case Right(c) => contracts = c ++ contracts
                        }
                    })
                    Right(contracts)
                } else {
                    Right(Nil)
                }
            }
            case None =>
                Left(ErrorRecord(ImportError(s"Unable to read file: $importPath"), importPos, importFilename))
        }
    }

    // filter out IsMain modifier to ensure only one main contract in Client
    // add IsImport tag

    /*check this */
    def filterTags(contracts: Seq[Contract]): Seq[Contract] = {
        contracts.map(c => c match {
            case c: ObsidianContractImpl =>
                     val newMods = c.modifiers - IsMain() + IsImport()
                     val newC = ObsidianContractImpl(newMods, c.name, c.declarations, c.transitions, c.isInterface, c.sourcePath).setLoc(c)
                     newC
            case c: JavaFFIContractImpl => c
        })
    }
}

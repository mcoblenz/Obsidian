package edu.cmu.cs.obsidian.parser

import java.nio.file.{Files, Path, Paths}

import com.google.common.base.{MoreObjects, Objects}
import edu.cmu.cs.obsidian.typecheck.{ErrorRecord, ImportError}

import scala.collection.mutable
import scala.util.parsing.input.Position

object ImportProcessor {
    val filesAlreadyProcessed = new mutable.HashSet[String]

    class ImportException (val message : String) extends Exception {}

    private def standardLibraryPath(fileName: String): Path =
        Paths.get(MoreObjects.firstNonNull(System.getenv("OBSIDIAN_STDLIB"), "")).resolve(fileName)

    private def relativePath(importingFile: String, fileName: String): Path =
        Paths.get(importingFile).toAbsolutePath.getParent.resolve(fileName)

    private def searchPaths(importingFile: String, fileName: String): Seq[Path] =
        List(standardLibraryPath(fileName), relativePath(importingFile, fileName))

    def searchImportFile(importingFile: String, fileName: String): Option[Path] =
        searchPaths(importingFile, fileName).find(Files.exists(_))

    def processImports(inFile: String, ast: Program): (Program, Seq[ErrorRecord]) = {
        if (filesAlreadyProcessed.contains(inFile)) {
            (ast, Seq.empty)
        }
        else {
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

                val importedProgram = Parser.parseFileAtPath(resolvedImportPath, printTokens = false)

                var contracts = filterTags(importedProgram.contracts)

                seenFiles.add(importPath)

                importedProgram.imports.foreach(i => {
                    if (!seenFiles.contains(i.name)) {
                        val cs = processImport(i.name, seenFiles, i.loc, resolvedImportPath)

                        cs match {
                            case Left(err) => return Left(err)
                            case Right(c) => contracts = c ++ contracts

                        }
                    }
                })
                Right(contracts)
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

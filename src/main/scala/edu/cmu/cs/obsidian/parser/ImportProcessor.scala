package edu.cmu.cs.obsidian.parser

import java.io.{FileInputStream, InputStream}
import java.nio.file.{FileSystems, Files, Path, Paths}

import edu.cmu.cs.obsidian.Main
import edu.cmu.cs.obsidian.typecheck.{ErrorRecord, ImportError}

import scala.collection.mutable
import scala.util.parsing.input.Position

object ImportProcessor {
    val filesAlreadyProcessed = new mutable.HashSet[String]

    class ImportException (val message : String) extends Exception {}

    def resolvePathAndInputStream(path: Path, fileName: String): Option[(String, InputStream)] = {
        val filePath = path.resolve(fileName)
        if (Files.exists(filePath)) {
            Some((filePath.toAbsolutePath.toString, new FileInputStream(filePath.toFile)))
        } else {
            None
        }
    }

    private def relativePath(importingFile: String, fileName: String): Option[(String, InputStream)] =
        resolvePathAndInputStream(Paths.get(importingFile).toAbsolutePath.getParent, fileName)

    private def compilerPath(fileName: String): Option[(String, InputStream)] =
        resolvePathAndInputStream(Main.compilerPath(), fileName)

    def standardLibraryPath(fileName: String): Option[(String, InputStream)] = {
        val classLoader = Thread.currentThread().getContextClassLoader
        val resourcePath = "stdlib/" + fileName
        val resolvedUrl = classLoader.getResource(resourcePath)

        if (resolvedUrl != null) {
            val inputStream = classLoader.getResourceAsStream(resourcePath)
            Some((resolvedUrl.toString, inputStream))
        } else {
            None
        }
    }

    private def searchPaths(importingFile: String, fileName: String, includePaths: List[Path]): Option[(String, InputStream)] = {
        val searchFs = List(
            relativePath(importingFile, _: String),
            standardLibraryPath(_),
            compilerPath(_)) ++ includePaths.map(path => resolvePathAndInputStream(path, _: String))

        for (searchF <- searchFs) {
            searchF(fileName) match {
                case Some(x) => return Some(x)
                case None => ()
            }
        }

        None
    }

    // Requires that all paths in includePaths exist and are directories
    def processImports(inFile: String, includePaths: List[Path], ast: Program): (Program, Seq[ErrorRecord]) = {
        var importErrors: Seq[ErrorRecord] = Seq.empty[ErrorRecord]

        if (filesAlreadyProcessed.contains(inFile)) {
            (ast, Seq.empty)
        } else {
            filesAlreadyProcessed.add(inFile)

            var allContracts = ast.contracts

            ast.imports.foreach(i => {
                val contracts = processImport(i.name, filesAlreadyProcessed, i.loc, inFile, includePaths)

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

    def processImport(importPath: String, seenFiles: mutable.HashSet[String], importPos: Position, importFilename: String, includePaths: List[Path]) : Either[ErrorRecord, Seq[Contract]] = {
        searchPaths(importFilename, importPath, includePaths) match {
            case Some((resolvedImportPath, inputStream)) => {
                if (!seenFiles.contains(resolvedImportPath)) {
                    val importedProgram = Parser.parseFileAtPath(resolvedImportPath, inputStream, printTokens = false)

                    var contracts = filterTags(importedProgram.contracts)

                    seenFiles.add(resolvedImportPath)

                    importedProgram.imports.foreach(i => {
                        processImport(i.name, seenFiles, i.loc, resolvedImportPath, includePaths) match {
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
                val newC = ObsidianContractImpl(newMods, c.name, c.params, c.bound, c.declarations, c.transitions, c.isInterface, c.sourcePath).setLoc(c)
                newC
            case c: JavaFFIContractImpl => c
        })
    }
}

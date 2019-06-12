package edu.cmu.cs.obsidian.parser

import java.nio.file.{Files, Paths}

import edu.cmu.cs.obsidian.typecheck.{DuplicateContractError, ErrorRecord, ImportError}

import scala.collection.mutable
import scala.util.parsing.input.Position

object ImportProcessor {
    val filesAlreadyProcessed = new mutable.HashSet[String]

    class ImportException (val message : String) extends Exception {}

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
        if (Files.exists(Paths.get(importPath))) {
            val importedProgram = Parser.parseFileAtPath(importPath, printTokens = false)

            var contracts = filterTags(importedProgram.contracts)

            seenFiles.add(importPath)

            importedProgram.imports.foreach(i => {
                if (!seenFiles.contains(i.name)) {
                    val cs = processImport(i.name, seenFiles, i.loc, importPath)

                    cs match {
                        case Left(err) => return Left(err)
                        case Right(c) => contracts = c ++ contracts

                    }
                }
            })
            Right(contracts)
        }
        else {
            Left(ErrorRecord(ImportError(s"Unable to read file: $importPath"), importPos, importFilename))
        }
    }

    // filter out IsMain modifier to ensure only one main contract in Client
    // add IsImport tag
    def filterTags(contracts: Seq[Contract]): Seq[Contract] = {
        contracts.map(c => {
            val newMods = c.modifiers - IsMain() + IsImport()
            val newC = Contract(newMods, c.name, c.declarations, c.transitions, c.isInterface, c.sourcePath).setLoc(c)
            newC
        })
    }
}

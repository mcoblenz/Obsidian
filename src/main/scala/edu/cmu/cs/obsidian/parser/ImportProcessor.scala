package edu.cmu.cs.obsidian.parser

import java.nio.file.{Files, Paths}

import edu.cmu.cs.obsidian.typecheck.{ErrorRecord, ImportError}

object ImportProcessor {

    class ImportException (val message : String) extends Exception {}

    def processImports(inFile: String, ast: Program): (Program, Seq[ErrorRecord]) = {

        var allContracts = ast.contracts
        var importErrors: Seq[ErrorRecord] = Seq.empty[ErrorRecord]

        ast.imports.foreach(i => {
            val contracts = processImport(i.name, Seq(Import(inFile)), ast.contracts.map(_.name))

            contracts match {
                case Left(msg) =>
                    val newErrors = importErrors :+ ErrorRecord(ImportError(msg), i.loc, inFile)
                    importErrors = newErrors
                case Right(cs) => allContracts = cs ++ allContracts
            }
        })

        (Program(Seq.empty, allContracts).setLoc(ast), importErrors)
    }

    def processImport(importPath: String, seen: Seq[Import], contractNames: Seq[String]) : Either[String, Seq[Contract]] = {
        if (Files.exists(Paths.get(importPath))) {
            val importedProgram = Parser.parseFileAtPath(importPath, printTokens = false)

            var contracts = filterTags(importedProgram.contracts)

            importedProgram.contracts.foreach(c => {
                if (contractNames.contains(c.name)) {
                    return Left("Repeat contract " + c.name + " in " + importPath)
                }
            })

            val updatedSeen = seen :+ Import(importPath)
            val updatedContractNames = contractNames ++ importedProgram.contracts.map(_.name)

            importedProgram.imports.foreach(i => {
                if (seen.contains(i)) {
                    return Left("Cyclical import " + i + " from " + importPath)
                }

                val cs = processImport(i.name, updatedSeen, updatedContractNames)

                cs match {
                    case Left(err) => return Left(err)
                    case Right(c) => contracts = c ++ contracts

                }
            })
            Right(contracts)
        }
        else {
            Left(s"Unable to read file: $importPath")
        }
    }

    // filter out IsMain modifier to ensure only one main contract in Client
    // add IsImport tag
    def filterTags(contracts: Seq[Contract]): Seq[Contract] = {
        contracts.map(c => {
            val newMods = c.modifiers - IsMain() + IsImport()
            val newC = Contract(newMods, c.name, c.declarations, c.transitions, c.isInterface, c.sourcePath)
            newC
        })
    }
}

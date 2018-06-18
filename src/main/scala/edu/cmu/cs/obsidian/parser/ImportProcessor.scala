package edu.cmu.cs.obsidian.parser

object ImportProcessor {

    class ImportException (val message : String) extends Exception {}

    def processImports(name: String, ast: Program): Program = {

        var allContracts = ast.contracts

        ast.imports.foreach(i => {
            val contracts = processImport(i.name, Seq(Import(name)), ast.contracts.map(_.name))

            contracts match {
                case Left(msg) => throw new ImportException(msg)
                case Right(cs) => allContracts = cs ++ allContracts
            }
        })

        Program(Seq.empty, allContracts)
    }

    def processImport(imp: String, seen: Seq[Import], contractNames: Seq[String]) : Either[String, Seq[Contract]] = {
        val importedProgram = Parser.parseFileAtPath(imp, printTokens = false)

        var contracts = filterTags(importedProgram.contracts)

        importedProgram.contracts.foreach(c => {
            if (contractNames.contains(c.name)) {
                return Left("Repeat contract " + c.name + " in " + imp)
            }
        })

        val updatedSeen = seen :+ Import(imp)
        val updatedContractNames = contractNames ++ importedProgram.contracts.map(_.name)

        importedProgram.imports.foreach(i => {
            if (seen.contains(i)) {
                return Left("Cyclical import " + i + " from " + imp)
            }

            val cs = processImport(i.name, updatedSeen, updatedContractNames)

            cs match {
                case Left(err) => return Left(err)
                case Right(c) => contracts = c ++ contracts

            }
        })
        Right(contracts)
    }

    // filter out IsMain modifier to ensure only one main contract in Client
    // add IsImport tag
    def filterTags(contracts: Seq[Contract]): Seq[Contract] = {
        contracts.map(c => {
            val newMods = c.modifiers - IsMain() + IsImport()
            val newC = Contract(newMods, c.name, c.declarations, c.isInterface)
            newC
        })
    }
}

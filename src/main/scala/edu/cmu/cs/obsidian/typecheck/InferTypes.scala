package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.HashMap

class InferTypes(val symbolTable: SymbolTable) {
    def inferTypesInProgram(): Program = {
        val program = symbolTable.ast
        program.copy(contracts = program.contracts map inferTypesInContract).setLoc(program)
    }

    private def inferTypesInContract(c: Contract): Contract = {
        c match {
            case obs: ObsidianContractImpl => obs.copy(declarations = c.declarations map inferTypesInDeclaration).setLoc(c)
            case ffi: javaFFIContractImpl => ffi
        }
    }

    private def inferTypesInDeclaration(d: Declaration): Declaration = {
        // Only transactions need their types inferred.
        d match {
            case tx: Transaction => inferTypesInTransaction(tx)
            case _ => d
        }
    }


    private def inferTypesInTransaction(transaction: Transaction): Transaction = {
        // For now, focus on inferring types of local variables.
        transaction
    }

    /* TODO: finish this (it was started to infer pre- and post- types of private fields).
     private def inferTypesInTransaction(transaction: Transaction): Transaction = {
         // Infer initial and final field types if this transaction is private.
         // Also infer types of local variables.


         val receiverType: NonPrimitiveType = transaction.thisType
         val receiverContractTableOpt: Option[ContractTable] = symbolTable.contract(receiverType.contractName)
         if(receiverContractTableOpt.isEmpty) {
             // Bogus type. We'll produce an error later in the typechecking process.
             return transaction;
         }

         val receiverContractTable = receiverContractTableOpt.get
         val receiverFields: Set[Field] = receiverType match {
             case StateType(_, stateNames, _) =>
                 receiverContractTable.allFieldsAvailableInStates(stateNames)
             case _ => receiverContractTable.allFieldsAvailableInStates(receiverContractTable.possibleStates)
         }

         // Initially assume the least restrictive permissions.
         // No need to track primitive types, since those don't have permissions.
         val nonPrimitiveFieldTypes = new HashMap[String, NonPrimitiveType]
         for (f <- receiverFields) {
             nonPrimitiveFieldTypes.update(f.name, ContractReferenceType(ContractType(receiverType.contractName), Unowned(), receiverType.isRemote))
         }

         // TODO: revisit this structure.

         def fieldNamesRead(stmt: Statement): Set[String] = {
             stmt match {
                 case VariableDeclWithInit(_, _, e) => fieldNamesRead(e)
                 case ReturnExpr(e) => fieldNamesRead(e)
                 case Transition(_, updates) =>
                     updates match {
                         case None => Set.empty
                         case Some(u) => u.map(_._2).map(fieldNamesRead).reduce((s1, s2) => s1.union(s2))
                     }
                 case Assignment(_, e) => fieldNamesRead(e)
                 case If(e, stmts) => fieldNamesRead(e).union(stmts.map(fieldNamesRead).reduce((s1, s2) => s1.union(s2)))
                 case IfThenElse(e, thenStmts, elseStmts) =>
                     val thenFields = thenStmts.map(fieldNamesRead).reduce((s1, s2) => s1.union(s2))
                     val elseFields = elseStmts.map(fieldNamesRead).reduce((s1, s2) => s1.union(s2))
                     fieldNamesRead(e).union(thenFields).union(elseFields)
                 case TryCatch(s1, s2) => fieldNamesRead(s1).union(fieldNamesRead(s2))
                 // TODO: more cases
             }
         }

         for (stmt <- transaction.body) {
             // For each statement that uses a field as an input, see what the type requirements are, and factor those restrictions in.

         }


        transaction
    }
*/
}

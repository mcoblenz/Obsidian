package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.util._

import scala.util.parsing.input.Position


/* Important Note: be sure to take into account the fact that AST nodes need a location.
 * To construct a new AST node in this file, explicitly set the location using [setLoc] */


// Checks to make sure all state names mentioned in type names are actually valid states.
object StateNameValidator extends IdentityAstTransformer {
    override def transformType(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            context: Context,
            t: ObsidianType,
            pos: Position,
            params: Seq[GenericType]): (ObsidianType, List[ErrorRecord]) = {
        t match {
            case genericType: GenericType =>
                val (transformedParams, errorLists) =
                    genericType.genericParams.map(transformType(table, lexicallyInsideOf, context, _, pos, params)).unzip
                (genericType.withParams(transformedParams), errorLists.flatten.toList)
            // TODO GENERIC: Clean this up
            case np: NonPrimitiveType =>
                lexicallyInsideOf.lookupContract(np.contractName) match {
                case Some(ct) =>
                    np match {
                        case StateType(_, stateNames, _) =>
                            var errors = List.empty[ErrorRecord]
                            for (stateName <- stateNames) {
                                if (ct.state(stateName).isEmpty) {
                                    errors = ErrorRecord(StateUndefinedError(np.contractName, stateName), pos, currentContractSourcePath) +: errors
                                }
                            }

                            if (errors.isEmpty) {
                                val (transformedParams, errorLists) =
                                    np.genericParams.map(transformType(table, lexicallyInsideOf, context, _, pos, params)).unzip
                                (np.withParams(transformedParams), errors ++ errorLists.flatten)
                            } else {
                                (BottomType(), errors)
                            }

                        case _ =>
                            val (transformedParams, errorLists) =
                                np.genericParams.map(transformType(table, lexicallyInsideOf, context, _, pos, params)).unzip
                            (np.withParams(transformedParams), errorLists.flatten.toList)
                    }

                case None =>
                    lexicallyInsideOf.contract match {
                        case impl: ObsidianContractImpl =>
                            (impl.params ++ params).find(p => p.gVar.varName == np.contractName) match {
                                case Some(genericType) =>
                                    val newGenericType = np match {
                                        case StateType(contractType, states, isRemote) =>
                                            // If any of these states is the variable, then the actual bound should be the bound on the generic variable
                                            if (states.exists(genericType.gVar.permissionVar.contains(_))) {
                                                genericType
                                            } else {
                                                genericType.withTypeState(States(states))
                                            }
                                        case GenericType(gVar, bound) => genericType
                                        case _ => genericType.withTypeState(np.permission)
                                    }

                                    val (transformedParams, errorLists) =
                                        newGenericType.genericParams.map(transformType(table, lexicallyInsideOf, context, _, pos, params)).unzip
                                    (newGenericType.withParams(transformedParams), errorLists.flatten.toList)
                                case None =>
                                    (BottomType(), List(ErrorRecord(ContractUndefinedError(np.contractName), pos, currentContractSourcePath)))
                            }

                        case _: JavaFFIContractImpl =>
                            (BottomType(), List(ErrorRecord(ContractUndefinedError(np.contractName), pos, currentContractSourcePath)))
                    }
            }
            case _ => (t, List.empty)
        }
    }

    override def transformTypeState(table: SymbolTable, lexicallyInsideOf: DeclarationTable, typeState: TypeState,
                                    pos: Position, params: Seq[GenericType]): (TypeState, List[ErrorRecord]) = {
        val allParams = lexicallyInsideOf.contract match {
            case impl: ObsidianContractImpl => impl.params ++ params
            case _ => params
        }

        typeState match {
            case sts@States(states) =>
                val lookedUp = states.map(state => allParams.find(_.hasPermissionVar(state)))
                lookedUp.find(_.isDefined) match {
                    case Some(genericType) =>
                        (PermVar(genericType.get.gVar.permissionVar.get), List())
                    case None => (sts, List())
                }

            case ts => (ts, List())
        }
    }
}

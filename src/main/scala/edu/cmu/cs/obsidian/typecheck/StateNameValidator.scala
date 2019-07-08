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
            pos: Position): (ObsidianType, List[ErrorRecord]) = {
        t match {
            case np: NonPrimitiveType => lexicallyInsideOf.lookupContract(np.contractName) match {
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
                                (np, errors)
                            } else {
                                (BottomType(), errors)
                            }

                        case _ => (np, List.empty)
                    }

                case None =>
                    lexicallyInsideOf.contract match {
                        case ObsidianContractImpl(modifiers, name, params, implementBound, declarations, transitions, isInterface, sp) =>
                            params.find(p => p.gVar.varName == np.contractName) match {
                                case Some(genericType) =>
                                    val stateNames = np match {
                                        case StateType(contractType, states, isRemote) => Some(states)
                                        case GenericType(gVar, bound) => bound match {
                                            case GenericBoundStates(interfaceName, interfaceParams, states) => Some(states)
                                            case GenericBoundPerm(interfaceName, interfaceParams, permission) => None
                                        }
                                        case _ => None
                                    }

                                    // TODO GENERIC: Factor out this sort of thing to like a copy permission/state thing
                                    stateNames match {
                                        case Some(states) =>
                                            (genericType.withStates(states), List())
                                        case None =>
                                            (genericType.withPermission(np.permission), List())
                                    }
                                case None =>
                                    (BottomType(), List(ErrorRecord(ContractUndefinedError(np.contractName), pos, currentContractSourcePath)))
                            }

                        case JavaFFIContractImpl(name, interface, javaPath, sp, declarations) =>
                            // TODO GENERIC: how to handle interaction between java and obsidian generics?
                            (BottomType(), List(ErrorRecord(ContractUndefinedError(np.contractName), pos, currentContractSourcePath)))
                    }
            }
            case _ => (t, List.empty)
        }
    }
}

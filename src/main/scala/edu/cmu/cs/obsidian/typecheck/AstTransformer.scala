package edu.cmu.cs.obsidian.typecheck

import com.helger.jcodemodel.JPackage
import edu.cmu.cs.obsidian.codegen.{Client, Server}
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.util._

import scala.collection.Map
import scala.util.parsing.input.Position
import scala.collection.immutable.{HashSet, TreeMap, TreeSet}

/* The only purpose of this compilation phase at the moment is to disambiguate
 * path-types. For example, If [T] is defined as a dependent type of [C], then
 * inside of [C], the types [T] and [this.T] refer to the same thing. This
 * must be clarified. */

/* Important Note: be sure to take into account the fact that AST nodes need a location.
 * To construct a new AST node in this file, explicitly set the location using [setLoc] */

object AstTransformer extends IdentityAstTransformer {

    override def transformType(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            context: Context,
            t: ObsidianType,
            pos: Position): (ObsidianType, List[ErrorRecord]) = {

        // We should only be transforming potentially-unresolved types, but we can't specify that statically because ASTs are used for resolved types too.

        t match {
            case np: NonPrimitiveType =>
                lexicallyInsideOf.lookupContract(np.contractName) match {
                    case Some(ct) =>
                        np match {
                            case StateType(_, stateNames, _) =>
                                var errors = List.empty[ErrorRecord]
                                for (stateName <- stateNames) {
                                    if (ct.state(stateName).isEmpty) {
                                        errors = ErrorRecord(StateUndefinedError(np.contractName, stateName), pos) +: errors
                                    }
                                }
                                if (errors.isEmpty) {
                                    (np, errors)
                                }
                                else {
                                    (BottomType(), errors)
                                }
                            case _ => (np, List.empty)
                        }
                    case None => (BottomType(), List(ErrorRecord(ContractUndefinedError(np.contractName), pos)))
                }
            case _ => (t, List.empty)
        }
    }



}

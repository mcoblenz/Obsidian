package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.parser._

/* a type error: has a message (presented to the user).
 * The message generally depends on the parameters of the error: e.g. a subtyping error
 * is parametrized on the types T1 and T2 such that [T1 <: T2] was not satisfied */
abstract class Error {
    val msg: String
}

case class ErrorRecord (error: Error, pos: scala.util.parsing.input.Position, srcPath: String) extends Ordered[ErrorRecord] {
    def printMessage(): Unit = {
        println(s"$srcPath $pos: ${error.msg}")
    }

    def compare(that: ErrorRecord): Int = {
        if (this.pos < that.pos) -1
        else if (this.pos == that.pos) 0
        else 1
    }
}

case class NoMainContractError() extends Error {
    val msg: String = "No main contract found."
}

case class ShadowingError(fieldName: String, stateName: String, prevLine: Int) extends Error {
    val msg: String = s"Field '$fieldName' in '$stateName' also declared in contract at line $prevLine. Consider removing declaration in state."
}

case class ArgShadowingError(arg: String, transactionName: String, prevLine: Int) extends Error {
    val msg: String = s"The transaction parameter '$arg' in '$transactionName' is also declared in contract at line $prevLine. Consider changing the transaction parameter."
}

case class SharedFieldNameError(fieldName: String, stateName: String, prevLine: Int) extends Error {
    val msg: String = s"Field '$fieldName' previously declared in state '$stateName' at line $prevLine."
}
case class RepeatContractFields(fieldName: String, lineNum: Int, prevLine: Int) extends Error {
    val msg: String = s"Field '$fieldName' previously declared at line $prevLine. Consider removing line $lineNum."
}
case class CombineAvailableIns(fieldName: String, states: String, prevLine: Int) extends Error {
    val msg: String = s"Field '$fieldName' previously declared at line $prevLine. Did you mean '$fieldName available in $states'?"
}

case class SubtypingError(t1: ObsidianType, t2: ObsidianType, isThis : Boolean) extends Error {
    val msg: String =
        t1 match {
            case np: NonPrimitiveType =>
                if (np.permission == Inferred()) {
                    s"Found a local variable of unknown permission; ensure that a value was assigned to this variable."
                }
                else if (isThis) {

                    t2 match {
                        case np2: NonPrimitiveType =>
                            val p1 = np.permission
                            val p2 = np2.permission
                            s"'this' is $p1 at the end of the transaction, but the transaction signature requires that it be $p1"
                        case _ => assert(false); "s\"Found type '$t1', but expected something of type '$t2'"
                    }
                }
                else {
                    s"Found type '$t1', but expected something of type '$t2'"
                }
            case _ => s"Found type '$t1', but expected something of type '$t2'"
        }

}
case class VariableUndefinedError(x: String, context: String) extends Error {
    val msg: String = s"Variable '$x' is undefined in '$context'."

    override def equals(that: Any): Boolean = {
        that match {
            case that@VariableUndefinedError(y, _) => y == x
            case _ => false
        }
    }

    override def hashCode(): Int = x.hashCode()

}
case class DifferentTypeError(e1: Expression, t1: ObsidianType, e2: Expression, t2: ObsidianType) extends Error {
    val msg: String = s"Expression '$e1' has type '$t1', and expression '$e2' has type '$t2'," +
        s"but these expressions must have the same type"
}
case class FieldUndefinedError(fieldOf: NonPrimitiveType, fName: String) extends Error {
    val msg: String = fieldOf match {
        case ContractReferenceType(cName, _, _) => s"Field '$fName' is not defined in contract '$cName'"
        case StateType(cName, stateNames, _) => s"Field '$fName' is not defined in states '$stateNames' of contract '$cName'"
        case InterfaceContractType(name, typ) => s"Interfaces do not include fields."
    }
}
case class RecursiveFieldTypeError(cName: String, fName: String) extends Error {
    val msg: String = s"The type of field '$fName' in contract '$cName' recursively refers to itself"
}
case class RecursiveVariableTypeError(varName: String) extends Error {
    val msg: String = s"The type of variable '$varName' recursively refers to itself"
}
case class FieldNotConstError(cName: String, fName: String) extends Error {
    val msg: String = s"Field '$fName' must be labeled 'const' in contract '$cName'"
}
case class FieldConstMutationError(fName: String) extends Error {
    val msg: String = s"Field '$fName' cannot be mutated because it is labeled 'const'"
}
case class DereferenceError(typ: ObsidianType) extends Error {
    val msg: String = s"Type '$typ' cannot be dereferenced."
}
case class SwitchError(typ: ObsidianType) extends Error {
    val msg: String = s"Type '$typ' cannot be switched on."
}
case class MethodUndefinedError(receiver: NonPrimitiveType, name: String) extends Error {
    val msg: String = receiver match {
        case ContractReferenceType(cName, _, _) =>
            s"No transaction or function with name '$name' was found in contract '$cName'"
        case InterfaceContractType(cName, _) =>
            s"No transaction or function with name '$name' was found in interface '$cName'"
        case StateType(cName, sNames, _) =>
            s"No transaction or function with name '$name' was found in states '$sNames' of contract '$cName'"

    }
}
case class StateUndefinedError(cName: String, sName: String) extends Error {
    val msg: String = s"No state with name '$sName' was found in contract '$cName'"
}
case class ContractUndefinedError(cName: String) extends Error {
    val msg: String = s"No contract with name '$cName' is defined"
}
case class NonInvokeableError(t: ObsidianType) extends Error {
    val msg: String = s"Cannot invoke functions or transactions on type '$t'"
}
case class WrongArityError(expected: Int, have: Int, methName: String) extends Error {
    val msg: String =
        if (expected > have) {
            s"Too few arguments supplied to '$methName': expected '$expected', but found '$have'"
        } else {
            s"Too many arguments supplied to '$methName': expected '$expected', but found '$have'"
        }
}
case class MergeIncompatibleError(name: String, t1: ObsidianType, t2: ObsidianType) extends Error {
    val msg: String = s"Variable '$name' is incompatibly typed as both '$t1' and '$t2' after branch."
}
case class MustReturnError(methName: String) extends Error {
    val msg: String = s"'$methName' specifies a return type, but no return value is given."
}
case class CannotReturnError(methName: String) extends Error {
    val msg: String = s"'$methName' does not return anything, but a return value was given."
}
case class NotAValueError(methName: String) extends Error {
    val msg: String = s"'$methName' does not return anything, but is used here as a value."
}

case class TransitionUpdateError(mustSupply: Set[String]) extends Error {
    val fieldNames: String = mustSupply.mkString(", ")
    val msg: String = s"The following fields in the destination state may not be initialized: '$fieldNames'"
}
case class AssignmentError() extends Error {
    val msg: String = s"Assignment target must be a variable or a field."
}
//case class AlreadyKnowStateError(e: Expression, sName: String) extends Error {
//    val msg: String = s"'$e' is already known to be in state '$sName': a dynamic check is not needed"
//}

case class LeakReturnValueError(methName: String) extends Error {
    val msg: String = s"Invocation of '$methName' leaks ownership of return value."
}
case class NoEffectsError(s: Statement) extends Error {
    val msg: String = s"Statement '$s' has no side-effects."
}

case class UnusedOwnershipError(name: String) extends Error {
    val msg: String = s"Variable '$name' holds ownership, but is unused at the end of its scope."
}

case class UnusedExpressionOwnershipError(e: Expression) extends Error {
    val msg: String = s"Expression '$e' holds ownership, but is unused at the end of its scope."
}

case class PotentiallyUnusedOwnershipError(name: String) extends Error {
    val msg: String = s"Variable '$name' holds ownership, but may be unused at the end of its scope."
}

case class OverwrittenOwnershipError(name: String) extends Error {
    val msg: String = s"Variable '$name' is an owning reference to an asset, so it cannot be overwritten."
}

case class ConstructorNameError(contractName: String) extends Error {
    val msg: String = s"Invalid constructor name for contract '$contractName'"
}
case class CannotConvertPathError(badPart: String, expr: Expression, typ: NonPrimitiveType) extends Error {
    val msg: String = s"Cannot convert path in type '$typ': '$badPart' is equivalent to" +
        s"a non-variable expression '$expr'"
}
case class UnreachableCodeError() extends Error {
    val msg: String = s"Statement is unreachable"
}
case class NoStartStateError(contractName: String) extends Error {
    val msg: String = s"Constructor for '$contractName' does not transition to a named state"
}
case class NoConstructorError(contractName: String) extends Error {
    val msg: String = s"Contract '$contractName' must have a constructor since it contains states"
}

case class MultipleConstructorsError(contractName: String) extends Error {
    val msg: String = s"Main contract '$contractName' must only contain one constructor."
}

case class RepeatConstructorsError(contractName: String) extends Error {
    val msg: String = s"Contract '$contractName' has multiple constructors that take the same arguments."
}

case class NoParentError(cName: String) extends Error {
    val msg: String = s"Contract $cName has no parent contract"
}

case class AssetContractConstructorError(contractName: String) extends Error {
    val msg: String = s"Constructors in asset contract $contractName must return owned references."
}

case class OwnershipSubtypingError(t1: ObsidianType, t2: ObsidianType) extends Error {
    val msg: String = s"Can't transfer ownership to type '$t2' from unowned type '$t1'."
}

case class NonAssetOwningAssetError(contractName: String, f: Field) extends Error {
    val msg: String = s"Non-asset contract '$contractName' cannot own asset field '${f.name}' of type '${f.typ}'."
}

case class DisownUnowningExpressionError(e: Expression) extends Error {
    val msg: String = s"Can't disown expression that is not already owned: '$e'."
}

case class InvalidStateFieldInitialization(stateName: String, fieldName: String) extends Error {
    val msg: String = s"Can't assign to field $fieldName without transitioning to its state, $stateName."
}

case class NonStaticAccessError(method: String, name:String) extends Error {
    val msg: String = s"Cannot invoke a non-static transaction '$method' via a static reference '$name'"
}

case class StaticAssertOnPrimitiveError(e: Expression) extends Error {
    val msg: String = s"Cannot check the ownership or state of primitive expression '$e'."
}

case class StaticAssertFailed(e: Expression, statesOrPermissions: Seq[String], actualType: ObsidianType) extends Error {
    val stateStr = statesOrPermissions.mkString(" | ")

    val msg: String = s"Expression '$e' failed assertion $stateStr. Actual type: $actualType."
}

case class StaticAssertInvalidState(contractName: String, stateOrPermission: String) extends Error {
    val msg: String = s"Cannot assert for invalid state '$stateOrPermission' in contract $contractName"
}

case class ArgumentSpecificationError(arg: String, transactionName: String, t1: ObsidianType, t2: ObsidianType) extends Error {
    val msg: String = s"The argument '$arg' in '$transactionName' was specified to end as type '$t1', " +
        s"but actually ends as type '$t2'."
}

case class InvalidNonThisFieldAssignment() extends Error {
    val msg: String = "Cannot assign to fields of variables other than 'this'."
}

case class InvalidNonThisFieldAccess() extends Error {
    val msg: String = "Cannot read fields of variables other than 'this'. Instead, use an accessor transaction."
}


case class InvalidInconsistentFieldType(fieldName: String, actualType: ObsidianType, expectedType: ObsidianType) extends Error {
    val msg: String = s"At the ends of transactions, all fields must reference objects consistent with their declared types. " +
        s" Field '$fieldName' is of type $actualType but was declared as $expectedType."
}

case class TransitionNotAllowedError() extends Error {
    val msg: String = "Cannot change state because 'this' was specified to not allow state changes."
}

case class ReceiverTypeIncompatibleError(transactionName: String, actualType: ObsidianType, expectedType: ObsidianType) extends Error {
    val msg: String = s"Cannot invoke $transactionName on a receiver of type $actualType; a receiver of type $expectedType is required."
}

case class InconsistentContractTypeError(declaredContractName: String, actualContractName: String) extends Error {
    val msg: String = s"Cannot assign a value of contract $actualContractName to a variable that requires a value of contract $declaredContractName."
}

case class InconsistentTypeAssignmentError(declaredType: ObsidianType, actualType: ObsidianType) extends Error {
    val msg: String = s"Cannot assign a value of type $actualType to a variable of type $declaredType."
}

case class ArgumentSubtypingError(tName: String, arg: String, t1: ObsidianType, t2: ObsidianType) extends Error {
    val msg: String = s"Found type '$t1' as an argument to '$tName', but the argument '$arg' is expected to be something of type '$t2'."
}

case class FieldTypesDeclaredOnPublicTransactionError(tName: String) extends Error {
    val msg: String = s"Transaction $tName is public, so it cannot have initial or final field types specified."
}

case class InvalidFinalFieldTypeDeclarationError(fieldName: String) extends Error {
    val msg: String = s"Field $fieldName does not exist, so it cannot have a different declared final type."
}

case class FieldSubtypingError(fieldName: String, actualType: ObsidianType, expectedType: ObsidianType) extends Error {
    val msg: String = s"Field $fieldName needs to be of type $expectedType but is actually of type $actualType."
}


case class FieldMissingPermissionError(fieldName: String) extends Error {
    val msg: String = s"Field $fieldName needs to have a permission, such as Owned, Unowned, or Shared."
}

case class ReturnTypeMissingPermissionError(typeName: String) extends Error {
    val msg: String = s"Return type $typeName needs to have a permission, such as Owned, Unowned, or Shared."
}

case class InvalidLocalVariablePermissionDeclarationError() extends Error {
    val msg: String = s"Local variable declarations cannot include states or permissions. They must be inferred from the type of the assigned value."
}

case class ImportError(msg: String) extends Error {
}

case class DuplicateContractError(contractName: String) extends Error {
    val msg: String = s"Duplicate contract $contractName."
}

case class UninitializedFieldError(fieldName: String) extends Error {
    val msg: String = s"Field $fieldName may not be initialized in this constructor."
}

case class StateCheckOnPrimitiveError() extends Error {
    val msg: String = s"Can't check the state of a primitive-type expression."
}

case class StateCheckRedundant() extends Error {
    val msg: String = s"State check will always pass/fail. Remove redundant code."
}

case class InvalidValAssignmentError() extends Error {
    val msg: String = s"Can't reassign to variables that are formal parameters or which are used in a dynamic state check."
}

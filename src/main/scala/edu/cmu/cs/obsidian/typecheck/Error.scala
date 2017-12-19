package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

/* a type error: has a message (presented to the user).
 * The message generally depends on the parameters of the error: e.g. a subtyping error
 * is parametrized on the types T1 and T2 such that [T1 <: T2] was not satisfied */
abstract class Error {
    val msg: String
}

case class ErrorRecord (error: Error, pos: scala.util.parsing.input.Position) extends Ordered[ErrorRecord] {
    def printMessage(): Unit = {
        println(s"At $pos: ${error.msg}")
    }

    def compare(that: ErrorRecord): Int = {
        if (this.pos < that.pos) -1
        else if (this.pos == that.pos) 0
        else 1
    }
}

case class SubTypingError(t1: ObsidianType, t2: ObsidianType) extends Error {
    val msg: String = s"Found type '$t1', but expected something of type '$t2'"
}
case class VariableUndefinedError(x: String, context: String) extends Error {
    val msg: String = s"Variable '$x' is undefined in $context"

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
case class FieldUndefinedError(fieldOf: SimpleType, fName: String) extends Error {
    val msg: String = fieldOf match {
        case JustContractType(cName) => s"Field '$fName' is not defined in contract '$cName'"
        case StateUnionType(cName, _) => s"Field '$fName' is not defined in contract '$cName'"
        case StateType(cName, sName) => s"Field '$fName' is not defined in state '$sName' of contract '$cName'"
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
    val msg: String = s"Type '$typ' cannot be dereferenced"
}
case class SwitchError(typ: ObsidianType) extends Error {
    val msg: String = s"Type '$typ' cannot be switched on"
}
case class MethodUndefinedError(receiver: SimpleType, name: String) extends Error {
    val msg: String = receiver match {
        case JustContractType(cName) =>
            s"No transaction or function with name '$name' was found in contract '$cName'"
        case StateUnionType(cName, _) =>
            s"No transaction or function with name '$name' was found in contract '$cName'"
        case StateType(cName, sName) =>
            s"No transaction or function with name '$name' was found in state '$sName' of contract '$cName'"
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
    val msg: String = s"Variable '$name' is incompatibly typed as both '$t1' and '$t2' after branch"
}
case class MustReturnError(methName: String) extends Error {
    val msg: String = s"'$methName' specifies a return type, but no return value is given"
}
case class CannotReturnError(methName: String) extends Error {
    val msg: String = s"'$methName' does not return anything, but a return value was given"
}
case class NotAValueError(methName: String) extends Error {
    val msg: String = s"'$methName' does not return anything, but is used here as a value"
}
case class TransitionError() extends Error {
    val msg: String = s"'this' must be typed to a particular state in order to transition"
}
case class TransitionUpdateError(mustSupply: Set[String]) extends Error {
    val fieldNames: String = mustSupply.mkString(", ")
    val msg: String = s"Must specify the following fields in the update clause: '$fieldNames'"
}
case class AssignmentError() extends Error {
    val msg: String = s"Assignment target must be a variable or a field"
}
case class AlreadyKnowStateError(e: Expression, sName: String) extends Error {
    val msg: String = s"'$e' is already known to be in state '$sName': a dynamic check is not needed"
}
case class LeakReturnValueError(methName: String) extends Error {
    val msg: String = s"Invocation of '$methName' leaks ownership of return value"
}
case class NoEffectsError(s: Statement) extends Error {
    val msg: String = s"Statement '$s' has no side-effects"
}
case class StateSpecificSharedError() extends Error {
    val msg: String = s"State-specific types are not safe for 'shared' references"
}
case class StateSpecificReadOnlyError() extends Error {
    val msg: String = s"State-specific types are not safe for 'readonlyState' references"
}
case class UnusedOwnershipError(name: String) extends Error {
    val msg: String = s"Variable '$name' holds ownership, but is unused at the end of its scope"
}
case class ConstructorNameError(contractName: String) extends Error {
    val msg: String = s"Invalid constructor name for contract '$contractName'"
}
case class CannotConvertPathError(badPart: String, expr: Expression, typ: UnpermissionedType) extends Error {
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

case class NoParentError(cName: String) extends Error {
    val msg: String = s"Contract $cName has no parent contract"
}

case class ResourceContractConstructorError(contractName: String) extends Error {
    val msg: String = s"Constructors in resource contract $contractName must return owned references."
}

case class OwnershipSubtypingError(t1: ObsidianType, t2: ObsidianType) extends Error {
    val msg: String = s"Can't transfer ownership to type '$t2' from unowned type '$t1'."
}

case class NonResourceOwningResourceError(contractName: String, f: Field) extends Error {
    val msg: String = s"Non-resource contract '$contractName' cannot own resource field '${f.name}' of type '${f.typ}'."
}

case class DisownUnowningExpressionError(e: Expression) extends Error {
    val msg: String = s"Can't disown expression that is not already owned: '$e'."
}
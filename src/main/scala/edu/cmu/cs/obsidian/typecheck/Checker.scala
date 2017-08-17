package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.{TreeMap, TreeSet, HashSet}
import scala.collection.Map
import scala.util.parsing.input.Position

/* [SimpleType] simply indicates a contract, and possibly a state or set of states: there
 * is neither a permission nor a path associated with the type */
sealed trait SimpleType { val contractName: String }

case class JustContractType(contractName: String) extends SimpleType {
    override def toString: String = contractName
}
/* Invariant: [stateNames] is missing at least one of the states of the
 * contract (i.e. it is more specific than [JustContractType(contractName)],
 * but has at least 2 distinct states */
case class StateUnionType(contractName: String, stateNames: Set[String]) extends SimpleType {
    private def orOfStates: String = stateNames.toSeq.tail.foldLeft(stateNames.head)(
        (prev: String, sName: String) => prev + " | " + sName
    )
    override def toString: String = contractName + "." + "(" + orOfStates + ")"
}

case class StateType(contractName: String, stateName: String) extends SimpleType {
    override def toString: String = contractName + "." + stateName
}

/* Either a raw type or a primitive type. This class of types is important because
 * they can be translated purely syntactically from AST types (i.e. no resolving of
 * path/contract/state names is required */
sealed trait UnresolvedType

/* [RawType] is a contract type that doesn't have a permission associated with it,
 * but potentially has a path. */
sealed trait RawType extends UnresolvedType {
    val extractSimpleType: SimpleType
}

case class NoPathType(ts: SimpleType) extends RawType {
    override def toString: String = ts.toString
    override val extractSimpleType: SimpleType = ts
}

/* a path starts with either a local variable or "this", but "this" can sometimes be omitted */
case class PathType(path: Seq[String], ts: SimpleType) extends RawType {
    private def pathAsString = path.foldLeft("")(
        (prev: String, pathNode: String) => prev + pathNode + "."
    )
    override def toString: String = pathAsString + ts.toString
    override val extractSimpleType: SimpleType = ts
}

/* This is different from the representation of types in the AST in that the permission
 * associated with the reference is always explicit.
 * Invariant: any path that occurs in the type makes "this" explicit */
sealed trait ResolvedType {
    // for tests
    val isBottom: Boolean
    val tableOpt: Option[DeclarationTable]

    /* the permission system doesn't allow arbitrary aliasing of a reference
     * typed as [t]: aliasing forces one of the resulting types to be
     * [residualType(t)] instead */
    val residualType: ResolvedType

    val extractSimpleType: Option[SimpleType]
    val extractRawType: Option[RawType]
}

/* int, bool, or string */
sealed trait PrimitiveType extends ResolvedType with UnresolvedType {
    val isBottom: Boolean = false
    val tableOpt: Option[DeclarationTable] = None
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = None
    override val extractRawType: Option[RawType] = None
}

sealed trait NonPrimitiveType extends ResolvedType {
    val isBottom: Boolean = false
    def table: DeclarationTable
    val tableOpt: Option[DeclarationTable] = Some(table)
}

/* all permissioned types are associated with their corresponding symbol table */
case class ReadOnlyRef(tableOf: DeclarationTable, t: RawType) extends NonPrimitiveType {
    override def table: DeclarationTable = tableOf
    override def toString: String = "readonly " + t.toString
    override def equals(other: Any): Boolean = {
        other match {
            case ReadOnlyRef(_, tr) => tr == t
            case _ => false
        }
    }
    override def hashCode(): Int = t.hashCode()
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = Some(t.extractSimpleType)
    override val extractRawType: Option[RawType] = Some(t)
}
case class SharedRef(tableOf: DeclarationTable, t: RawType) extends NonPrimitiveType {
    override def table: DeclarationTable = tableOf
    override def toString: String = "shared " + t.toString
    override def equals(other: Any): Boolean = {
        other match {
            case SharedRef(_, tr) => tr == t
            case _ => false
        }
    }
    override def hashCode(): Int = t.hashCode()
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = Some(t.extractSimpleType)
    override val extractRawType: Option[RawType] = Some(t)
}
case class OwnedRef(tableOf: DeclarationTable, t: RawType) extends NonPrimitiveType {
    override def table: DeclarationTable = tableOf
    override def toString: String = "owned " + t.toString
    override def equals(other: Any): Boolean = {
        other match {
            case OwnedRef(_, tr) => tr == t
            case _ => false
        }
    }
    override def hashCode(): Int = t.hashCode()
    override val residualType: ResolvedType = ReadOnlyRef(tableOf, t)
    override val extractSimpleType: Option[SimpleType] = Some(t.extractSimpleType)
    override val extractRawType: Option[RawType] = Some(t)
}
case class IntType() extends PrimitiveType {
    override def toString: String = "int"
}
case class BoolType() extends PrimitiveType {
    override def toString: String = "bool"
}
case class StringType() extends PrimitiveType {
    override def toString: String = "string"
}
/* Used to indicate an error in the type checker when a reasonable type cannot
 * otherwise be inferred */
case class BottomType() extends ResolvedType {
    val isBottom: Boolean = true
    val tableOpt: Option[DeclarationTable] = None
    override val residualType: ResolvedType = this
    override val extractSimpleType: Option[SimpleType] = None
    override val extractRawType: Option[RawType] = None
}

/* a type error: has a message (presented to the user).
 * The message generally depends on the parameters of the error: e.g. a subtyping error
 * is parametrized on the types T1 and T2 such that [T1 <: T2] was not satisfied */
abstract class Error {
    val msg: String
}

case class SubTypingError(t1: ResolvedType, t2: ResolvedType) extends Error {
    val msg: String = s"Found type '$t1', but expected something of type '$t2'"
}
case class VariableUndefinedError(x: String) extends Error {
    val msg: String = s"Variable '$x' is undefined in the current context"
}
case class DifferentTypeError(e1: Expression, t1: ResolvedType, e2: Expression, t2: ResolvedType) extends Error {
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
case class DereferenceError(typ: ResolvedType) extends Error {
    val msg: String = s"Type '$typ' cannot be dereferenced"
}
case class SwitchError(typ: ResolvedType) extends Error {
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
case class NonInvokeableError(t: ResolvedType) extends Error {
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
case class MergeIncompatibleError(name: String, t1: ResolvedType, t2: ResolvedType) extends Error {
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
    val msg: String = s"State-specific types are not safe for 'readonly' references"
}
case class UnusedOwnershipError(name: String) extends Error {
    val msg: String = s"Variable '$name' holds ownership, but is unused at the end of its scope"
}
case class ConstructorNameError(contractName: String) extends Error {
    val msg: String = s"Invalid constructor name for contract '$contractName'"
}
case class CannotConvertPathError(badPart: String, expr: Expression, typ: RawType) extends Error {
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

/* We define a custom type to store a special flag for if a context in after a "throw".
 * In the formalism, we allow throw to result in any type: in the implementation, we don't know
 * which immediately which type this needs to be in order for type checking to work */
case class Context(underlyingVariableMap: Map[String, ResolvedType], isThrown: Boolean)  {
    def keys: Iterable[String] = underlyingVariableMap.keys
    def updated(s: String, t: ResolvedType): Context =
        Context(underlyingVariableMap.updated(s, t), isThrown)
    def get(s: String): Option[ResolvedType] = underlyingVariableMap.get(s)
    def apply(s: String): ResolvedType = underlyingVariableMap(s)
    def uncheckedContext: Map[String, UnresolvedType] = {
        var rawContext = TreeMap[String, UnresolvedType]()
        for ((x, t) <- underlyingVariableMap) {
            t match {
                case BottomType() => ()
                case ReadOnlyRef(_, tr) => rawContext = rawContext.updated(x, tr)
                case SharedRef(_, tr) => rawContext = rawContext.updated(x, tr)
                case OwnedRef(_, tr) => rawContext = rawContext.updated(x, tr)
                case prim@IntType() => rawContext = rawContext.updated(x, prim)
                case prim@StringType() => rawContext = rawContext.updated(x, prim)
                case prim@BoolType() => rawContext = rawContext.updated(x, prim)
            }
        }
        rawContext
    }
    def makeThrown: Context = this.copy(isThrown = true)
    def tableOfThis: DeclarationTable = this("this").asInstanceOf[NonPrimitiveType].table
}

class Checker(unmodifiedTable: SymbolTable, verbose: Boolean = false) {

    val globalTable: SymbolTable = AstTransformer.disambiguateProgram(unmodifiedTable)

    /* only stores [UncheckedType]s; all types in the context can thus be
     * translated purely syntactically */
    type UncheckedContext = Map[String, UnresolvedType]

    val errors = new collection.mutable.ArrayStack[(Error, Position)]()

    /* an error is associated with an AST node to indicate where the error took place */
    private def logError(where: AST, err: Error): Unit = {
        errors.push((err, where.loc))

        /* this is helpful for debugging (to find out what function generated an error */
        if (verbose) {
            println("Logging Error:")
            val (msg, loc) = (err.msg, where.loc)
            println(s"$msg at Location $loc")
            println()
            for (ste: StackTraceElement <- Thread.currentThread.getStackTrace) {
                println(ste)
            }
            println("\n\n\n")
        }
    }

    //-------------------------------------------------------------------------
    /* [updated] functions replace one instance of a smaller component type for
     * another instance within the same larger type */

    private def updatedSimpleType(t: RawType, newSimple: SimpleType): RawType = {
        t match {
            case NoPathType(_) => NoPathType(newSimple)
            case PathType(p, _) => PathType(p, newSimple)
        }
    }

    private def updatedSimpleType(t: ResolvedType, newSimple: SimpleType): ResolvedType = {
        t match {
            case OwnedRef(table, tr) => OwnedRef(table, updatedSimpleType(tr, newSimple))
            case SharedRef(table, tr) => SharedRef(table, updatedSimpleType(tr, newSimple))
            case ReadOnlyRef(table, tr) => ReadOnlyRef(table, updatedSimpleType(tr, newSimple))
            case ts => ts
        }
    }

    private def updatedRawType(t: ResolvedType, newRaw: RawType): ResolvedType = {
        t match {
            case OwnedRef(table, _) => OwnedRef(table, newRaw)
            case SharedRef(table, _) => SharedRef(table, newRaw)
            case ReadOnlyRef(table, _) => ReadOnlyRef(table, newRaw)
            case ts => ts
        }
    }

    //-------------------------------------------------------------------------
    /* Subtyping definitions */

    /* true iff [t1 <: t2] */
    private def isSimpleSubtype(t1: SimpleType, t2: SimpleType): Boolean = {
        (t1, t2) match {
            case (JustContractType(c1), JustContractType(c2)) => c1 == c2
            case (StateType(c1, s1), StateType(c2, s2)) => c1 == c2 && s1 == s2
            case (StateType(c1, _), JustContractType(c2)) => c1 == c2
            case (StateType(c1, s), StateUnionType(c2, ss)) =>
                c1 == c2 && (ss contains s)
            case (StateUnionType(c1, ss1), StateUnionType(c2, ss2)) =>
                c1 == c2 && ss1.subsetOf(ss2)
            case _ => false
        }
    }

    private def isRawSubtype(t1: RawType, t2: RawType): Boolean = {
        (t1, t2) match {
            case (NoPathType(ts1), NoPathType(ts2)) => isSimpleSubtype(ts1, ts2)
            case (PathType(p1, ts1), PathType(p2, ts2)) if p1 == p2 =>
                isSimpleSubtype(ts1, ts2)
            case _ => false
        }
    }

    /* true iff [t1 <: t2] */
    private def isSubtype(t1: ResolvedType, t2: ResolvedType): Boolean = {
        (t1, t2) match {
            case (BottomType(), _) => true
            case (IntType(), IntType()) => true
            case (BoolType(), BoolType()) => true
            case (StringType(), StringType()) => true
            case (OwnedRef(_, tr1), OwnedRef(_, tr2)) => isRawSubtype(tr1, tr2)
            case (ReadOnlyRef(_, tr1), ReadOnlyRef(_, tr2)) => isRawSubtype(tr1, tr2)
            case (SharedRef(_, tr1), SharedRef(_, tr2)) => isRawSubtype(tr1, tr2)
            case _ => false
        }
    }

    /* returns [t1] if [t1 <: t2], logs an error and returns [BottomType] otherwise */
    private def checkIsSubtype(ast: AST, t1: ResolvedType, t2: ResolvedType): ResolvedType = {
        if (!isSubtype(t1, t2)) {
            logError(ast, SubTypingError(t1, t2))
            BottomType()
        }
        else t1
    }

    //-------------------------------------------------------------------------
    /* Helper functions to easily make new types */

    /* Determines what sort of simple type should be used, given a set a possible states */
    private def simpleOf(
            lexicallyInsideOf: DeclarationTable,
            cName: String,
            states: Set[String]): SimpleType = {
        val allPossibleStates = lexicallyInsideOf.lookupContract(cName).get.possibleStates
        if (states == allPossibleStates) {
            JustContractType(cName)
        } else if (states.size > 1) {
            StateUnionType(cName, states)
        } else {
            StateType(cName, states.head)
        }
    }

    /* just like the above, but accepts [states] in the format of [ensures] clauses */
    private def simpleOf(
            lexicallyInsideOf: DeclarationTable,
            cName: String,
            states: Option[Set[String]]): SimpleType = {
        states match {
            case None => JustContractType(cName)
            case Some(ss) => simpleOf(lexicallyInsideOf, cName, ss)
        }
    }

    private def rawOf(simple: SimpleType, path: Option[Seq[String]]): RawType = {
        path match {
            case None => NoPathType(simple)
            case Some(p) => PathType(p, simple)
        }
    }

    //-------------------------------------------------------------------------
    /* the upper bound U of two types T1 and T2 is a type such that
     * subtypeOf(T1, U) and subtypeOf(t2, U). Such a type doesn't always exist. */

    private def simpleUpperBound(
            lexicallyInsideOf: DeclarationTable,
            t1: SimpleType,
            t2: SimpleType): Option[SimpleType] = {
        def handleStateUnion(ss1: Set[String], ss2: Set[String]): Option[SimpleType] = {
            val c = t1.contractName
            val unionStates = ss1.union(ss2)
            Some(simpleOf(lexicallyInsideOf, c, unionStates))
        }
        if (t1.contractName != t2.contractName) return None
        (t1, t2) match {
            case (_, JustContractType(_)) => Some(t2)
            case (JustContractType(_), _) => Some(t1)
            case (StateType(c, s1), StateType(_, s2)) =>
                if (s1 == s2) Some(StateType(c, s1))
                else Some(JustContractType(c))
            case (StateUnionType(_, ss1), StateUnionType(_, ss2)) =>
                handleStateUnion(ss1, ss2)
            case (StateUnionType(_, ss), StateType(_, s)) =>
                handleStateUnion(ss, TreeSet[String]().insert(s))
            case (StateType(_, s), StateUnionType(_, ss)) =>
                handleStateUnion(ss, TreeSet[String]().insert(s))
            case _ => None
        }
    }

    private def rawUpperBound(
            lexicallyInsideOf: DeclarationTable,
            t1: RawType,
            t2: RawType): Option[RawType] = {
        (t1, t2) match {
            case (NoPathType(ts1), NoPathType(ts2)) =>
                simpleUpperBound(lexicallyInsideOf, ts1, ts2).map(NoPathType)
            case (PathType(p1, ts1), PathType(p2, ts2)) if p1 == p2 =>
                simpleUpperBound(lexicallyInsideOf, ts1, ts2).map(PathType(p1, _))
            case _ => None
        }
    }

    private def upperBound(
                              lexicallyInsideOf: DeclarationTable,
                              t1: ResolvedType, t2: ResolvedType): Option[ResolvedType] = {
        (t1, t2) match {
            case (IntType(), IntType()) => Some(IntType())
            case (BoolType(), BoolType()) => Some(BoolType())
            case (StringType(), StringType()) => Some(StringType())
            case (OwnedRef(table, tr1), OwnedRef(_, tr2)) =>
                rawUpperBound(lexicallyInsideOf, tr1, tr2).flatMap(s => Some(OwnedRef(table, s)))
            case (ReadOnlyRef(table, tr1), ReadOnlyRef(_, tr2)) =>
                rawUpperBound(lexicallyInsideOf, tr1, tr2).flatMap(s => Some(ReadOnlyRef(table, s)))
            case (SharedRef(table, tr1), SharedRef(_, tr2)) =>
                rawUpperBound(lexicallyInsideOf, tr1, tr2).flatMap(s => Some(SharedRef(table, s)))
            case _ => None
        }
    }

    //-------------------------------------------------------------------------
    /* [translateType] functions map from [AstType] instances to the new type
     * definitions defined at the top of this file */

    private def translateUncheckedTypeHelper(
            convertPath: Function[Seq[String], Seq[String]],
            t: AstType): UnresolvedType = {
        t match {
            case AstContractType(_, name) =>
                NoPathType(JustContractType(name))
            case AstStateType(_, cName, sName) =>
                NoPathType(StateType(cName, sName))
            case AstPathContractType(_, p, name) =>
                PathType(convertPath(p), JustContractType(name))
            case AstPathStateType(_, p, cName, sName) =>
                PathType(convertPath(p), StateType(cName, sName))
            case AstStringType() => StringType()
            case AstIntType() => IntType()
            case AstBoolType() => BoolType()
        }
    }

    private def translateUncheckedType(
            args: Seq[VariableDecl],
            t: AstType): UnresolvedType = {
        val argsAsSet = args.map(_.varName).toSet
        def convertPath(p: Seq[String]): Seq[String] = {
            if ((argsAsSet contains p.head) || p.head == "this") p
            else "this" +: p
        }
        translateUncheckedTypeHelper(convertPath, t)
    }

    private def translateUncheckedType(
            lexicallyInsideOf: DeclarationTable,
            t: AstType): UnresolvedType = {
        def convertPath(p: Seq[String]): Seq[String] = {
            if (p.head != "this")  "this" +: p
            else p
        }
        translateUncheckedTypeHelper(convertPath, t)
    }

    private def translateUncheckedType(
            context: Context,
            t: AstType): UnresolvedType = {
        def convertPath(p: Seq[String]): Seq[String] = {
            // try two interpretations: in path [x.y.z], [x] refers to a field or variable
            context.get(p.head) match {
                case Some(_) | None if p.head == "this" => p
                case None => "this" +: p
            }
        }
        translateUncheckedTypeHelper(convertPath, t)
    }

    /* fully translates a type by translating with [translateUncheckedType] and then
     * traversins the type to "check" it */

    private def translateType(lexicallyInsideOf: DeclarationTable, t: AstType): ResolvedType = {
        val tr = translateUncheckedType(lexicallyInsideOf, t) match {
            case prim: PrimitiveType => return prim
            case rawType: RawType => rawType
        }

        val (trNew, table) = resolveRawTypeNoContext(lexicallyInsideOf, tr) match {
            case Left(err) =>
                logError(t, err)
                return BottomType()
            case Right(travData) => travData
        }

        addModifier(trNew, table, extractModifiers(t))
    }

    private def translateType(context: Context, t: AstType): ResolvedType = {
        val tr = translateUncheckedType(context, t) match {
            case prim: PrimitiveType => return prim
            case rawType: RawType => rawType
        }

        val unchecked = context.uncheckedContext
        val lexicallyInsideOf = context.tableOfThis
        val (trNew, table) = resolveRawType(unchecked, lexicallyInsideOf, tr) match {
            case Left(err) =>
                logError(t, err)
                return BottomType()
            case Right(travData) => travData
        }

        addModifier(trNew, table, extractModifiers(t))
    }

    //-------------------------------------------------------------------------
    /* [traverse] functions try to resolve a type by looking it up.
     * Either an error is returned, or the symbol table that was found to be
     * associated with the type */

    private def resolveState(
            contractTable: ContractTable,
            sName: String): Either[Error, DeclarationTable] = {

        val stLookup = contractTable.state(sName)
        if (stLookup.isEmpty) {
            val err = StateUndefinedError(contractTable.name, sName)
            Left(err)
        } else {
            Right(stLookup.get)
        }

    }

    private def resolveStates(
            contractTable: ContractTable,
            ts: SimpleType): Either[Error, DeclarationTable] = {
        ts match {
            case StateType(_, sName) =>
                resolveState(contractTable, sName) match {
                    case Left(err) => Left(err)
                    case Right(stateTable) => Right(stateTable)
                }
            case StateUnionType(_, sNames) =>
                for (sName <- sNames) {
                    resolveState(contractTable, sName) match {
                        case Left(err) => return Left(err)
                        case Right(_) => ()
                    }
                }
                Right(contractTable)
            case JustContractType(_) => Right(contractTable)
        }
    }

    private def resolveNoPathSimple(ts: SimpleType): Either[Error, TraverseData[NoPathType]] = {

        val cName = ts.contractName
        val ctLookup = globalTable.contract(cName)

        if (ctLookup.isEmpty) {
            return Left(ContractUndefinedError(cName))
        }

        val contractTable = ctLookup.get

        resolveStates(contractTable, ts) match {
            case Right(table) => Right((NoPathType(ts), table))
            case Left(err) => Left(err)
        }
    }

    private def resolvePathSimple(
            lexicallyInsideOf: DeclarationTable,
            ts: SimpleType): Either[Error, TraverseData[PathType]] = {

        val cName = ts.contractName
        val ctLookup = lexicallyInsideOf.contract.childContract(ts.contractName)

        if (ctLookup.isEmpty) {
            return Left(ContractUndefinedError(cName))
        }

        val contractTable = ctLookup.get

        resolveStates(contractTable, ts) match {
            case Right(table) => Right(PathType(Nil, ts), table)
            case Left(err) => Left(err)
        }
    }

    private def appendToPath(
            f: String,
            td: Either[Error, TraverseData[PathType]]): Either[Error, TraverseData[PathType]] = {
        td match {
            case Left(_) => td
            case Right((PathType(path, ts), table)) =>
                Right((PathType(f +: path, ts), table))
        }
    }

    type TraverseData[T <: RawType] = (T, DeclarationTable)

    /* [resolveRawType] returns either an error that was reached while checking
     * (if [tr] could not be traversed), or the declaration table of the type,
     * as well as new raw type: this return value is only different from [tr]
     * if [tr] starts with an implicit "this" (in this case, "this" is added) */

    // For places where no context exists: e.g. in fields
    private def resolveRawTypeNoContext(
            lexicallyInsideOf: DeclarationTable,
            tr: RawType): Either[Error, TraverseData[RawType]] = {
        tr match {
            case NoPathType(ts) => resolveNoPathSimple(ts)
            case pathType@PathType(_,_) =>
                val visitedFields = new HashSet[(DeclarationTable, String)]()
                resolveRawTypeHelper(lexicallyInsideOf, visitedFields, pathType)
        }
    }

    private def resolveRawType(
           uncheckedContext: UncheckedContext,
           lexicallyInsideOf: DeclarationTable,
           tr: RawType): Either[Error, TraverseData[RawType]] = {
        resolveRawTypeHelper(uncheckedContext, lexicallyInsideOf, new HashSet[String](), tr)
    }

    // we keep track of which fields we've visited to avoid endless recursion
    private def resolveRawTypeHelper(
            lexicallyInsideOf: DeclarationTable,
            visitedFields: Set[(DeclarationTable, String)],
            tr: PathType): Either[Error, TraverseData[PathType]] = {
        tr match {
            case PathType(Seq(), ts) =>
                resolvePathSimple(lexicallyInsideOf, ts)

            case PathType("parent"::rest, ts) =>
                if (lexicallyInsideOf.contract.hasParent) {
                    val trNew = PathType(rest, ts)
                    val newInsideOf = lexicallyInsideOf.contract.parent.get
                    appendToPath("parent", resolveRawTypeHelper(newInsideOf, visitedFields, trNew))
                } else {
                    Left(NoParentError(lexicallyInsideOf.contract.name))
                }
            case PathType(first::restBeforePrune, ts) =>

                // prune off "this" from the head
                val p = if (first == "this") restBeforePrune else first::restBeforePrune
                if (p.isEmpty) {
                    return resolvePathSimple(lexicallyInsideOf, ts)
                }

                val f = p.head
                val rest = p.tail

                val fieldLookup = lexicallyInsideOf.lookupField(f)
                if (fieldLookup.isEmpty) {
                    return Left(FieldUndefinedError(lexicallyInsideOf.simpleType, f))
                }

                val field = fieldLookup.get

                val unchecked = translateUncheckedType(lexicallyInsideOf, field.typ)

                // paths must consist entirely of [const] fields
                if (!field.isConst) {
                    return Left(FieldNotConstError(lexicallyInsideOf.name, f))
                }

                if (visitedFields contains (lexicallyInsideOf, f)) {
                    return Left(RecursiveFieldTypeError(lexicallyInsideOf.name, f))
                }

                val newVisited = visitedFields.+((lexicallyInsideOf, f))

                val traverseField = unchecked match {
                    case prim: PrimitiveType =>
                        return Left(DereferenceError(prim))
                    case trField: NoPathType =>
                        resolveNoPathSimple(trField.ts)
                    case trField: PathType =>
                        resolveRawTypeHelper(lexicallyInsideOf, visitedFields, trField)
                }

                val newInsideOf = traverseField match {
                    case Left(err) => return Left(err)
                    case Right(travData) => travData._2
                }

                val trNew = PathType(rest, ts)

                appendToPath(f, resolveRawTypeHelper(newInsideOf, newVisited, trNew))
        }
    }

    // we keep track of which local variables we've visited to avoid endless recursion
    private def resolveRawTypeHelper(
            context: UncheckedContext,
            lexicallyInsideOf: DeclarationTable,
            visitedLocalVars: Set[String],
            tr: RawType): Either[Error, TraverseData[RawType]] = {

        tr match {
            case NoPathType(ts) => resolveNoPathSimple(ts)

            case PathType(x::rest, ts) if context contains x =>

                if (visitedLocalVars contains x) {
                    Left(RecursiveVariableTypeError(x))
                }
                val newVisited = visitedLocalVars + x

                val pathHeadType = context(x) match {
                    case prim: PrimitiveType => return Left(DereferenceError(prim))
                    case trNext: RawType => trNext
                }

                val newInsideOf =
                    resolveRawTypeHelper(context, lexicallyInsideOf, newVisited, pathHeadType) match {
                        case l@Left(_) => return l
                        case Right(travData) => travData._2
                }

                val trNew = PathType(rest, ts)

                val visitedFields = HashSet[(DeclarationTable, String)]()

                appendToPath(x, resolveRawTypeHelper(newInsideOf, visitedFields, trNew))


            case PathType(f::rest, ts) =>
                // see if the head of the path is actually a field
                val fieldLookup = lexicallyInsideOf.lookupField(f)

                if (fieldLookup.isEmpty) {
                    return Left(VariableUndefinedError(f))
                }

                val field = fieldLookup.get

                val unchecked = translateUncheckedType(lexicallyInsideOf, field.typ)

                val visitedFields =
                    HashSet[(DeclarationTable, String)]().+((lexicallyInsideOf, f))

                val traverseField = unchecked match {
                    case prim: PrimitiveType =>
                        return Left(DereferenceError(prim))
                    case trField: NoPathType =>
                        resolveNoPathSimple(trField.ts)
                    case trField: PathType =>
                        resolveRawTypeHelper(lexicallyInsideOf, visitedFields, trField)
                }

                val newInsideOf = traverseField match {
                    case Left(err) => return Left(err)
                    case Right(travData) => travData._2
                }

                val trNew = PathType(rest, ts)

                appendToPath("this",
                    appendToPath(f,
                        resolveRawTypeHelper(newInsideOf, visitedFields, trNew)
                    )
                )

            case PathType(Nil, _) => throw new RuntimeException("shouldn't happen")
        }
    }

    /* assumes that [t] is not a primitive type */
    private def extractModifiers(t: AstType): Seq[TypeModifier] = {
        t match {
            case x: AstPathContractType => x.modifiers
            case x: AstContractType => x.modifiers
            case x: AstPathStateType => x.modifiers
            case x: AstStateType => x.modifiers
            case _ => throw new RuntimeException("violates assumption")
        }
    }

    /* adds the modifier from [t] to [tr], assuming that [table] is the
     * symbol table of the type [tr] */
    private def addModifier(
            tr: RawType,
            table: DeclarationTable,
            mods: Seq[TypeModifier]): ResolvedType = {

        val defaultMod = table.contract.ast.mod match {
                case Some(m) => m
                case None => IsOwned()
            }

        /* if a reference is 'readonly', it is labeled as such; otherwise, it is
         * 'owned'/'shared', based on the declaration of the contract itself */
        if (mods.exists(_.isInstanceOf[IsReadOnly])) {
            ReadOnlyRef(table, tr)
        } else if (defaultMod.isInstanceOf[IsOwned]) {
            OwnedRef(table, tr)
        } else {
            // TODO: are main contracts always deemed shared by the type system?
            SharedRef(table, tr)
        }
    }

    private def checkType(
            context: UncheckedContext,
            lexicallyInsideOf: DeclarationTable,
            tAst: AstType,
            tUnchecked: UnresolvedType): ResolvedType = {
        checkTypeDontLog(context, lexicallyInsideOf, tAst, tUnchecked) match {
            case Left((ast, err)) =>
                logError(ast, err)
                BottomType()
            case Right(typ) => typ
        }
    }

    private def checkTypeDontLog(
            context: UncheckedContext,
            lexicallyInsideOf: DeclarationTable,
            tAst: AstType,
            tUnchecked: UnresolvedType): Either[(AST, Error), ResolvedType] = {
        tUnchecked match {
            case tr: RawType =>
                resolveRawType(context, lexicallyInsideOf, tr) match {
                    case Left(err) =>
                        Left((tAst, err))
                    case Right(travData) =>
                        Right(addModifier(travData._1, travData._2, extractModifiers(tAst)))
                }
            case prim: PrimitiveType => Right(prim)
        }
    }

    private def checkTypeReturnError(
            context: Context,
            tAst: AstType,
            tUnchecked: UnresolvedType): Either[(AST, Error), ResolvedType] = {
        checkTypeDontLog(context.uncheckedContext, context.tableOfThis, tAst, tUnchecked)
    }

    private def checkType(context: Context, tAst: AstType, tUnchecked: UnresolvedType): ResolvedType = {
        checkType(context.uncheckedContext, context.tableOfThis, tAst, tUnchecked)
    }

    //-------------------------------------------------------------------------
    // Checking definitions for language constructs begins here

    private def inferAndCheckExpr(
            decl: InvokableDeclaration,
            context: Context,
            e: Expression): (ResolvedType, Context) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: ResolvedType, c: Context): (ResolvedType, Context) = {
            val (tPrime, contextPrime) = inferAndCheckExpr(decl, c, e)
            (checkIsSubtype(e, tPrime, t), contextPrime)
        }

        def assertOperationType(e1: Expression, e2: Expression, t: ResolvedType): (ResolvedType, Context) = {
            val (_, c1) = assertTypeEquality(e1, t, context)
            val (_, c2) = assertTypeEquality(e2, t, c1)
            (t, c2)
        }

        def assertComparisonType(e1: Expression, e2: Expression): (ResolvedType, Context) = {
            val (_, c1) = assertTypeEquality(e1, IntType(), context)
            val (_, c2) = assertTypeEquality(e2, IntType(), c1)
            (BoolType(), c2)
        }

        def handleInvocation(
                context: Context,
                table: DeclarationTable,
                name: String,
                receiver: Expression,
                args: Seq[Expression]): (ResolvedType, Context) = {
            // Lookup the invocation
            val txLookup = table.lookupTransaction(name)
            val funLookup = table.lookupFunction(name)

            val invokable: InvokableDeclaration = (txLookup, funLookup) match {
                case (None, None) =>
                    val err = MethodUndefinedError(table.simpleType, name)
                    logError(e, err)
                    return (BottomType(), context)
                case (_, Some(f)) => f
                case (Some(t), _) => t
            }

            // check arguments
            val (argTypes, contextPrime) = inferAndCheckExprs(decl, context, args)
            val specList = (invokable.args, invokable)::Nil

            val (correctInvokable, calleeToCaller) =
                checkArgs(e, name, context, specList, receiver, argTypes) match {
                    case None => return (BottomType(), contextPrime)
                    case Some(x) => x
                }

            // check that there's a value to return
            if (correctInvokable.retType.isEmpty) {
                logError(e, NotAValueError(name))
                return (BottomType(), contextPrime)
            }

            val spec = correctInvokable.args

            val astType = correctInvokable.retType.get
            val retTypeCalleePoV = translateUncheckedType(spec, astType) match {
                case prim: PrimitiveType => return (prim, contextPrime)
                case tr: RawType => tr
            }

            toCallerPoV(calleeToCaller, retTypeCalleePoV) match {
                case Right(retTypeCallerPoV) =>
                    val checkedTyp = checkType(contextPrime, astType,
                                               fixRawType(contextPrime, retTypeCallerPoV))
                    (checkedTyp, contextPrime)
                case Left((first, badExpr)) =>
                    val err = CannotConvertPathError(first, badExpr, retTypeCalleePoV)
                    logError(e, err)
                    (BottomType(), contextPrime)
            }
        }

         e match {
             case Variable(x) =>
                 (context get x, context.tableOfThis.lookupField(x)) match {
                     case (Some(t), _) =>
                         (t, context.updated(x, t.residualType))
                     case (_, Some(f)) =>
                         // TODO handle cases for e.g. if the field is owned
                         (translateType(context, f.typ), context)
                     case (None, None) =>
                         logError(e, VariableUndefinedError(x))
                         (BottomType(), context)
                 }
             case NumLiteral(_) => (IntType(), context)
             case StringLiteral(_) => (StringType(), context)
             case TrueLiteral() => (BoolType(), context)
             case FalseLiteral() => (BoolType(), context)
             case This() =>
                 /* unlike variables, "this" must always be valid, so the residual type
                  * is returned, and the actual type stays in the variable */
                 (context("this").residualType, context)
             case Parent() =>
                 val thisTable = context.tableOfThis.contract
                 if (thisTable.hasParent) {
                     val parentTable = thisTable.parent.get
                     val ts = parentTable.simpleType
                     val tr = if (parentTable.hasParent) {
                         PathType("this"::"parent"::"parent"::Nil, ts)
                     } else {
                         NoPathType(ts)
                     }

                     (addModifier(tr, parentTable, Seq()), context)

                 } else {
                     logError(e, NoParentError(thisTable.name))
                     (BottomType(), context)
                 }
             case Conjunction(e1: Expression, e2: Expression) =>
                 assertOperationType(e1, e2, BoolType())
             case Disjunction(e1: Expression, e2: Expression) =>
                 assertOperationType(e1, e2, BoolType())
             case LogicalNegation(e: Expression) =>
                 assertTypeEquality(e, BoolType(), context)
             case Add(e1: Expression, e2: Expression) =>
                 assertOperationType(e1, e2, IntType())
             case Subtract(e1: Expression, e2: Expression) =>
                 assertOperationType(e1, e2, IntType())
             case Divide(e1: Expression, e2: Expression) =>
                 assertOperationType(e1, e2, IntType())
             case Multiply(e1: Expression, e2: Expression) =>
                 assertOperationType(e1, e2, IntType())
             case Equals(e1: Expression, e2: Expression) =>
                 val (t1, c1) = inferAndCheckExpr(decl, context, e1)
                 val (t2, c2) = inferAndCheckExpr(decl, c1, e2)
                 if (t1 == t2) (BoolType(), c2) else {
                     logError(e, DifferentTypeError(e1, t1, e2, t2))
                     (BottomType(), c2)
                 }
             case GreaterThan(e1: Expression, e2: Expression) =>
                 assertComparisonType(e1, e2)
             case GreaterThanOrEquals(e1: Expression, e2: Expression) =>
                 assertComparisonType(e1, e2)
             case LessThan(e1: Expression, e2: Expression) =>
                 assertComparisonType(e1, e2)
             case LessThanOrEquals(e1: Expression, e2: Expression) =>
                 assertComparisonType(e1, e2)
             case NotEquals(e1: Expression, e2: Expression) =>
                 val (t1, c1) = inferAndCheckExpr(decl, context, e1)
                 val (t2, c2) = inferAndCheckExpr(decl, c1, e2)
                 if (t1 == t2) (BoolType(), c2) else {
                     logError(e, DifferentTypeError(e1, t1, e2, t2))
                     (BottomType(), c2)
                 }

             case Dereference(eDeref: Expression, f) =>
                 val (derefType, contextPrime) = inferAndCheckExpr(decl, context, eDeref)

                 val derefTable = derefType match {
                     case BottomType() => return (BottomType(), contextPrime)
                     case IntType() | BoolType() | StringType() =>
                         logError(e, DereferenceError(derefType))
                         return (BottomType(), contextPrime)
                     // [get] is safe because we ruled out all other options
                     case _ => derefType.tableOpt.get
                 }

                 val fieldAST = derefTable.lookupField(f) match {
                     case Some(ast) => ast
                     case None =>
                         logError(e, FieldUndefinedError(derefTable.simpleType, f))
                         return (BottomType(), contextPrime)
                 }

                 (translateType(derefTable, fieldAST.typ), contextPrime)

             case LocalInvocation(name, args: Seq[Expression]) =>
                 val thisTable = context.tableOfThis
                 handleInvocation(context, thisTable, name, This(), args)

             case Invocation(receiver: Expression, name, args: Seq[Expression]) =>
                 val (receiverType, contextPrime) = inferAndCheckExpr(decl, context, receiver)

                 val receiverTable = receiverType match {
                     case BottomType() => return (BottomType(), contextPrime)
                     case IntType() | BoolType() | StringType() =>
                         logError(e, NonInvokeableError(receiverType))
                         return (BottomType(), contextPrime)
                     // [get] is safe because [receiverType] must be non-primitive
                     case _ => receiverType.tableOpt.get
                 }

                 handleInvocation(contextPrime, receiverTable, name, receiver, args)

             case Construction(name, args: Seq[Expression]) =>
                 val tableLookup = context.tableOfThis.lookupContract(name)
                 if (tableLookup.isEmpty) {
                     logError(e, ContractUndefinedError(name))
                     return (BottomType(), context)
                 }

                 val ctTableOfConstructed = tableLookup.get

                 var path: Option[Seq[String]] = null
                 if (ctTableOfConstructed.hasParent) path = Some("this"::Nil)
                 else path = None

                 val (argTypes, contextPrime) = inferAndCheckExprs(decl, context, args)
                 val constrSpecs = ctTableOfConstructed
                                    .constructors
                                    .map(constr => (constr.args, constr))

                 // todo : should this have a receiver
                 val result =
                     checkArgs(e, s"constructor of $name", context, constrSpecs, This(), argTypes)

                 val simpleType = result match {
                     // Even if the args didn't check, we can still output a type
                     case None => JustContractType(name)
                     case Some((constr, _)) =>
                         simpleOf(contextPrime.tableOfThis, name, constr.ensuresState)
                 }

                 val rawType = rawOf(simpleType, path)
                 // come up with something that has no permissions so we get the default
                 val dummyAstType = AstContractType(Nil, "dummy")

                 (checkType(contextPrime, dummyAstType, rawType), contextPrime)
         }
    }


    private def inferAndCheckExprs(
                                    decl: InvokableDeclaration,
                                    context: Context,
                                    es: Seq[Expression]
                                ): (Seq[(ResolvedType, Expression)], Context) = {
        val types = new ListBuffer[(ResolvedType, Expression)]()
        var contextPrime = context
        for (e <- es) {
            val (t, contextPrime2) =
                inferAndCheckExpr(decl, contextPrime, e)
            contextPrime = contextPrime2
            types.append((t, e))
        }
        (types, contextPrime)
    }

    /* returns true if the sequence of statements includes a return statement, or an if/else statement
     * where both branches have return statements, and false otherwise
     */
    private def hasReturnStatement(tx: Transaction, statements: Seq[Statement]) : Boolean = {
        var hasRet = false

        for (statement <- statements) {
            if (hasRet) {
                logError(statement, UnreachableCodeError())
                return hasRet
            }

            statement match {
                case Return() | ReturnExpr(_) => hasRet = true
                case IfThenElse(_, s1, s2) =>
                    hasRet = hasReturnStatement(tx, s1) && hasReturnStatement(tx, s2)
                case _ => ()
            }
        }

        hasRet
    }

    private def hasReturnStatementDontLog(statements: Seq[Statement]) : Boolean = {
        var hasRet = false

        for (statement <- statements) {
            statement match {
                case Return() | ReturnExpr(_) => return true
                case IfThenElse(_, s1, s2) =>
                    hasRet = hasReturnStatementDontLog(s1) && hasReturnStatementDontLog(s2)
                case _ => ()
            }
        }

        hasRet
    }

    /* returns true if the sequence of statements includes a state transition, or an if/else statement
    * where both branches have state transitions, and false otherwise
    */
    private def hasTransition(statements: Seq[Statement]) : Boolean = {
        var transition = false

        for (statement <- statements) {
            statement match {
                case Transition(_, _) => transition = true
                case IfThenElse(_, s1, s2) =>
                    transition = hasTransition(s1) && hasTransition(s2)
                case _ => ()
            }
        }

        transition
    }

    private def checkStatementSequence(
                                          decl: InvokableDeclaration,
                                          context: Context,
                                          s: Seq[Statement]
                                      ): Context = {
        s.foldLeft(context)((prevContext: Context, s: Statement) =>
                checkStatement(decl, prevContext, s))
    }

    /* returns a context that is the same as [branchContext], except only with
     * those variables bound which [oldContext] actually assign a value to */
    private def pruneContext(ast: AST, branchContext: Context, oldContext: Context): Context = {
        var newContext = oldContext

        for (x <- oldContext.keys) {
            val t = branchContext.get(x) match {
                case Some(tBranch) => tBranch
                case None => oldContext(x)
            }
            newContext = newContext.updated(x, t)
        }

        for (x <- branchContext.keys.toSet.diff(oldContext.keys.toSet)) {
            branchContext(x) match {
                case _: OwnedRef => logError(ast, UnusedOwnershipError(x))
                case _ => ()
            }
        }

        Context(newContext.underlyingVariableMap, isThrown = branchContext.isThrown)
    }

    private def mergeContext(
            lexicallyInsideOf: DeclarationTable,
            ast: AST,
            context1: Context,
            context2: Context): Context = {
        /* If we're merging with a context from a "throw", just take the other context
        * emit no errors */
        if (context1.isThrown && !context2.isThrown) return context2
        if (!context1.isThrown && context2.isThrown) return context1

        var mergedMap = new TreeMap[String, ResolvedType]()

        val inBoth = context1.keys.toSet.intersect(context2.keys.toSet)

        for (x <- inBoth) {
            val t1 = context1(x)
            val t2 = context2(x)
            upperBound(lexicallyInsideOf, t1, t2) match {
                case Some(u) => mergedMap = mergedMap.updated(x, u)
                case None =>
                    logError(ast, MergeIncompatibleError(x, t1, t2))
            }
        }

        Context(mergedMap, context1.isThrown)
    }

    /* if [e] is of the form Variable(x), This(), or if [e] is a sequence of
     * dereferences on Variable(x) or This(), [extractPath] extracts the list
     * of identifiers on the path. If [e] isn't this form, returns None */
    private def extractPath(e: Expression): Option[Seq[String]] = {
        e match {
            case Variable(x) => Some(x::Nil)
            case This() => Some("this"::Nil)
            case Parent() => Some("this"::"parent"::Nil)
            case Dereference(ePrime, f) => extractPath(ePrime).map(_ ++ (f::Nil))
            case _ => None
        }
    }

    /* Returns [Left(p.head)] where [p] is the head of the path if failure,
     * otherwise [Right(t)] where [t] is the type from the perspective of the caller */
    private def toCallerPoV(
            calleeToCaller: Map[String, Expression],
            tr: RawType): Either[(String, Expression), RawType] = {
        tr match {
            case PathType(p, ts) =>
                extractPath(calleeToCaller(p.head)) match {
                    case Some(newPath) => Right(PathType(newPath ++ p.tail, ts))
                    case None => Left(p.head, calleeToCaller(p.head))
                }
            case NoPathType(_) => Right(tr)
        }
    }

    /* removes unnecessary instances of "parent" from a type: e.g. if [x : y.T1],
     * then the type [x.parent.T2] is converted to [y.T2] */
    private def fixRawType(context: Context, tr: RawType): RawType = {
        tr match {
            case PathType(inContext +: "parent" +: rest, ts) if inContext != "this" =>
                context.get(inContext) match {
                    case Some(t) =>
                        t.extractRawType match {
                            /* shouldn't happen, but can be reported later */
                            case Some(PathType(newPath, _)) => PathType(newPath ++ rest, ts)
                            case _ => tr
                        }
                    /* shouldn't happen, but can be reported later */
                    case _ => tr
                }
            case _ => tr
        }
    }

    /* returns [Left(errs)] if [spec] and [args] don't match,
     * and returns [Right(mapping)] if they do, where [mapping] maps argument names
     * (from the callee's PoV) to expressions (from the caller's PoV).
     * This function is special in that it doesn't immediately call [logError], but
     * rather returns a set of errors. This is because, even if checking for this
     * particular spec fails, another spec may match. */
    private def checkArgs(
            ast: AST,
            methName: String,
            context: Context,
            spec: Seq[VariableDecl],
            receiver: Expression,
            args: Seq[(ResolvedType, Expression)]): Either[Seq[(AST, Error)], Map[String, Expression]] = {

        val (specL, argsL) = (spec.length, args.length)

        if (specL != argsL) {
            Left((ast, WrongArityError(specL, argsL, methName))::Nil)
        } else {

            // Make the mapping
            var calleeToCaller = TreeMap[String, Expression]()
            for (i <- args.indices) {
                calleeToCaller = calleeToCaller.updated(spec(i).varName, args(i)._2)
            }
            calleeToCaller = calleeToCaller.updated("this", receiver)

            val specCallerPoV = spec.map(arg => {
                translateUncheckedType(spec, arg.typ) match {
                    case prim: PrimitiveType => prim
                    case tr: RawType => toCallerPoV(calleeToCaller, tr) match {
                        case Left((head, e)) =>
                            return Left((ast, CannotConvertPathError(head, e, tr))::Nil)
                        case Right(trNew) => fixRawType(context, trNew)
                    }
                }
            })

            var errList: List[(AST, Error)] = Nil
            for (i <- args.indices) {
                val (argTypeCallerPoV, _) = args(i)

                val result = checkTypeReturnError(context, spec(i).typ, specCallerPoV(i))
                val specTypeCallerPoV = result match {
                    case Left(err) => return Left(err::Nil)
                    case Right(typ) => typ
                }

                if (!isSubtype(argTypeCallerPoV, specTypeCallerPoV)) {
                    val err = SubTypingError(argTypeCallerPoV, specTypeCallerPoV)
                    errList = (ast, err)::errList
                }
            }
            if (errList.isEmpty) Right(calleeToCaller)
            else Left(errList)
        }
    }

    /* takes multiple declarations ([specs]) for a transaction/function/constructor,
     * ensuring that at least one matches the argument types given in [args].
     * [ast] and [methName] are passed in order to generate helpful errors.
     * A member of [U] is attached to each spec to indicate which spec matches.
     * The return value from the successful call to [checkArgs] is also returned */
    private def checkArgs[U](
            ast: AST,
            methName: String,
            context: Context,
            specs: Seq[(Seq[VariableDecl], U)],
            receiver: Expression,
            args: Seq[(ResolvedType, Expression)]): Option[(U, Map[String, Expression])] = {

        var errs: List[(AST, Error)] = Nil
        for ((spec, extraData) <- specs) {
            checkArgs(ast, methName, context, spec, receiver, args) match {
                case Right(calleeToCaller) =>
                    return Some((extraData, calleeToCaller))
                case Left(newErrs) =>
                    errs = newErrs.toList ++ errs
            }
        }
        errs.foreach((err: (AST, Error)) => logError(err._1, err._2))
        None
    }

    private def checkStatement(
                                  decl: InvokableDeclaration,
                                  context: Context,
                                  s: Statement
                              ): Context = {
        def handleInvocation(
                context: Context,
                table: DeclarationTable,
                name: String,
                receiver: Expression,
                args: Seq[Expression]): Context = {
            // Lookup the invocation
            val txLookup = table.lookupTransaction(name)
            val funLookup = table.lookupFunction(name)

            val invokable: InvokableDeclaration = (txLookup, funLookup) match {
                case (None, None) =>
                    val err = MethodUndefinedError(table.simpleType, name)
                    logError(s, err)
                    return context
                case (_, Some(f)) => f
                case (Some(t), _) => t
            }

            // check arguments
            val (argTypes, contextPrime) = inferAndCheckExprs(decl, context, args)
            val specList = (invokable.args, invokable)::Nil

            val (correctInvokable, calleeToCaller) =
                checkArgs(s, name, context, specList, receiver, argTypes) match {
                    case None => return contextPrime
                    case Some(x) => x
                }

            val spec = correctInvokable.args

            val retOpt = correctInvokable.retType

            // check that no ownership is leaked by the (necessarily unused) return value
            if (retOpt.isDefined) {
                translateUncheckedType(spec, retOpt.get) match {
                    case rawType: RawType =>
                        val rawTypeOurPoV = toCallerPoV(calleeToCaller, rawType)
                        // todo : is the [.get] here okay?
                        val retType = checkType(contextPrime, retOpt.get,
                                                fixRawType(contextPrime, rawTypeOurPoV.right.get))
                        if (retType.isInstanceOf[OwnedRef]) {
                            logError(s, LeakReturnValueError(name))
                        }
                    case _ => ()
                }
            }

            contextPrime
        }

        s match {
            case VariableDecl(typ: AstType, name) =>
                context.updated(name, translateType(context, typ))

            case VariableDeclWithInit(typ: AstType, name, e: Expression) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e)
                val tDecl = typ match {
                    case AstContractType(_, cName) => globalTable.contract(cName) match {
                        case None =>
                            logError(s, ContractUndefinedError(cName))
                            BottomType()
                        case Some(_) => translateType(context, typ)
                    }
                    case AstStateType(_, cName, _) => globalTable.contract(cName) match {
                        case None =>
                            logError(s, ContractUndefinedError(cName))
                            BottomType()
                        case Some(_) => translateType(context, typ)
                    }
                    case _ => translateType(context, typ)
                }
                if (tDecl != BottomType()) {
                    checkIsSubtype(s, t, tDecl)
                }
                contextPrime.updated(name, tDecl)

            case Return() =>
                decl match {
                    /* the tx/function must have no return type */
                    case tx: Transaction if tx.retType.isEmpty =>
                        for ((x, t) <- context.underlyingVariableMap) {
                            if (t.isInstanceOf[OwnedRef] && x != "this") {
                                logError(s, UnusedOwnershipError(x))
                            }
                        }
                        context.makeThrown
                    case f: Func if f.retType.isEmpty =>
                        for ((x, t) <- context.underlyingVariableMap) {
                            if (t.isInstanceOf[OwnedRef] && x != "this") {
                                logError(s, UnusedOwnershipError(x))
                            }
                        }
                        context.makeThrown
                    case _ =>
                        logError(s, MustReturnError(decl.name))
                        context.makeThrown
                }

            case ReturnExpr(e: Expression) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e)
                val (tRet, tAst) = decl match {
                    /* must be no return type */
                    case tx: Transaction if tx.retType.isDefined =>
                        for ((x, t) <- context.underlyingVariableMap) {
                            if (t.isInstanceOf[OwnedRef] && x != "this") {
                                e match {
                                    case Variable(xOther) if x == xOther => ()
                                    case _ => logError(s, UnusedOwnershipError(x))
                                }
                            }
                        }
                        (translateUncheckedType(tx.args, tx.retType.get), tx.retType.get)
                    case f: Func if f.retType.isDefined =>
                        for ((x, t) <- context.underlyingVariableMap) {
                            if (t.isInstanceOf[OwnedRef] && x != "this") {
                                e match {
                                    case Variable(xOther) if x == xOther => ()
                                    case _ => logError(s, UnusedOwnershipError(x))
                                }
                            }
                        }
                        (translateUncheckedType(f.args, f.retType.get), f.retType.get)
                    case _ =>
                        logError(s, CannotReturnError(decl.name))
                        return contextPrime.makeThrown
                }

                val expectedRet = tRet match {
                    case tr: RawType => checkType(contextPrime, tAst, tr)
                    case prim: PrimitiveType => prim
                }

                if (!expectedRet.isBottom) checkIsSubtype(s, t, expectedRet)
                contextPrime.makeThrown

            case Transition(newStateName, updates: Seq[(Variable, Expression)]) =>
                val thisTable = context.tableOfThis.contract

                if (thisTable.state(newStateName).isEmpty) {
                    logError(s, StateUndefinedError(thisTable.name, newStateName))
                    return context
                }

                val newStateTable = thisTable.state(newStateName).get

                val oldFields = context.tableOfThis match {
                    case oldStateTable: StateTable =>
                        oldStateTable.ast.declarations
                            .filter(_.isInstanceOf[Field])
                            .map(_.asInstanceOf[Field].name)
                    case _: ContractTable =>
                        /* special case to allow transitioning during constructors */
                        if (decl.isInstanceOf[Constructor]) {
                            TreeSet[String]()
                        } else {
                            logError(s, TransitionError())
                            return context
                        }
                }

                val newFields = newStateTable.ast.declarations
                                .filter(_.isInstanceOf[Field])
                                .map(_.asInstanceOf[Field].name)

                val toInitialize = newFields.toSet.diff(oldFields.toSet)

                val updated = updates.map(_._1.name).toSet
                val uninitialized = toInitialize.diff(updated)
                val badInitializations = updated.diff(newFields.toSet)
                if (uninitialized.nonEmpty) logError(s, TransitionUpdateError(uninitialized))
                for (s <- badInitializations) {
                    val err = FieldUndefinedError(newStateTable.simpleType, s)
                    logError(updates.find(_._1.name == s).get._1, err)
                }

                var contextPrime = context
                for ((Variable(f), e) <- updates) {
                    if (newFields.contains(f)) {
                        val fieldAST = newStateTable.lookupField(f).get
                        val (t, contextPrime2) = inferAndCheckExpr(decl, contextPrime, e)
                        contextPrime = contextPrime2
                        val fieldType = translateType(thisTable, fieldAST.typ)
                        checkIsSubtype(s, t, fieldType)
                    }
                }

                val newSimpleType = StateType(thisTable.name, newStateName)
                val newType = updatedSimpleType(context("this"), newSimpleType)
                contextPrime.updated("this", newType)

            case Assignment(Variable(x), e: Expression) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e)
                val contextType = context.get(s"$x")

                /* if the variable is not in the context, see if it's a field */
                if (contextType.isEmpty) {
                    val thisTable = contextPrime.tableOfThis
                    val fieldLookup = thisTable.lookupField(x)

                    /* if it's not a field either, log an error */
                    if (fieldLookup.isEmpty) logError(s, VariableUndefinedError(x))
                    else checkIsSubtype(e, t, translateType(thisTable, fieldLookup.get.typ))
                }
                else {
                    if (t != BottomType()) {
                        checkIsSubtype(s, t, contextType.get)
                    }

                }
                contextPrime

            case Assignment(Dereference(eDeref, f), e: Expression) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e)
                val (derefType, contextPrime2) = inferAndCheckExpr(decl, contextPrime, eDeref)

                val derefTable = derefType match {
                    case BottomType() => return contextPrime2
                    case IntType() | BoolType() | StringType() =>
                        logError(s, DereferenceError(derefType))
                        return contextPrime2
                    case _ => derefType.tableOpt.get
                }

                val fieldAST = derefTable.lookupField(f) match {
                    case Some(ast) => ast
                    case None =>
                        logError(s, FieldUndefinedError(derefTable.simpleType, f))
                        return contextPrime2
                }

                val fieldType = translateType(derefTable, fieldAST.typ)
                checkIsSubtype(s, t, fieldType)
                contextPrime2

            // assignment target is neither a variable nor a field
            case Assignment(_, e: Expression) =>
                val (_, contextPrime) = inferAndCheckExpr(decl, context, e)
                logError(s, AssignmentError())
                contextPrime

            case Throw() => Context(context.underlyingVariableMap, isThrown = true)

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, eCond)
                checkIsSubtype(s, t, BoolType())
                val contextIfTrue = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body),
                    contextPrime)
                mergeContext(contextPrime.tableOfThis, s, contextPrime, contextIfTrue)

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, eCond)
                checkIsSubtype(s, t, BoolType())
                val contextIfTrue = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body1),
                    contextPrime)
                val contextIfFalse = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body2),
                    contextPrime)
                mergeContext(contextPrime.tableOfThis, s, contextIfFalse, contextIfTrue)

            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) =>
                val contextIfTry = pruneContext(s,
                    checkStatementSequence(decl, context, s1),
                    context)
                val contextIfCatch = pruneContext(s,
                    checkStatementSequence(decl, context, s2),
                    context)
                mergeContext(context.tableOfThis, s, contextIfTry, contextIfCatch)

            case Switch(e: Expression, cases: Seq[SwitchCase]) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e)

                t.tableOpt match {
                    case Some(st: StateTable) => logError(st.ast, AlreadyKnowStateError(e, st.name))
                    case Some(_) => ()
                    case None =>
                        logError(e, SwitchError(t))
                        return contextPrime
                }

                val ContractTable = t.tableOpt.get.contract

                var mergedContext = contextPrime
                for (SwitchCase(sName, body) <- cases) {
                    val newType =
                        ContractTable.state(sName) match {
                            case Some(stTable) =>
                                val newSimple = StateType(ContractTable.name, stTable.name)
                                updatedSimpleType(t, newSimple)
                            case None =>
                                logError(s, StateUndefinedError(ContractTable.name, sName))
                                val newSimple = JustContractType(ContractTable.name)
                                updatedSimpleType(t, newSimple)
                        }

                    /* special case to allow types to change in the context if we match on a variable */
                    val startContext = e match {
                        case This() => contextPrime.updated("this", newType)
                        case Variable(x) => contextPrime.updated(x, newType)
                        case _ => contextPrime
                    }

                    val endContext = pruneContext(s,
                        checkStatementSequence(decl, startContext, body),
                        startContext)
                    mergedContext = mergeContext(contextPrime.tableOfThis, s, mergedContext, endContext)
                }

                mergedContext

            case LocalInvocation(name, args: Seq[Expression]) =>
                val thisTable = context.tableOfThis
                handleInvocation(context, thisTable, name, This(), args)

            case Invocation(receiver: Expression, name, args: Seq[Expression]) =>
                val (receiverType, contextPrime) = inferAndCheckExpr(decl, context, receiver)
                if (receiverType.isBottom) return contextPrime
                val receiverTable = receiverType match {
                    case BottomType() => return contextPrime
                    case IntType() | BoolType() | StringType() =>
                        logError(s, NonInvokeableError(receiverType))
                        return contextPrime
                    case _ => receiverType.tableOpt.get
                }

                handleInvocation(contextPrime, receiverTable, name, receiver, args)

            // TODO maybe allow constructors as statements later, but it's not very important
            /* expressions are statements, but we prune out expressions with no side effects */
            case _ =>
                logError(s, NoEffectsError(s))
                context
        }
    }

    private def checkField(field: Field, lexicallyInsideOf: ContractTable): Unit = {
        def checkNonStateSpecific(simple: SimpleType, err: Error): Unit = {
            simple match {
                case JustContractType(_) => ()
                case StateType(_,_) | StateUnionType(_,_) =>
                    logError(field, err)
            }
        }
        translateType(lexicallyInsideOf, field.typ) match {
            case _: OwnedRef => ()
            case SharedRef(_, tr) =>
                checkNonStateSpecific(tr.extractSimpleType, StateSpecificSharedError())
            case ReadOnlyRef(_, tr) =>
                checkNonStateSpecific(tr.extractSimpleType, StateSpecificReadOnlyError())

            case _ => None
        }
    }

    private def rawTypeOfThis(lexicallyInsideOf: DeclarationTable): RawType = {
        if (lexicallyInsideOf.contract.hasParent) {
            PathType("this"::"parent"::Nil, lexicallyInsideOf.simpleType)
        } else {
            NoPathType(lexicallyInsideOf.simpleType)
        }
    }

    private def checkTransaction(tx: Transaction, lexicallyInsideOf: DeclarationTable): Unit = {

        // first create this unchecked context so we can translate types
        var uncheckedContext = new TreeMap[String, UnresolvedType]()

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- tx.args) {
            val typ = translateUncheckedType(tx.args, arg.typ)
            uncheckedContext = uncheckedContext.updated(arg.varName, typ)
        }

        val thisRawType = rawTypeOfThis(lexicallyInsideOf)

        uncheckedContext = uncheckedContext.updated("this", thisRawType)

        val thisType = OwnedRef(lexicallyInsideOf, thisRawType)
        val cName = lexicallyInsideOf.contract.name

        // Construct the context that the body should start with
        var initContext = Context(new TreeMap[String, ResolvedType](), isThrown = false)
        initContext = initContext.updated("this", thisType)

        // Check that the argument types make sense
        for (arg <- tx.args) {
            val checkedType =
                checkType(uncheckedContext, lexicallyInsideOf, arg.typ, uncheckedContext(arg.varName))
            initContext = initContext.updated(arg.varName, checkedType)
        }

        // Check the body; ensure [this] is well-typed after, and check for leaked ownership
        val outputContext =
            checkStatementSequence(tx, initContext, tx.body)

        val expectedType = OwnedRef(lexicallyInsideOf, NoPathType(JustContractType(cName)))
        checkIsSubtype(tx, outputContext("this"), expectedType)

        if (!hasReturnStatementDontLog(tx.body)) {
            for ((x, t) <- outputContext.underlyingVariableMap) {
                if (t.isInstanceOf[OwnedRef] && x != "this") {
                    logError(tx, UnusedOwnershipError(x))
                }
            }
        }

        // todo: analyze that there is a return in every branch
        if (tx.retType.isDefined & !hasReturnStatement(tx, tx.body)) {
            logError(tx.body.last, MustReturnError(tx.name))
        }

        // todo: check that every declared variable is initialized before use
    }

    private def checkFunc(func: Func, lexicallyInsideOf: Contract): Unit = {
        None // todo
    }

    private def checkState(lexicallyInsideOf: ContractTable, state: State): Unit = {
        val table = lexicallyInsideOf.state(state.name).get
        for (decl <- state.declarations) {
            decl match {
                case t: Transaction => checkTransaction(t, table)
                case f: Field => checkField(f, table.contract)
                case _ => () // TODO
            }
        }
    }

    private def checkConstructor(constr: Constructor, table: ContractTable, hasStates: Boolean): Unit = {

        // maybe this error should be handled in the parser
        if(constr.name != table.name) {
            logError(constr, ConstructorNameError(table.name))
        }

        // first create this unchecked context so we can translate types
        var uncheckedContext = new TreeMap[String, UnresolvedType]()

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- constr.args) {
            val typ = translateUncheckedType(constr.args, arg.typ)
            uncheckedContext = uncheckedContext.updated(arg.varName, typ)
        }

        var initContext = Context(new TreeMap[String, ResolvedType](), isThrown = false)

        //should it be owned?
        val thisType = OwnedRef(table, NoPathType(JustContractType(table.name)))

        initContext = initContext.updated("this", thisType)

        for (arg <- constr.args) {
            val checkedType =
                checkType(uncheckedContext, table, arg.typ, uncheckedContext(arg.varName))
            initContext = initContext.updated(arg.varName, checkedType)
        }

        val outputContext =
            checkStatementSequence(constr, initContext, constr.body)

        val expectedThisType =
            OwnedRef(table, NoPathType(simpleOf(table, table.name, constr.ensuresState)))
        checkIsSubtype(constr, outputContext("this"), expectedThisType)

        for ((x, t) <- outputContext.underlyingVariableMap) {
            if (t.isInstanceOf[OwnedRef] && x != "this") {
                logError(constr, UnusedOwnershipError(x))
            }
        }

        // if the contract contains states, its constructor must contain a state transition
        if (hasStates && !hasTransition(constr.body)) {
            logError(constr, NoStartStateError(constr.name))
        }

    }

    private def checkContract(contract: Contract): Unit = {
        val table = globalTable.contract(contract.name).get
        for (decl <- contract.declarations) {
            decl match {
                case t: Transaction => checkTransaction(t, table)
                case s: State => checkState(table, s)
                case f: Field => checkField(f, table)
                case c: Constructor => checkConstructor(c, table, table.stateLookup.nonEmpty)
                case _ => () // TODO
            }
        }

        if (table.constructors.isEmpty && table.stateLookup.nonEmpty) {
            logError(contract, NoConstructorError(contract.name))
        }
    }

    /* [true] if no type errors, [false] otherwise */
    def checkProgramAndPrintErrors(): Boolean = {
        val errs = checkProgram()

        for ((err, loc) <- errs) {
            val msg = err.msg
            println(s"At $loc: $msg")
        }

        errs.isEmpty
    }

    /* just returns the errors from the program */
    def checkProgram(): Seq[(Error, Position)] = {
        for (contract <- globalTable.ast.contracts) {
            checkContract(contract)
        }

        errors.reverse
    }
}

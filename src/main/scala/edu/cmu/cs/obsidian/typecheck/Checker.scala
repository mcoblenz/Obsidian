package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.{TreeMap, TreeSet, HashSet}
import scala.collection.Map
import scala.util.parsing.input.Position

/* [SimpleType] simply indicates a contract, and possibly a state: there
 * is neither a permission nor a path associated with the type */
sealed trait SimpleType { val contractName: String }
case class JustContractType(contractName: String) extends SimpleType
/* Invariant: [stateNames] is missing at least one of the states of the
 * contract (i.e. it is more specific than [JustContractType(contractName)],
 * but has at least 2 distinct states */
case class StateUnionType(contractName: String, stateNames: Set[String]) extends SimpleType
case class StateType(contractName: String, stateName: String) extends SimpleType

/* [RawType] is a type that doesn't have a permission associated with it */
sealed trait RawType
case class NoPathType(base: SimpleType) extends RawType
case class PathType(path: Seq[String], base: SimpleType) extends RawType

/* This is necessarily different from the representation of types in the AST; it's
* unclear in the AST if a reference is "shared" or "owned" when it has no modifier */
sealed trait Type { val isBottom: Boolean }
case class ReadOnlyRef(t: RawType) extends Type { val isBottom: Boolean = false }
case class SharedRef(t: RawType) extends Type { val isBottom: Boolean = false }
case class OwnedRef(t: RawType) extends Type { val isBottom: Boolean = false }
case class IntType() extends Type { val isBottom: Boolean = false }
case class BoolType() extends Type { val isBottom: Boolean = false }
case class StringType() extends Type { val isBottom: Boolean = false }
/* Used to indicate an error in the type checker when a reasonable type cannot
 * otherwise be inferred */
case class BottomType() extends Type { val isBottom: Boolean = true }

abstract class Error {
    var loc: Position = _
    val msg: String
}

case class SubTypingError(t1: Type, t2: Type) extends Error {
    val msg: String = s"Found type $t1, but expected something of type $t2"
}
case class VariableUndefinedError(x: String) extends Error {
    val msg: String = s"Variable $x is undefined in the current context"
}
case class DifferentTypeError(e1: Expression, t1: Type, e2: Expression, t2: Type) extends Error {
    val msg: String = s"Expression $e1 has type $t1, and expression $e2 has type $t2," +
              s"but these expressions must have the same type"
}
case class FieldUndefinedError(insideOf: SimpleType, fName: String) extends Error {
    val msg: String = insideOf match {
        case JustContractType(cName) => s"Field $fName is not defined in contract $cName"
        case StateUnionType(cName, _) => s"Field $fName is not defined in contract $cName"
        case StateType(cName, sName) => s"Field $fName is not defined in state $sName of contract $cName"
    }
}
case class RecursiveFieldTypeError(cName: String, fName: String) extends Error {
    val msg: String = s"The type of field $fName in contract $cName recursively refers to itself"
}
case class FieldNotConstError(cName: String, fName: String) extends Error {
    val msg: String = s"Field $fName must be labeled 'const' in contract $cName"
}
case class FieldConstMutationError(fName: String) extends Error {
    val msg: String = s"Field $fName cannot be mutated because it is labeled 'const'"
}
case class DereferenceError(typ: Type) extends Error {
    val msg: String = s"Type $typ cannot be dereferenced"
}
case class MethodUndefinedError(insideOf: SimpleType, name: String) extends Error {
    val msg: String = insideOf match {
        case JustContractType(cName) =>
            s"No transaction or function with name $name was found in contract $cName"
        case StateUnionType(cName, _) =>
            s"No transaction or function with name $name was found in contract $cName"
        case StateType(cName, sName) =>
            s"No transaction or function with name $name was found in state $sName of contract $cName"
    }
}
case class StateUndefinedError(cName: String, sName: String) extends Error {
    val msg: String = s"No state with name $sName was found in contract $cName"
}
case class ContractUndefinedError(cName: String) extends Error {
    val msg: String = s"No contract with name $cName is defined"
}
case class NestedContractUndefinedError(cName: String, insideOf_cName: String) extends Error {
    val msg: String = s"No contract with name $cName is defined inside of contract $insideOf_cName"
}
case class NonInvokeableError(t: Type) extends Error {
    val msg: String = s"Cannot invoke functions or transactions on type $t"
}
case class WrongArityError(expected: Int, have: Int, methName: String) extends Error {
    val msg: String =
        if (expected > have) {
            s"Too few arguments supplied to $methName: expected $expected, but found $have"
        } else {
            s"Too many arguments supplied to $methName: expected $expected, but found $have"
        }
}
case class MergeIncompatibleError(name: String, t1: Type, t2: Type) extends Error {
    val msg: String = s"Variable $name is incompatibly typed as both $t1 and $t2 after branch"
}
case class MustReturnError(methName: String) extends Error {
    val msg: String = s"$methName specifies a return type, but no return value is given"
}
case class CannotReturnError(methName: String) extends Error {
    val msg: String = s"$methName does not return anything, but a return value was given"
}
case class NotAValueError(methName: String) extends Error {
    val msg: String = s"$methName does not return anything, but is used here as a value"
}
case class TransitionError() extends Error {
    val msg: String = s"'this' must be typed to a particular state in order to transition"
}
case class TransitionUpdateError(mustSupply: Set[String]) extends Error {
    val msg: String = s"Must specify the following fields in the update clause: $mustSupply"
}
case class AssignmentError() extends Error {
    val msg: String = s"Assignment target must be a variable or a field"
}
case class AlreadyKnowStateError(e: Expression, sName: String) extends Error {
    val msg: String = s"$e is already known to be in state $sName: a dynamic check is not needed"
}
case class LeakReturnValueError(methName: String) extends Error {
    val msg: String = s"Invocation of $methName leaks ownership of return value"
}
case class NoEffectsError(s: Statement) extends Error {
    val msg: String = s"Statement $s has no side-effects"
}
case class StateSpecificSharedError() extends Error {
    val msg: String = s"State-specific types are not safe for 'shared' references"
}
case class StateSpecificReadOnlyError() extends Error {
    val msg: String = s"State-specific types are not safe for 'readonly' references"
}
case class UnusedOwnershipError(name: String) extends Error {
    val msg: String = s"Variable $name holds ownership, but is unused at the end of its scope"
}
case class ConstructorNameError(contractName: String) extends Error {
    val msg: String = s"Invalid constructor name for contract $contractName"
}
case class CannotConvertPathError(badPart: String, expr: Expression, typ: Type) extends Error {
    val msg: String = s"Cannot convert path in type $typ: $badPart is equivalent to" +
        s"a non-variable expression $expr"
}

/* We define a custom type to store a special flag for if a context in after a "throw".
 * In the formalism, we allow throw to result in any type: in the implementation, we don't know
 * which immediately which type this needs to be in order for type checking to work */
case class Context(underlying: Map[String, Type], isThrown: Boolean)  {
    def keys: Iterable[String] = underlying.keys
    def updated(s: String, t: Type): Context = Context(underlying.updated(s, t), isThrown)
    def get(s: String): Option[Type] = underlying.get(s)
    def apply(s: String): Type = underlying(s)
}

class Checker(globalTable: SymbolTable) {

    val errors = new collection.mutable.ArrayStack[Error]()

    /* an error is associated with an AST node to indicate where the error took place */
    private def logError(where: AST, err: Error): Unit = {
        err.loc = where.loc
        errors.push(err)
    }


    //-------------------------------------------------------------------------
    /* [extract] functions retrieve a smaller component type out of a larger one */

    private def extractSimpleType(t: RawType): SimpleType = {
        t match {
            case NoPathType(ts) => ts
            case PathType(_, ts) => ts
        }
    }

    private def extractSimpleType(t: Type): Option[SimpleType] = {
        t match {
            case OwnedRef(tr) => Some(extractSimpleType(tr))
            case SharedRef(tr) => Some(extractSimpleType(tr))
            case ReadOnlyRef(tr) => Some(extractSimpleType(tr))
            case _ => None
        }
    }

    private def extractRawType(t: Type): Option[RawType] = {
        t match {
            case OwnedRef(tr) => Some(tr)
            case SharedRef(tr) => Some(tr)
            case ReadOnlyRef(tr) => Some(tr)
            case _ => None
        }
    }

    //-------------------------------------------------------------------------
    /* [update] functions replace one instance of a smaller component type for
     * another instance within the same larger type */

    private def updateSimpleType(t: RawType, newSimple: SimpleType): RawType = {
        t match {
            case NoPathType(_) => NoPathType(newSimple)
            case PathType(p, _) => PathType(p, newSimple)
        }
    }

    private def updateSimpleType(t: Type, newSimple: SimpleType): Type = {
        t match {
            case OwnedRef(tr) => OwnedRef(updateSimpleType(tr, newSimple))
            case SharedRef(tr) => SharedRef(updateSimpleType(tr, newSimple))
            case ReadOnlyRef(tr) => ReadOnlyRef(updateSimpleType(tr, newSimple))
            case ts => ts
        }
    }

    private def updateRawType(t: Type, newRaw: RawType): Type = {
        t match {
            case OwnedRef(_) => OwnedRef(newRaw)
            case SharedRef(_) => SharedRef(newRaw)
            case ReadOnlyRef(_) => ReadOnlyRef(newRaw)
            case ts => ts
        }
    }

    //-------------------------------------------------------------------------
    /* Subtyping definitions */

    /* true iff [t1 <: t2] */
    private def simpleSubTypeOf(t1: SimpleType, t2: SimpleType): Boolean = {
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

    private def rawSubTypeOf(t1: RawType, t2: RawType): Boolean = {
        (t1, t2) match {
            case (NoPathType(ts1), NoPathType(ts2)) => simpleSubTypeOf(ts1, ts2)
            case (PathType(p1, ts1), PathType(p2, ts2)) if p1 == p2 =>
                simpleSubTypeOf(ts1, ts2)
            case _ => false
        }
    }

    /* true iff [t1 <: t2] */
    private def subTypeOf(t1: Type, t2: Type): Boolean = {
        (t1, t2) match {
            case (BottomType(), _) => true
            case (IntType(), IntType()) => true
            case (BoolType(), BoolType()) => true
            case (StringType(), StringType()) => true
            case (OwnedRef(tr1), OwnedRef(tr2)) => rawSubTypeOf(tr1, tr2)
            case (ReadOnlyRef(tr1), ReadOnlyRef(tr2)) => rawSubTypeOf(tr1, tr2)
            case (SharedRef(tr1), SharedRef(tr2)) => rawSubTypeOf(tr1, tr2)
            case _ => false
        }
    }

    /* returns [t1] if [t1 <: t2], logs an error and returns [BottomType] otherwise */
    private def assertSubType(ast: AST, t1: Type, t2: Type): Type = {
        if (!subTypeOf(t1, t2)) {
            logError(ast, SubTypingError(t1, t2))
            BottomType()
        }
        else t1
    }

    //-------------------------------------------------------------------------
    /* Helper functions to easily make new types */

    /* Determines what sort of simple type should be used, given a set a possible states */
    private def simpleOf(cName: String, states: Set[String]): SimpleType = {
        val possibleStates = globalTable.contract(cName).get.possibleStates
        if (states == possibleStates) {
            JustContractType(cName)
        } else if (states.size > 1) {
            StateUnionType(cName, states)
        } else {
            StateType(cName, states.head)
        }
    }

    /* just like the above, but accepts [states] in the format of [ensures] clauses */
    private def simpleOf(cName: String, states: Option[Set[String]]): SimpleType = {
        states match {
            case None => JustContractType(cName)
            case Some(ss) => simpleOf(cName, ss)
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

    private def simpleUpperBound(t1: SimpleType, t2: SimpleType): Option[SimpleType] = {
        def handleStateUnion(ss1: Set[String], ss2: Set[String]): Option[SimpleType] = {
            val c = t1.contractName
            val unionStates = ss1.union(ss2)
            Some(simpleOf(c, unionStates))
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

    private def rawUpperBound(t1: RawType, t2: RawType): Option[RawType] = {
        (t1, t2) match {
            case (NoPathType(ts1), NoPathType(ts2)) =>
                simpleUpperBound(ts1, ts2).map(NoPathType)
            case (PathType(p1, ts1), PathType(p2, ts2)) if p1 == p2 =>
                simpleUpperBound(ts1, ts2).map(PathType(p1, _))
            case _ => None
        }
    }

    private def upperBound(t1: Type, t2: Type): Option[Type] = {
        (t1, t2) match {
            case (IntType(), IntType()) => Some(IntType())
            case (BoolType(), BoolType()) => Some(BoolType())
            case (StringType(), StringType()) => Some(StringType())
            case (OwnedRef(tr1), OwnedRef(tr2)) =>
                rawUpperBound(tr1, tr2).flatMap(s => Some(OwnedRef(s)))
            case (ReadOnlyRef(tr1), ReadOnlyRef(tr2)) =>
                rawUpperBound(tr1, tr2).flatMap(s => Some(ReadOnlyRef(s)))
            case (SharedRef(tr1), SharedRef(tr2)) =>
                rawUpperBound(tr1, tr2).flatMap(s => Some(SharedRef(s)))
            case _ => None
        }
    }

    private def addModifier(tr: RawType, mods: Seq[TypeModifier], contractName: String): Type = {
        // TODO: None.get error if there is no contract with contractName
        val contract = globalTable.contract(contractName).get.ast
        // TODO: what is the behavior of a contract that is not labeled?
        val mod = contract.mod match {
            case Some(m) => m
            case None => IsOwned()
        }

        /* if a reference is 'readonly', it is labeled as such; otherwise, it is 'owned'/'shared', based on the
         * declaration of the contract itself */
        if (mods.exists(_.isInstanceOf[IsReadOnly])) {
            ReadOnlyRef(tr)
        } else if (mod.isInstanceOf[IsOwned]) {
            OwnedRef(tr)
        } else {
            // TODO: are main contracts always deemed shared by the type system?
            SharedRef(tr)
        }
    }

    //-------------------------------------------------------------------------
    /* [translateType] functions map from [AstType] instances to the new type
     * definitions defined at the top of this file */

    private def translateTypeHelper(
                    convertPath: Function[Seq[String], Seq[String]],
                    t: AstType
                ): Type = {
        t match {
            case AstIntType() => IntType()
            case AstBoolType() => BoolType()
            case AstStringType() => StringType()
            case AstContractType(mods, name) =>
                addModifier(NoPathType(JustContractType(name)), mods, name)
            case AstStateType(mods, cName, sName) =>
                addModifier(NoPathType(StateType(cName, sName)), mods, cName)
            case AstPathContractType(mods, p, name) =>
                addModifier(PathType(convertPath(p), JustContractType(name)), mods, name)
            case AstPathStateType(mods, p, cName, sName) =>
                addModifier(PathType(convertPath(p), StateType(cName, sName)), mods, cName)
        }
    }

    /* The following definitions (for [unsafeTranslateType]) do not check that
     * a type is valid: they merely translate an AST type to a possibly-valid
     * [Type] object. The [Type] object must be [checkType]d in order to
     * ensure, e.g. that the contract name resolves to something meaningful. */

    /* Used to translate a type that occurs in a method declaration (e.g. the
     * return value, or the type of one of the arguments) */
    private def unsafeTranslateType(args: Seq[VariableDecl], t: AstType): Type = {
        val argsAsSet = args.map(_.varName).toSet
        def convertPath(p: Seq[String]): Seq[String] = {
            if ((argsAsSet contains p.head) || p.head == "this") p
            else "this" +: p
        }
        translateTypeHelper(convertPath, t)
    }

    /* Used to translate a type that occurs as a field */
    private def unsafeTranslateType(t: AstType): Type = {
        def convertPath(p: Seq[String]): Seq[String] = {
            if (p.head != "this")  "this" +: p
            else p
        }
        translateTypeHelper(convertPath, t)
    }

    private def unsafeTranslateType(context: Context, t: AstType): Type = {
        def convertPath(p: Seq[String]): Seq[String] = {
            // try two interpretations: in path [x.y.z], [x] refers to a field or variable
            context.get(p.head) match {
                case Some(_) | None if p.head == "this" => p
                case None => "this" +: p
            }
        }
        translateTypeHelper(convertPath, t)
    }

    /* Attempts to translate a type and then check that it's valid.
     * If the type is not valid, returns [BottomType] */

    private def translateType(context: Context, t: AstType): Type = {
        val typ = unsafeTranslateType(context, t)
        checkType(t, context, typ)
    }

    private def translateType(insideOf: DeclarationTable, t: AstType): Type = {
        val typ = unsafeTranslateType(t)
        checkType(t, insideOf, typ)
    }

    //-------------------------------------------------------------------------
    /* [tableOf] functions retrieve the symbol table corresponding to a type
     * (see SymbolTable.scala for definitions) */

    private def tableOfThis(context: Context): DeclarationTable = {
        extractSimpleType(context("this")).get match {
            case JustContractType(c) => globalTable.contract(c).get
            case StateUnionType(c, _) => globalTable.contract(c).get
            case StateType(c, s) => globalTable.contract(c).get.state(s).get
        }
    }

    /* Like [indexedOf], but only works for contracts that are globally defined
     * (i.e. not nested contracts) */
    private def globalTableOf(t: Type): Option[DeclarationTable] = {
        extractSimpleType(t) match {
            case Some(JustContractType(cName)) =>
                globalTable.contract(cName).flatMap((c: ContractTable) => {
                    Some(c)
                })
            case Some(StateUnionType(cName, _)) =>
                globalTable.contract(cName).flatMap((c: ContractTable) => {
                    Some(c)
                })
            case Some(StateType(cName, sName)) =>
                globalTable.contract(cName).flatMap((c: ContractTable) => {
                    c.state(sName).flatMap((s: StateTable) => {
                        Some(s)
                    })
                })
            case None => None
        }
    }

    /* Invariant for [indexedOf]: always returns Some(_) if the analogous call
     * to [checkType] on [t] (i.e. using [context]/[insideOf]) doesn't return
     * [BottomType] */

    private def tableOf(context: Context, t: Type): Option[DeclarationTable] = {
        extractRawType(t) match {

            case Some(PathType(x +: pRest, ts)) =>
                if (!(context.underlying contains x)) {
                    return None
                }
                val newInsideOf = tableOf(context, context(x)).get.contract
                val newPathType = PathType(pRest, ts)
                tableOf(newInsideOf, updateRawType(t, newPathType))

            // Shouldn't happen
            case Some(PathType(Seq(), _)) => None
            case Some(NoPathType(_)) => globalTableOf(t)
            case None => None
        }
    }

    private def tableOf(insideOf: DeclarationTable, t: Type): Option[DeclarationTable] = {
        tableOf(insideOf, HashSet[(DeclarationTable, String)](), t)
    }

    /* like the [checkType] functions below, we must track which fields we have
     * already visited in trying to resolve a path, otherwise we may loop forever */
    private def tableOf(
            insideOf: DeclarationTable,
            visitedFields: Set[(DeclarationTable, String)],
            t: Type): Option[DeclarationTable] = {
        extractRawType(t) match {
            case Some(PathType(f +: pRest, ts)) =>

                if (visitedFields contains (insideOf, f)) {
                    return None
                }

                val newVisited = visitedFields.+((insideOf, f))

                val fieldLookup = insideOf.field(f)
                if (fieldLookup.isEmpty) return None

                val field = fieldLookup.get
                val fieldType = translateType(insideOf, field.typ)

                val newInsideOfLookup = tableOf(insideOf, fieldType)
                if (newInsideOfLookup.isEmpty) return None
                val newInsideOf = newInsideOfLookup.get.contract

                val newPathType = PathType(pRest, ts)
                tableOf(newInsideOf, newVisited, updateRawType(t, newPathType))

            case Some(PathType(Seq(), JustContractType(cName))) =>
                insideOf.contract(cName).flatMap((c: ContractTable) => {
                    Some(c)
                })
            case Some(PathType(Seq(), StateUnionType(cName, _))) =>
                insideOf.contract(cName).flatMap((c: ContractTable) => {
                    Some(c)
                })
            case Some(PathType(Seq(), StateType(cName, sName))) =>
                insideOf.contract(cName).flatMap((c: ContractTable) => {
                    c.state(sName).flatMap((s: StateTable) => {
                        Some(s)
                    })
                })
            case Some(NoPathType(_)) => globalTableOf(t)
            case None => None
        }
    }

    //-------------------------------------------------------------------------
    /* [checkType] functions check that a type is valid: e.g. that the path
     * resolves, that any states/contracts named in the type exist. These functions
     * log errors using [logError] as they encounter them */

    /* Checks that the type [ts] makes sense as the end of a path that leads to
     * the contract [insideOf]. */
    private def checkSimpleType(
                                   ast: AST,
                                   insideOf: DeclarationTable,
                                   ts: SimpleType): Boolean = {

        val cName = ts.contractName
        val ctLookup = insideOf.contract(cName)

        if (ctLookup.isEmpty) {
            logError(ast, NestedContractUndefinedError(cName, insideOf.name))
            return false
        }

        def checkState(sName: String): Boolean = {
            val stLookup = ctLookup.get.state(sName)
            if (stLookup.isEmpty) {
                logError(ast, StateUndefinedError(cName, sName))
                false
            } else true
        }

        ts match {
            case StateType(_, sName) => checkState(sName)
            case StateUnionType(_, sNames) => sNames.forall(checkState)
            case _ => true
        }
    }


    /* Checks that the type [ts] makes sense globally; i.e. it refers to a globally scoped
     * contract, and any states referenced in the type are valid states of this contract */
    private def checkSimpleType(ast: AST, ts: SimpleType): Boolean = {

        val cName = ts.contractName
        val ctLookup = globalTable.contract(cName)

        if (ctLookup.isEmpty) {
            logError(ast, ContractUndefinedError(cName))
            return false
        }

        def checkState(sName: String): Boolean = {
            val stLookup = ctLookup.get.state(sName)
            if (stLookup.isEmpty) {
                logError(ast, StateUndefinedError(cName, sName))
                false
            } else true
        }

        ts match {
            case StateType(_, sName) => checkState(sName)
            case StateUnionType(_, sNames) => sNames.forall(checkState)
            case _ => true
        }
    }

    def checkRawType(ast: AST, insideOf: DeclarationTable, tr: RawType): Boolean = {
        checkRawType(ast, insideOf, HashSet[(DeclarationTable, String)](), tr)
    }

    /* Checks [RawType]s that occur where no context exists; e.g. in field types.
     * [alreadyVisited] prevents looping forever on malicious cases: e.g. the program
     * [contract C { f.T f; }]
     * We have to check field [f] has a valid type to check the type [f.T].
     * But field [f] has type [f.T], so we recursively check [f.T] forever.
     */
    def checkRawType(
                        ast: AST,
                        insideOf: DeclarationTable,
                        visitedFields: Set[(DeclarationTable, String)],
                        tr: RawType): Boolean = {
        tr match {
            case NoPathType(ts) => checkSimpleType(ast, ts)
            case PathType(p, ts) =>
                val actualP = if (p.head == "this") p.tail else p
                if (actualP.isEmpty) {
                    return checkSimpleType(ast, insideOf, ts)
                }

                val f = actualP.head
                val pPrime = actualP.tail

                val fieldLookup = insideOf.field(f)
                if (fieldLookup.isEmpty) {
                    logError(ast, FieldUndefinedError(insideOf.simpleTypeOf, f))
                    return false
                }

                val field = fieldLookup.get

                // paths must consist entirely of [const] fields
                if (!field.isConst) {
                    logError(field, FieldNotConstError(insideOf.name, f))
                    return false
                }

                if (visitedFields contains (insideOf, f)) {
                    logError(field, RecursiveFieldTypeError(insideOf.name, f))
                    return false
                }

                val newVisited = visitedFields.+((insideOf, f))

                val fieldType = translateType(insideOf, field.typ)
                if (fieldType == BottomType()) return false

                /* we can assume we got something, because we did [checkRawType] first */
                val newInsideOf = tableOf(insideOf, fieldType).get.contract

                checkRawType(ast, newInsideOf, newVisited, PathType(pPrime, ts))
        }
    }

    /* checks a raw type that occurs in places where a context exists;
     * for example, a method body, or in argument declarations */
    private def checkRawType(
            ast: AST,
            context: Context,
            visitedLocalVars: Set[String],
            tr: RawType): Boolean = {
        tr match {
            case NoPathType(ts) => checkSimpleType(ast, ts)
            case PathType(x::_, _) if context.underlying contains x =>

                if (visitedLocalVars contains x) return false
                val newVisited = visitedLocalVars + x

                val pathHeadType = context(x)
                if (checkType(ast, context, newVisited, pathHeadType) == BottomType()) {
                    return false
                }

                val newInsideOf = tableOf(context, pathHeadType).get.contract
                checkRawType(ast, newInsideOf, tr)

            case PathType(x::_, _) =>
                logError(ast, VariableUndefinedError(x))
                false

            // shouldn't happen
            case PathType(Nil, _) => false
        }
    }

    /* outputs either [t], or [BottomType] (the latter iff there's an error) */

    private def checkType(ast: AST, insideOf: DeclarationTable, t: Type): Type = {
        extractRawType(t) match {
            case Some(tr) =>
                if (!checkRawType(ast, insideOf, tr))
                    BottomType()
                else t
            case None => t
        }
    }

    private def checkType(ast: AST, context: Context, t: Type): Type = {
        checkType(ast, context, TreeSet[String](), t)
    }

    private def checkType(
            ast: AST,
            context: Context,
            visitedLocalVars: Set[String],
            t: Type): Type = {
        extractRawType(t) match {
            case Some(tr) =>
                if (!checkRawType(ast, context, visitedLocalVars, tr))
                    BottomType()
                else t
            case None => t
        }
    }

    //-------------------------------------------------------------------------
    // Checking definitions for language constructs begins here

    private def checkExpr(
            decl: InvokableDeclaration,
            context: Context,
            e: Expression): (Type, Context) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: Type, c: Context): (Type, Context) = {
            val (tPrime, contextPrime) = checkExpr(decl, c, e)
            (assertSubType(e, tPrime, t), contextPrime)
        }

        def assertOperationType(e1: Expression, e2: Expression, t: Type): (Type, Context) = {
            val (_, c1) = assertTypeEquality(e1, t, context)
            val (_, c2) = assertTypeEquality(e2, t, c1)
            (t, c2)
        }

        def assertComparisonType(e1: Expression, e2: Expression): (Type, Context) = {
            val (_, c1) = assertTypeEquality(e1, IntType(), context)
            val (_, c2) = assertTypeEquality(e2, IntType(), c1)
            (BoolType(), c2)
        }

        def handleInvocation(
                context: Context,
                table: DeclarationTable,
                name: String,
                args: Seq[Expression]): (Type, Context) = {
            // Lookup the invocation
            val txLookup = table.transaction(name)
            val funLookup = table.function(name)

            val invokable: InvokableDeclaration = (txLookup, funLookup) match {
                case (None, None) =>
                    val err = MethodUndefinedError(table.simpleTypeOf, name)
                    logError(e, err)
                    return (BottomType(), context)
                case (_, Some(f)) => f
                case (Some(t), _) => t
            }

            // check arguments
            val (argTypes, contextPrime) = checkExprs(decl, context, args)
            val specList = (invokable.args, invokable)::Nil

            val (correctInvokable, calleeToCaller) =
                assertArgsCorrect(e, name, specList, argTypes) match {
                    case None => return (BottomType(), contextPrime)
                    case Some(x) => x
                }

            // check that there's a value to return
            if (correctInvokable.retType.isEmpty) {
                logError(e, NotAValueError(name))
                return (BottomType(), contextPrime)
            }

            val spec = correctInvokable.args

            val retTypeCalleePoV = unsafeTranslateType(spec, correctInvokable.retType.get)

            fromInvocationPoV(calleeToCaller, retTypeCalleePoV) match {
                case Right(retTypeCallerPoV) =>
                    (checkType(e, contextPrime, retTypeCallerPoV), contextPrime)
                case Left(first) =>
                    val badExpr = calleeToCaller(first)
                    val err = CannotConvertPathError(first, badExpr, retTypeCalleePoV)
                    logError(e, err)
                    (BottomType(), contextPrime)
            }
        }

         e match {
             case Variable(x) =>
                 (context get x, tableOfThis(context).field(x)) match {
                     case (Some(t), _) => t match {
                         case OwnedRef(simp) => (OwnedRef(simp), context.updated(x, ReadOnlyRef(simp)))
                         case _ => (t, context)
                     }
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
                 val baseType = NoPathType(tableOfThis(context).simpleTypeOf)
                 decl match {
                     // if we're in a transaction (or constructor?), we can consider [this] to be [owned]
                     case t: Transaction =>
                         (OwnedRef(baseType), context)
                     case c: Constructor =>
                         (OwnedRef(baseType), context)
                     case f: Func => (ReadOnlyRef(baseType), context)
                     // if we're in a function, [this] must be deemed [readonly]
                     case _ => (ReadOnlyRef(baseType), context)
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
                 val (t1, c1) = checkExpr(decl, context, e1)
                 val (t2, c2) = checkExpr(decl, c1, e2)
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
                 val (t1, c1) = checkExpr(decl, context, e1)
                 val (t2, c2) = checkExpr(decl, c1, e2)
                 if (t1 == t2) (BoolType(), c2) else {
                     logError(e, DifferentTypeError(e1, t1, e2, t2))
                     (BottomType(), c2)
                 }

             case Dereference(eDeref: Expression, f) =>
                 val (derefType, contextPrime) = checkExpr(decl, context, eDeref)

                 val derefTable = derefType match {
                     case BottomType() => return (BottomType(), contextPrime)
                     case IntType() | BoolType() | StringType() =>
                         logError(e, DereferenceError(derefType))
                         return (BottomType(), contextPrime)
                     case _ => tableOf(context, derefType).get
                 }

                 val fieldAST = derefTable.field(f) match {
                     case Some(ast) => ast
                     case None =>
                         logError(e, FieldUndefinedError(derefTable.simpleTypeOf, f))
                         return (BottomType(), contextPrime)
                 }

                 (translateType(derefTable, fieldAST.typ), contextPrime)

             case LocalInvocation(name, args: Seq[Expression]) =>
                 val thisTable = tableOfThis(context)
                 handleInvocation(context, thisTable, name, args)

             case Invocation(recipient: Expression, name, args: Seq[Expression]) =>
                 val (recipType, contextPrime) = checkExpr(decl, context, recipient)

                 val recipTable = recipType match {
                     case BottomType() => return (BottomType(), contextPrime)
                     case IntType() | BoolType() | StringType() =>
                         logError(e, NonInvokeableError(recipType))
                         return (BottomType(), contextPrime)
                     case _ => tableOf(contextPrime, recipType).get
                 }

                 handleInvocation(contextPrime, recipTable, name, args)

             case Construction(name, args: Seq[Expression]) =>
                 val tableLookup = tableOfThis(context).contract(name)
                 if (tableLookup.isEmpty) {
                     logError(e, ContractUndefinedError(name))
                     return (BottomType(), context)
                 }

                 val tableOfConstructed = tableLookup.get

                 var path: Option[Seq[String]] = null
                 if (tableOfConstructed.hasParent) path = Some("this"::Nil)
                 else path = None

                 val (argTypes, contextPrime) = checkExprs(decl, context, args)
                 val constrSpecs = tableOfConstructed
                                    .constructors
                                    .map(constr => (constr.args, constr))

                 val result =
                     assertArgsCorrect(e, s"constructor of $name", constrSpecs, argTypes)

                 val simpleType = result match {
                     // Even if the args didn't check, we can still output a type
                     case None => JustContractType(name)
                     case Some((constr, _)) => simpleOf(name, constr.ensuresState)
                 }

                 val rawType = rawOf(simpleType, path)

                 tableOfConstructed.ast.mod match {
                     case Some(IsOwned()) =>
                         (OwnedRef(rawType), contextPrime)
                     case Some(IsShared()) =>
                         (SharedRef(rawType), contextPrime)
                     case Some(IsMain()) =>
                         // todo : is the "main" contract always shared?
                         (SharedRef(rawType), contextPrime)
                     case None =>
                         // todo : we need to decide what the default is (if any)
                         (SharedRef(rawType), contextPrime)
                 }
         }
    }


    private def checkExprs(
                                    decl: InvokableDeclaration,
                                    context: Context,
                                    es: Seq[Expression]
                                ): (Seq[(Type, Expression)], Context) = {
        val types = new ListBuffer[(Type, Expression)]()
        var contextPrime = context
        for (e <- es) {
            val (t, contextPrime2) =
                checkExpr(decl, contextPrime, e)
            contextPrime = contextPrime2
            types.append((t, e))
        }
        (types, contextPrime)
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
                case OwnedRef(_) => logError(ast, UnusedOwnershipError(x))
                case _ => ()
            }
        }

        Context(newContext.underlying, isThrown = branchContext.isThrown)
    }

    private def mergeContext(ast: AST, context1: Context, context2: Context): Context = {
        /* If we're merging with a context from a "throw", just take the other context
        * emit no errors */
        if (context1.isThrown && !context2.isThrown) return context2
        if (!context1.isThrown && context2.isThrown) return context1

        var mergedMap = new TreeMap[String, Type]()

        val inBoth = context1.keys.toSet.intersect(context2.keys.toSet)

        for (x <- inBoth) {
            val t1 = context1(x)
            val t2 = context2(x)
            upperBound(t1, t2) match {
                case Some(u) => mergedMap = mergedMap.updated(x, u)
                case None =>
                    logError(ast, MergeIncompatibleError(x, t1, t2))
            }
        }

        Context(mergedMap, context1.isThrown && context2.isThrown)
    }

    /* this returns a _set_ of types because there's not a single unique type in general.
     * Example:
     *
     * transaction t(C a, C b, a.T p, b.T q)
     * t(x, x, y, y)
     *
     * In this above example, [y] could have type [a.T] and [b.T] when translated
     * to the method declaration's point of view.
     */
    private def toInvocationPoV(mapping: Map[String, Expression], t: Type): Set[Type] = {
        extractRawType(t) match {
            case Some(PathType(p, ts)) =>
                val first = p.head

                val matchesFirst = mapping.filter((entry: (String, Expression)) => {
                    entry._2 match {
                        case Variable(x) => first == x
                        case This() => first == "this"
                        case _ => false
                    }
                })

                matchesFirst.toSet.map((entry: (String, Expression)) => {
                    val newPath = entry._1 +: p.tail
                    val newPathType = PathType(newPath, ts)
                    updateRawType(t, newPathType)
                })

            case _ => HashSet[Type]() + t
        }
    }

    /* Returns [Left(p.head)] where [p] is the head of the path if failure,
     * otherwise [Right(t)] where [t] is the type from the perspective of the caller */
    private def fromInvocationPoV(
            mapping: Map[String, Expression],
            t: Type): Either[String, Type] = {
        extractRawType(t) match {
            case Some(PathType(p, ts)) =>
                val newPathType = mapping(p.head) match {
                    case Variable(x) => PathType(x +: p.tail, ts)
                    case This() => PathType("this" +: p.tail, ts)
                    case _ => return Left(p.head)
                }
                Right(updateRawType(t, newPathType))
            case _ => Right(t)
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
            spec: Seq[VariableDecl],
            args: Seq[(Type, Expression)]): Either[Seq[(AST, Error)], Map[String, Expression]] = {

        val (specL, argsL) = (spec.length, args.length)

        if (specL != argsL) {
            Left((ast, WrongArityError(specL, argsL, methName))::Nil)
        } else {

            // Make the mapping
            var calleeToCaller = TreeMap[String, Expression]()
            for (i <- args.indices) {
                calleeToCaller = calleeToCaller.updated(spec(i).varName, args(i)._2)
            }

            val argsCalleePoV = args.map(te => {
                (toInvocationPoV(calleeToCaller, te._1), te._2)
            })

            val specIter = spec.toIterator
            var errList: List[(AST, Error)] = Nil
            for (i <- args.indices) {
                val (possibleTypesCalleePoV, _) = argsCalleePoV(i)
                val (typeCallerPoV, _) = args(i)

                val expectedType = unsafeTranslateType(spec, specIter.next().typ)
                val matches = possibleTypesCalleePoV.exists(subTypeOf(_, expectedType))
                if (!matches) {
                    /* We use the type from the caller's point of view because
                     * there's a unique caller-PoV type to report */
                    val err = SubTypingError(typeCallerPoV, expectedType)
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
    private def assertArgsCorrect[U](
            ast: AST,
            methName: String,
            specs: Seq[(Seq[VariableDecl], U)],
            args: Seq[(Type, Expression)]): Option[(U, Map[String, Expression])] = {

        var errs: List[(AST, Error)] = Nil
        for ((spec, extraData) <- specs) {
            checkArgs(ast, methName, spec, args) match {
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
                args: Seq[Expression]): Context = {
            // Lookup the invocation
            val txLookup = table.transaction(name)
            val funLookup = table.function(name)

            val invokable: InvokableDeclaration = (txLookup, funLookup) match {
                case (None, None) =>
                    val err = MethodUndefinedError(table.simpleTypeOf, name)
                    logError(s, err)
                    return context
                case (_, Some(f)) => f
                case (Some(t), _) => t
            }

            // check arguments
            val (argTypes, contextPrime) = checkExprs(decl, context, args)
            val specList = (invokable.args, invokable)::Nil

            val (correctInvokable, _) =
                assertArgsCorrect(s, name, specList, argTypes) match {
                    case None => return contextPrime
                    case Some(x) => x
                }

            val spec = correctInvokable.args

            val retOpt = correctInvokable.retType
            // check that no ownership is leaked by the (necessarily unused) return value
            if (retOpt.isDefined
                && unsafeTranslateType(spec, retOpt.get).isInstanceOf[OwnedRef]) {
                logError(s, LeakReturnValueError(name))
            }

            contextPrime
        }

        s match {
            case VariableDecl(typ: AstType, name) =>
                context.updated(name, translateType(context, typ))

            case VariableDeclWithInit(typ: AstType, name, e: Expression) =>
                val (t, contextPrime) = checkExpr(decl, context, e)
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
                    assertSubType(s, t, tDecl)
                }
                contextPrime.updated(name, tDecl)

            case Return() =>
                decl match {
                    /* the tx/function must have no return type */
                    case t: Transaction if t.retType.isEmpty => context
                    case f: Func if f.retType.isEmpty => context
                    case _ =>
                        logError(s, MustReturnError(decl.name))
                        context
                }

            case ReturnExpr(e: Expression) =>
                val (t, contextPrime) = checkExpr(decl, context, e)
                val tRet = decl match {
                    /* must be no return type */
                    case t: Transaction if t.retType.isDefined =>
                        unsafeTranslateType(t.args, t.retType.get)
                    case f: Func if f.retType.isDefined =>
                        unsafeTranslateType(f.args, f.retType.get)
                    case _ =>
                        logError(s, CannotReturnError(decl.name))
                        return contextPrime
                }
                assertSubType(s, t, tRet)
                contextPrime

            case Transition(newStateName, updates: Seq[(Variable, Expression)]) =>
                val thisTable = tableOfThis(context).contract

                if (thisTable.state(newStateName).isEmpty) {
                    logError(s, StateUndefinedError(thisTable.name, newStateName))
                    return context
                }

                val newStateTable = thisTable.state(newStateName).get

                val oldFields = tableOfThis(context) match {
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
                    val err = FieldUndefinedError(extractSimpleType(context("this")).get, s)
                    logError(updates.find(_._1.name == s).get._1, err)
                }

                var contextPrime = context
                for ((Variable(f), e) <- updates) {
                    if (newFields.contains(f)) {
                        val fieldAST = newStateTable.field(f).get
                        val (t, contextPrime2) = checkExpr(decl, contextPrime, e)
                        contextPrime = contextPrime2
                        val fieldType = translateType(thisTable, fieldAST.typ)
                        assertSubType(s, t, fieldType)
                    }
                }

                val newSimpleType = StateType(thisTable.name, newStateName)
                val newType = updateSimpleType(context("this"), newSimpleType)
                contextPrime.updated("this", newType)

            case Assignment(Variable(x), e: Expression) =>
                val (t, contextPrime) = checkExpr(decl, context, e)
                val contextType = context.get(s"$x")

                /* if the variable is not in the context, see if it's a field */
                if (contextType.isEmpty) {
                    val thisTable = tableOfThis(contextPrime)
                    val fieldLookup = thisTable.field(x)

                    /* if it's not a field either, log an error */
                    if (fieldLookup.isEmpty) logError(s, VariableUndefinedError(x))
                    else assertSubType(e, t, translateType(thisTable, fieldLookup.get.typ))
                }
                else {
                    if (t != BottomType()) {
                        assertSubType(s, t, contextType.get)
                    }

                }
                contextPrime

            case Assignment(Dereference(eDeref, f), e: Expression) =>
                val (t, contextPrime) = checkExpr(decl, context, e)
                val (derefType, contextPrime2) = checkExpr(decl, contextPrime, eDeref)

                val derefTable = derefType match {
                    case BottomType() => return contextPrime2
                    case IntType() | BoolType() | StringType() =>
                        logError(s, DereferenceError(derefType))
                        return contextPrime2
                    case _ => tableOf(context, derefType).get
                }

                val fieldAST = derefTable.field(f) match {
                    case Some(ast) => ast
                    case None =>
                        logError(s, FieldUndefinedError(derefTable.simpleTypeOf, f))
                        return contextPrime2
                }

                val fieldType = translateType(derefTable, fieldAST.typ)
                assertSubType(s, t, fieldType)
                contextPrime2

            // assignment target is neither a variable nor a field
            case Assignment(_, e: Expression) =>
                val (_, contextPrime) = checkExpr(decl, context, e)
                logError(s, AssignmentError())
                contextPrime

            case Throw() => Context(context.underlying, isThrown = true)

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(decl, context, eCond)
                assertSubType(s, t, BoolType())
                val contextIfTrue = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body),
                    contextPrime)
                mergeContext(s, contextPrime, contextIfTrue)

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(decl, context, eCond)
                assertSubType(s, t, BoolType())
                val contextIfTrue = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body1),
                    contextPrime)
                val contextIfFalse = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body2),
                    contextPrime)
                mergeContext(s, contextIfFalse, contextIfTrue)

            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) =>
                val contextIfTry = pruneContext(s,
                    checkStatementSequence(decl, context, s1),
                    context)
                val contextIfCatch = pruneContext(s,
                    checkStatementSequence(decl, context, s2),
                    context)
                mergeContext(s, contextIfTry, contextIfCatch)

            case Switch(e: Expression, cases: Seq[SwitchCase]) =>
                val (t, contextPrime) = checkExpr(decl, context, e)

                tableOf(contextPrime, t) match {
                    case Some(st: StateTable) => logError(st.ast, AlreadyKnowStateError(e, st.name))
                    case _ => ()
                }

                val ContractTable = tableOf(contextPrime, t).map(_.contract)

                var mergedContext = contextPrime
                for (SwitchCase(sName, body) <- cases) {
                    val newType =
                        if (ContractTable.isEmpty) BottomType() else
                        ContractTable.get.state(sName) match {
                            case Some(stTable) =>
                                val newSimple = StateType(ContractTable.get.name, stTable.name)
                                updateSimpleType(t, newSimple)
                            case None =>
                                logError(s, StateUndefinedError(ContractTable.get.name, sName))
                                val newSimple = JustContractType(ContractTable.get.name)
                                updateSimpleType(t, newSimple)
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
                    mergedContext = mergeContext(s, mergedContext, endContext)
                }

                mergedContext

            case LocalInvocation(name, args: Seq[Expression]) =>
                val thisTable = tableOfThis(context)
                handleInvocation(context, thisTable, name, args)

            case Invocation(recipient: Expression, name, args: Seq[Expression]) =>
                val (recipType, contextPrime) = checkExpr(decl, context, recipient)
                if (recipType.isBottom) return contextPrime
                val recipTable = recipType match {
                    case BottomType() => return contextPrime
                    case IntType() | BoolType() | StringType() =>
                        logError(s, NonInvokeableError(recipType))
                        return contextPrime
                    case _ => tableOf(contextPrime, recipType).get
                }

                handleInvocation(contextPrime, recipTable, name, args)

            // TODO maybe allow constructors as statements later, but it's not very important
            /* expressions are statements, but we prune out expressions with no side effects */
            case _ =>
                logError(s, NoEffectsError(s))
                context
        }
    }

    private def checkField(field: Field, insideOf: ContractTable): Unit = {
        def checkNonStateSpecific(simple: SimpleType, err: Error): Unit = {
            simple match {
                case JustContractType(_) => ()
                case StateType(_,_) | StateUnionType(_,_) =>
                    logError(field, err)
            }
        }
        translateType(insideOf, field.typ) match {
            case OwnedRef(tr) => checkRawType(field, insideOf, tr)
            case SharedRef(tr) =>
                checkRawType(field, insideOf, tr)
                checkNonStateSpecific(extractSimpleType(tr), StateSpecificSharedError())
            case ReadOnlyRef(tr) =>
                checkRawType(field, insideOf, tr)
                checkNonStateSpecific(extractSimpleType(tr), StateSpecificReadOnlyError())

            case _ => None
        }
    }

    private def checkTransaction(
            tx: Transaction,
            insideOfContract: Either[StateTable, ContractTable]): Unit = {


        // Construct the context that the body should start with
        var initContext = Context(new TreeMap[String, Type](), isThrown = false)

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- tx.args) {
            val typ = unsafeTranslateType(tx.args, arg.typ)
            initContext = initContext.updated(arg.varName, typ)
        }

        val thisType = insideOfContract match {
            case Left(indexed) =>
                OwnedRef(NoPathType(StateType(indexed.contract.name, indexed.name)))
            case Right(indexed) =>
                OwnedRef(NoPathType(JustContractType(indexed.name)))
        }
        val cName = insideOfContract.fold(_.contract.name, _.name)

        initContext = initContext.updated("this", thisType)

        // Check that the argument types make sense
        for (arg <- tx.args) {
            checkType(arg.typ, initContext, initContext(arg.varName))
        }

        // Check the body; ensure [this] is well-typed after, and check for leaked ownership
        val outputContext =
            checkStatementSequence(tx, initContext, tx.body)

        assertSubType(tx, outputContext("this"), OwnedRef(NoPathType(JustContractType(cName))))

        for ((x, t) <- outputContext.underlying) {
            if (t.isInstanceOf[OwnedRef] && x != "this") {
                logError(tx, UnusedOwnershipError(x))
            }
        }

        // todo: analyze that there is a return in every branch
    }

    private def checkFunc(func: Func, insideOf: Contract): Unit = {
        None // todo
    }

    private def checkState(insideOf: ContractTable, state: State): Unit = {
        val indexed = insideOf.state(state.name).get
        for (decl <- state.declarations) {
            decl match {
                case t: Transaction => checkTransaction(t, Left(indexed))
                case f: Field => checkField(f, indexed.contract)
                case _ => () // TODO
            }
        }
    }

    private def checkConstructor(constr: Constructor, table: ContractTable): Unit = {

        // maybe this error should be handled in the parser
        if(constr.name != table.name) {
            logError(constr, ConstructorNameError(table.name))
        }

        var initContext = Context(new TreeMap[String, Type](), isThrown = false)

        for (arg <- constr.args) {
            val typ = unsafeTranslateType(constr.args, arg.typ)
            initContext = initContext.updated(arg.varName, typ)
        }

        //should it be owned?
        val thisType = OwnedRef(NoPathType(JustContractType(table.name)))

        initContext = initContext.updated("this", thisType)

        val outputContext =
            checkStatementSequence(constr, initContext, constr.body)

        val expectedThisType =
            OwnedRef(NoPathType(simpleOf(table.name, constr.ensuresState)))
        assertSubType(constr, outputContext("this"), expectedThisType)

        for ((x, t) <- outputContext.underlying) {
            if (t.isInstanceOf[OwnedRef] && x != "this") {
                logError(constr, UnusedOwnershipError(x))
            }
        }

    }

    private def checkContract(contract: Contract): Unit = {
        val indexed = globalTable.contract(contract.name).get
        for (decl <- contract.declarations) {
            decl match {
                case t: Transaction => checkTransaction(t, Right(indexed))
                case s: State => checkState(indexed, s)
                case f: Field => checkField(f, indexed)
                case c: Constructor => checkConstructor(c, indexed)
                case _ => () // TODO
            }
        }
    }

    /* [true] if no type errors, [false] otherwise */
    def checkProgramAndPrintErrors(): Boolean = {
        val errs = checkProgram()

        for (err <- errs) {
            val location = err.loc
            val msg = err.msg
            println(s"At $location: $msg")
        }

        errs.isEmpty
    }

    /* just returns the errors from the program */
    def checkProgram(): Seq[Error] = {
        for (contract <- globalTable.ast.contracts) {
            checkContract(contract)
        }

        errors.reverse
    }
}

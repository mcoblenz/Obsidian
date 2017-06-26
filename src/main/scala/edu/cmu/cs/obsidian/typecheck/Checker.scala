package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeMap
import scala.collection.Map
import scala.util.parsing.input.{NoPosition, Position}

sealed trait SimpleType
case class ContractType(contractName: String) extends SimpleType
case class StateType(contractName: String, stateName: String) extends SimpleType

/* This is necessarily different from the representation of types in the AST; it's
* unclear in the AST if a reference is "shared" or "owned" when it has no modifier */
sealed trait Type
case class ReadOnlyRef(t: SimpleType) extends Type
case class SharedRef(t: SimpleType) extends Type
case class OwnedRef(t: SimpleType) extends Type
case class IntType() extends Type
case class BoolType() extends Type
case class StringType() extends Type
/* Used to indicate an error in expressions */
case class BottomType() extends Type

sealed trait IndexedStateOrContract {
    def name: String
    def ast: AST
    def contract: IndexedContract
    def field(name: String): Option[Field]
    def transaction(name: String): Option[Transaction]
    def function(name: String): Option[Func]
    def simpleTypeOf: SimpleType
}

class IndexedState(
                      astNode: State,
                      insideOf: IndexedContract,
                      fieldLookup: Map[String, Field],
                      txLookup: Map[String, Transaction],
                      funLookup: Map[String, Func]
                  ) extends IndexedStateOrContract {

    def name: String = astNode.name
    def ast: State = astNode
    def contract: IndexedContract = insideOf

    def field(name: String): Option[Field] = {
        fieldLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.field(name)
        }
    }
    def transaction(name: String): Option[Transaction] = {
        txLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.transaction(name)
        }
    }
    def function(name: String): Option[Func] = {
        funLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.function(name)
        }
    }

    def simpleTypeOf: SimpleType = StateType(this.contract.name, this.name)
}

class IndexedContract(
                         astNode: Contract,
                         stateLookup: Map[String, IndexedState],
                         fieldLookup: Map[String, Field],
                         txLookup: Map[String, Transaction],
                         funLookup: Map[String, Func]
                     ) extends IndexedStateOrContract {
    def name: String = astNode.name
    def contract: IndexedContract = this
    def ast: Contract = astNode
    def field(name: String): Option[Field] = fieldLookup.get(name)
    def transaction(name: String): Option[Transaction] = txLookup.get(name)
    def function(name: String): Option[Func] = funLookup.get(name)
    def state(name: String): Option[IndexedState] = stateLookup.get(name)

    def constructors: Seq[Constructor] = {
        var constructors: List[Constructor] = Nil
        for (rawDecl <- astNode.declarations) {
            rawDecl match {
                case c: Constructor =>
                    constructors = c::constructors
                case _ => ()
            }

        }
        constructors
    }

    def simpleTypeOf: SimpleType = ContractType(this.name)
}

class IndexedProgram(
                        program: Program,
                        contractLookup: Map[String, IndexedContract]) {
    def ast: Program = program
    def contract: Function[String, Option[IndexedContract]] = contractLookup.get
}

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
        case ContractType(cName) => s"Field $fName is not defined in contract $cName"
        case StateType(cName, sName) => s"Field $fName is not defined in state $sName of contract $cName"
    }
}
case class DereferenceError(typ: Type) extends Error {
    val msg: String = s"Type $typ is not cannot be dereferenced"
}
case class MethodUndefinedError(insideOf: SimpleType, name: String) extends Error {
    val msg: String = insideOf match {
        case ContractType(cName) =>
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
case class MergeLeakError(name: String) extends Error {
    val msg: String = s"Variable $name is owned in one branch but not in any others"
}
case class MustReturnError(methName: String) extends Error {
    val msg: String = s"$methName specifies a return type, but no return value is given"
}
case class CannotReturnError(methName: String) extends Error {
    val msg: String = s"$methName does not return anything, but a return value was given"
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
    val msg: String = s"Variable $name holds ownership, but is unused at the end of the body"
}


class Checker {

    type Context = Map[String, Type]

    val errors = new collection.mutable.ArrayStack[Error]()
    var progInfo: IndexedProgram = _

    private def logError(where: AST, typ: Error): Unit = {
        typ.loc = where.loc
        errors.push(typ)
    }

    private def extractSimpleType(t: Type): Option[SimpleType] = {
        t match {
            case OwnedRef(s) => Some(s)
            case SharedRef(s) => Some(s)
            case ReadOnlyRef(s) => Some(s)
            case _ => None
        }
    }

    private def updateSimpleType(t: Type, newSimple: SimpleType): Type = {
        t match {
            case OwnedRef(_) => OwnedRef(newSimple)
            case SharedRef(_) => SharedRef(newSimple)
            case ReadOnlyRef(_) => ReadOnlyRef(newSimple)
            case ts => ts
        }
    }

    /* true iff [t1 <: t2] */
    private def simpleSubTypeOf(t1: SimpleType, t2: SimpleType): Boolean = {
        (t1, t2) match {
            // reflexivity rules
            case (ContractType(c1), ContractType(c2)) => c1 == c2
            case (StateType(c1, s1), StateType(c2, s2)) => c1 == c2 && s1 == s2

            // we can drop state information
            case (StateType(c1, _), ContractType(c2)) => c1 == c2

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
            case (OwnedRef(s_t1), OwnedRef(s_t2)) => simpleSubTypeOf(s_t1, s_t2)
            case (ReadOnlyRef(s_t1), ReadOnlyRef(s_t2)) => simpleSubTypeOf(s_t1, s_t2)
            case (SharedRef(s_t1), SharedRef(s_t2)) => simpleSubTypeOf(s_t1, s_t2)
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

    private def simpleUpperBound(t1: SimpleType, t2: SimpleType): Option[SimpleType] = {
        (t1, t2) match {
            case (ContractType(c1), ContractType(c2)) =>
                if (c1 == c2) Some(ContractType(c1)) else None
            case (StateType(c1, s1), StateType(c2, s2)) =>
                if (c1 == c2) {
                    if (s1 == s2) Some(StateType(c1, s1))
                    else Some(ContractType(c1))
                } else {
                    None
                }
            case (StateType(c1, _), c@ContractType(c2)) =>
                if (c1 == c2) Some(c) else None
            case (c@ContractType(c1), StateType(c2, _)) =>
                if (c1 == c2) Some(c) else None
            case _ => None
        }
    }

    private def upperBound(t1: Type, t2: Type): Option[Type] = {
        (t1, t2) match {
            case (IntType(), IntType()) => Some(IntType())
            case (BoolType(), BoolType()) => Some(BoolType())
            case (StringType(), StringType()) => Some(StringType())
            case (OwnedRef(s_t1), OwnedRef(s_t2)) =>
                simpleUpperBound(s_t1, s_t2).flatMap(s => Some(OwnedRef(s)))
            case (ReadOnlyRef(s_t1), ReadOnlyRef(s_t2)) =>
                simpleUpperBound(s_t1, s_t2).flatMap(s => Some(ReadOnlyRef(s)))
            case (SharedRef(s_t1), SharedRef(s_t2)) =>
                simpleUpperBound(s_t1, s_t2).flatMap(s => Some(SharedRef(s)))
            case _ => None
        }
    }

    private def translateType(t: AstType): Type = {

        val modifier = (t: SimpleType, mods: Seq[TypeModifier], contractName: String) => {
            val contract = progInfo.contract(contractName).get.ast
            // TODO: what is the behavior of a contract that is not labeled?
            val mod = contract.mod match {
                case Some(m) => m
                case None => IsOwned()
            }

            /* if a reference is 'readonly', it is labeled as such; otherwise, it is 'owned'/'shared', based on the
             * declaration of the contract itself */
            if (mods.exists(_.isInstanceOf[IsReadOnly])) {
                ReadOnlyRef(t)
            } else if (mod.isInstanceOf[IsOwned]) {
                OwnedRef(t)
            } else {
                // TODO: are main contracts always deemed shared by the type system?
                SharedRef(t)
            }
        }

        t match {
            case AstIntType() => IntType()
            case AstBoolType() => BoolType()
            case AstStringType() => StringType()
            case AstContractType(mods, name) =>
                modifier(ContractType(name), mods, name)
            case AstStateType(mods, nameC, nameS) =>
                modifier(StateType(nameC, nameS), mods, nameC)
        }
    }


    private def checkExpr(
                             insideOfMethod: Either[Transaction, Func],
                             context: Context,
                             e: Expression
                         ): (Type, Context) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: Type, c: Context): (Type, Context) = {
            val (tPrime, contextPrime) = checkExpr(insideOfMethod, c, e)
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

         e match {
             case Variable(x) =>
                 context get x match {
                     case Some(t) => t match {
                         case OwnedRef(simp) => (OwnedRef(simp), context.updated(x, ReadOnlyRef(simp)))
                         case _ => (t, context)
                     }
                     case None =>
                         logError(e, VariableUndefinedError(x))
                         (BottomType(), context)
                 }
             case NumLiteral(_) => (IntType(), context)
             case StringLiteral(_) => (StringType(), context)
             case TrueLiteral() => (BoolType(), context)
             case FalseLiteral() => (BoolType(), context)
             case This() =>
                 val baseType = indexedOfThis(context).simpleTypeOf
                 insideOfMethod match {
                     // if we're in a transaction, we can consider [this] to be [owned]
                     case Left(_) => (OwnedRef(baseType), context)
                     // if we're in a function, [this] must be deemed [readonly]
                     case Right(_) => (ReadOnlyRef(baseType), context)
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
                 val (t1, c1) = checkExpr(insideOfMethod, context, e1)
                 val (t2, c2) = checkExpr(insideOfMethod, c1, e2)
                 if (t1 == t2) (t1, c2) else {
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
                 val (t1, c1) = checkExpr(insideOfMethod, context, e1)
                 val (t2, c2) = checkExpr(insideOfMethod, c1, e2)
                 if (t1 == t2) (t1, c2) else {
                     logError(e, DifferentTypeError(e1, t1, e2, t2))
                     (BottomType(), c2)
                 }
             case Dereference(e: Expression, name) =>
                 val (t, contextPrime) = checkExpr(insideOfMethod, context, e)
                 val simple = extractSimpleType(t)
                 simple match {
                     case Some(ContractType(contractName)) => val contract = progInfo.contract(contractName).get
                         contract.field(name) match {
                             case Some(f) => (translateType(f.typ), contextPrime)
                             case None =>
                                 logError(e, FieldUndefinedError(simple.get, name))
                                 (BottomType(), context)
                         }
                     case Some(StateType(stateName, contractName)) => val contract = progInfo.contract(contractName).get
                         contract.state(stateName).get.field(name) match {
                             case Some(f) => (translateType(f.typ), contextPrime)
                             case None =>
                                 logError(e, FieldUndefinedError(simple.get, name))
                                 (BottomType(), context)
                         }
                     case _ =>
                         logError(e, DereferenceError(t))
                         (BottomType(), contextPrime)
                 }

             case LocalInvocation(name, args: Seq[Expression]) =>
                 val contract = indexedOfThis(context).contract

                 val (txLookup, funLookup) = extractSimpleType(context("this")) match {
                     case Some(StateType(_, sName)) =>
                         (contract.state(sName).get.function(name),
                           contract.state(sName).get.transaction(name))
                     case _ => (contract.function(name), contract.transaction(name))
                 }

                 val (spec, ret) = (txLookup, funLookup) match {
                     case (None, None) =>
                         MethodUndefinedError(extractSimpleType(context("this")).get, contract.name)
                         return (BottomType(), context)
                     case (_, Some(Transaction(_, txArgs, txRet, _, _))) => (txArgs, txRet)
                     case (Some(Func(_, funArgs, funRet, _)), _) => (funArgs, funRet)
                 }


                 val (argTypes, contextPrime) = checkExprs(insideOfMethod, context, args)
                 assertArgsCorrect(e, name, spec::Nil, argTypes)

                 ret match {
                     case Some(rType) => (translateType(rType), contextPrime)
                     case None => (BottomType(), contextPrime)
                 }

             case Invocation(recipient: Expression, name, args: Seq[Expression]) =>
                 val (recipType, contextPrime) = checkExpr(insideOfMethod, context, recipient)
                 val (tx, fun) = extractSimpleType(recipType) match {
                     case Some(ContractType(contractName)) =>
                         val contract = progInfo.contract(contractName).get
                         (contract.transaction(name), contract.function(name))
                     case Some(StateType(contractName, stateName)) =>
                         val contract = progInfo.contract(contractName).get
                         (contract.state(stateName).get.transaction(name),
                          contract.state(stateName).get.function(name))
                     case _ =>
                         logError(e, NonInvokeableError(recipType))
                         return (BottomType(), contextPrime)
                 }

                 val (spec, ret) = (tx, fun) match {
                     case (None, None) =>
                         val err = MethodUndefinedError(extractSimpleType(recipType).get, name)
                         logError(e, err)
                         return (BottomType(), context)
                     case (Some(Transaction(_, txArgs, txRet, _, _)), _) => (txArgs, txRet)
                     case (_, Some(Func(_, funArgs, funRet, _))) => (funArgs, funRet)
                 }

                 val (argTypes, contextPrime2) = checkExprs(insideOfMethod, context, args)
                 assertArgsCorrect(e, name, spec::Nil, argTypes)

                 ret match {
                     case Some(rType) => (translateType(rType), contextPrime2)
                     case None => (BottomType(), contextPrime2)
                 }

             case Construction(name, args: Seq[Expression]) =>
                 progInfo.contract(name) match {
                     case Some(c) =>
                         val (argTypes, contextPrime) = checkExprs(insideOfMethod, context, args)
                         val constrSpecs = c.constructors.map(constr => constr.args)
                         assertArgsCorrect(e, s"constructor of $name", constrSpecs, argTypes)
                         c.ast.mod match {
                             case Some(IsOwned()) =>
                                 (OwnedRef(ContractType(name)), contextPrime)
                             case Some(IsShared()) =>
                                 (SharedRef(ContractType(name)), contextPrime)
                             case Some(IsMain()) =>
                                 (SharedRef(ContractType(name)), contextPrime)// todo : is main contract shared?
                             case None =>
                                 (SharedRef(ContractType(name)), contextPrime) // todo : what goes here?
                         }
                     case None =>
                         logError(e, ContractUndefinedError(name))
                         (BottomType(), context)
                 }
         }
    }


    private def checkExprs(
                                    insideOfMethod: Either[Transaction, Func],
                                    context: Context,
                                    es: Seq[Expression]
                                ): (Seq[(Type, Expression)], Context) = {
        val types = new ListBuffer[(Type, Expression)]()
        var contextPrime = context
        for (e <- es) {
            val (t, contextPrime2) =
                checkExpr(insideOfMethod, contextPrime, e)
            contextPrime = contextPrime2
            types.append((t, e))
        }
        (types, contextPrime)
    }

    private def checkStatementSequence(
                                          insideOfMethod: Either[Transaction, Func],
                                          context: Context,
                                          s: Seq[Statement]
                                      ): Context = {
        s.foldLeft(context)((prevContext: Context, s: Statement) =>
                checkStatement(insideOfMethod, prevContext, s))
    }

    private def mergeContext(ast: AST, context1: Context, context2: Context): Context = {
        var mergedContext = new TreeMap[String, Type]()

        val onlyIn1: Set[String] = context1.keys.toSet.diff(context2.keys.toSet)
        val onlyIn2: Set[String] = context2.keys.toSet.diff(context1.keys.toSet)
        val inBoth = context1.keys.toSet.intersect(context2.keys.toSet)

        for (x <- inBoth) {
            val t1 = context1(x)
            val t2 = context2(x)
            upperBound(t1, t2) match {
                case Some(u) => mergedContext = mergedContext.updated(x, u)
                case None =>
                    logError(ast, MergeLeakError(x))
            }
        }

        for (x <- onlyIn1) {
            context2(x) match {
                case OwnedRef(_) =>
                    logError(ast, MergeLeakError(x))
                case _ => ()
            }
        }

        for (x <- onlyIn2) {
            context2(x) match {
                case OwnedRef(_) =>
                    logError(ast, MergeLeakError(x))
                case _ => ()
            }
        }

        mergedContext
    }

    // returns [Some(err)] if [spec] and [args] match, [None] if they do
    private def checkArgs(ast: AST,
                          methName: String,
                          spec: Seq[VariableDecl],
                          args: Seq[(Type, AST)]): Option[Seq[(AST, Error)]] = {
        val (specL, argsL) = (spec.length, args.length)
        if (specL != argsL) {
            Some((ast, WrongArityError(specL, argsL, methName))::Nil)
        } else {
            val specList = spec.toIterator
            var errList: List[(AST, Error)] = Nil
            for ((argType, ast) <- args) {
                val expectedType = translateType(specList.next().typ)
                if (!subTypeOf(argType, expectedType)) {
                    /* this is the one place we can't simply log the error
                     * because we don't know if it should be shown to the user or not */
                    val err = SubTypingError(argType, expectedType)
                    errList = (ast, err)::errList
                }
            }
            Some(errList)
        }
    }

    private def assertArgsCorrect(ast: AST,
                                  methName: String,
                                  specs: Seq[Seq[VariableDecl]],
                                  args: Seq[(Type, AST)]): Unit = {
        var errs: List[(AST, Error)] = Nil
        for (spec <- specs) {
            checkArgs(ast, methName, spec, args) match {
                case None => return
                case Some(newErrs) => errs = newErrs.toList ++ errs
            }
        }
        errs.foreach((err: (AST, Error)) => logError(err._1, err._2))
    }

    private def indexedOfThis(context: Context): IndexedStateOrContract = {
        extractSimpleType(context("this")).get match {
            case ContractType(c) => progInfo.contract(c).get
            case StateType(c, s) => progInfo.contract(c).get.state(s).get
        }
    }

    private def indexedOf(t: Type): Option[IndexedStateOrContract] = {
        extractSimpleType(t) match {
            case Some(ContractType(cName)) =>
                progInfo.contract(cName).flatMap((c: IndexedContract) => {
                        Some(c)
                    })
            case Some(StateType(cName, sName)) =>
                progInfo.contract(cName).flatMap((c: IndexedContract) => {
                        c.state(sName).flatMap((s: IndexedState) => {
                                Some(s)
                            })
                    })
            case None => None
        }
    }

    private def checkStatement(
                                  insideOfMethod: Either[Transaction, Func],
                                  context: Context,
                                  s: Statement
                              ): Context = {
        println(s"Checking statement $s. Current context:\n $context")

        val checkExpr = (context: Context, e: Expression) =>
            this.checkExpr(insideOfMethod, context, e)
        val checkExprs = (context: Context, es: Seq[Expression]) =>
            this.checkExprs(insideOfMethod, context, es)

        s match {
            case VariableDecl(typ: AstType, name) =>
                context.updated(name, translateType(typ))

            case VariableDeclWithInit(typ: AstType, name, e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                val tDecl = translateType(typ)
                assertSubType(s, t, tDecl)
                contextPrime.updated(name, tDecl)

            case Return() =>
                insideOfMethod match {
                    /* the tx/function must have no return type */
                    case Left(Transaction(_,_, None ,_,_)) | Right(Func(_,_, None ,_)) => context
                    case _ =>
                        logError(s, MustReturnError(insideOfMethod.fold(_.name, _.name)))
                        context

                }

            case ReturnExpr(e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                val tRet = insideOfMethod match {
                    /* must be no return type */
                    case Left(Transaction(_, _, Some(astType), _, _)) =>
                        translateType(astType)
                    case Right(Func(_, _, Some(astType), _)) =>
                        translateType(astType)
                    case _ =>
                        logError(s, CannotReturnError(insideOfMethod.fold(_.name, _.name)))
                        return contextPrime
                }
                assertSubType(s, t, tRet)
                contextPrime

            case Transition(newStateName, updates: Seq[(Variable, Expression)]) =>
                val (cName, sName) = extractSimpleType(context("this")) match {
                    case Some(StateType(ct, st)) => (ct, st)
                    case _ =>
                        logError(s, TransitionError())
                        return context
                }

                val indexedContract = indexedOfThis(context).contract
                val indexedOldState = indexedContract.state(sName).get

                if (indexedContract.state(newStateName).isEmpty) {
                    logError(s, StateUndefinedError(cName, newStateName))
                    return context
                }

                val indexedNewState = indexedContract.state(newStateName).get

                val oldFields = indexedOldState.ast.declarations
                    .filter(_.isInstanceOf[Field])
                    .map(_.asInstanceOf[Field].fieldName)
                val newFields = indexedNewState.ast.declarations
                    .filter(_.isInstanceOf[Field])
                    .map(_.asInstanceOf[Field].fieldName)

                val toInitialize = newFields.toSet.diff(oldFields.toSet)

                val updated = updates.map(_._1.x).toSet
                val uninitialized = toInitialize.diff(updated)
                val badInitializations = updated.diff(newFields.toSet)
                if (uninitialized.nonEmpty) logError(s, TransitionUpdateError(uninitialized))
                if (badInitializations.nonEmpty) {
                    for (s <- badInitializations) {
                        val err = FieldUndefinedError(extractSimpleType(context("this")).get, s)
                        logError(updates.find(_._1.x == s).get._1, err)
                    }
                }

                var types = new ListBuffer[Type]()
                types = BottomType() +: types

                var contextPrime = context
                for ((Variable(f), e) <- updates) {
                    val (t, contextPrime2) = checkExpr(contextPrime, e)
                    contextPrime = contextPrime2
                    val fieldType = translateType(indexedNewState.field(f).get.typ)
                    assertSubType(s, t, fieldType)
                }

                val newType = updateSimpleType(context("this"), StateType(cName, newStateName))
                contextPrime.updated("this", newType)

            case Assignment(Variable(x), e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                val contextType = context.get(s"$x")

                /* if the variable is not in the context, see if it's a field */
                if (contextType.isEmpty) {
                    val indexedThis = indexedOfThis(contextPrime)
                    val fieldLookup = indexedThis.field(x)

                    /* if it's not a field either, log an error */
                    if (fieldLookup.isEmpty) logError(s, VariableUndefinedError(x))
                    else assertSubType(e, t, translateType(fieldLookup.get.typ))
                }
                else assertSubType(s, t, contextType.get)
                contextPrime

            case Assignment(Dereference(eDeref, f), e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                val (derefType, contextPrime2) = checkExpr(contextPrime, eDeref)
                val fieldType = extractSimpleType(derefType) match {
                    // [e] is of contract type
                    case Some(simple@ContractType(cName)) =>

                        val lookup = progInfo.contract(cName)
                            .flatMap(_.field(f))

                        if (lookup.isEmpty) {
                            logError(s, FieldUndefinedError(simple, f))
                            return contextPrime2
                        }
                        translateType(lookup.get.typ)

                    // [e] is of state type
                    case Some(simple@StateType(cName, sName)) =>

                        val lookup = progInfo.contract(cName)
                            .flatMap(_.state(sName))
                            .flatMap(_.field(f))

                        if (lookup.isEmpty) {
                            logError(s, FieldUndefinedError(simple, f))
                            return contextPrime2
                        }
                        translateType(lookup.get.typ)

                    // [e] is a primitive type
                    case None =>
                        logError(s, DereferenceError(derefType))
                        return contextPrime2
                }

                assertSubType(s, t, fieldType)
                contextPrime2

            // assignment target is neither a variable nor a field
            case Assignment(_, e: Expression) =>
                val (_, contextPrime) = checkExpr(context, e)
                logError(s, AssignmentError())
                contextPrime

            case Throw() => context

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
                assertSubType(s, t, BoolType())
                checkStatementSequence(insideOfMethod, contextPrime, body)

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
                assertSubType(s, t, BoolType())
                val contextIfTrue =
                    checkStatementSequence(insideOfMethod, contextPrime, body1)
                val contextIfFalse =
                    checkStatementSequence(insideOfMethod, contextPrime, body2)
                mergeContext(s, contextIfFalse, contextIfTrue)

            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) =>
                val contextIfTry =
                    checkStatementSequence(insideOfMethod, context, s1)
                val contextIfCatch =
                    checkStatementSequence(insideOfMethod, context, s2)
                mergeContext(s, contextIfTry, contextIfCatch)

            case Switch(e: Expression, cases: Seq[SwitchCase]) =>
                val (t, contextPrime) = checkExpr(context, e)
                indexedOf(t) match {
                    case Some(st: IndexedState) => logError(st.ast, AlreadyKnowStateError(e, st.name))
                    case _ => ()
                }

                val indexedContract = indexedOf(t).map(_.contract)

                var mergedContext = contextPrime
                for (SwitchCase(sName, body) <- cases) {
                    val newType =
                        if (indexedContract.isEmpty) BottomType() else
                        indexedContract.get.state(sName) match {
                            case Some(indexedState) =>
                                val newSimple = StateType(indexedContract.get.name, indexedState.name)
                                updateSimpleType(t, newSimple)
                            case None =>
                                logError(s, StateUndefinedError(indexedContract.get.name, sName))
                                val newSimple = ContractType(indexedContract.get.name)
                                updateSimpleType(t, newSimple)
                        }

                    /* special case to allow types to change in the context if we match on a variable */
                    val startContext = e match {
                        case Variable(x) => contextPrime.updated(x, newType)
                        case _ => contextPrime
                    }

                    val endContext = checkStatementSequence(insideOfMethod, startContext, body)
                    mergedContext = mergeContext(s, mergedContext, endContext)
                }

                mergedContext

            case LocalInvocation(name, args: Seq[Expression]) =>
                val contract = indexedOfThis(context).contract
                val (txLookup, funLookup) = extractSimpleType(context("this")) match {
                    case Some(StateType(_, sName)) =>
                        (contract.state(sName).get.function(name),
                         contract.state(sName).get.transaction(name))
                    case _ => (contract.function(name), contract.transaction(name))
                }

                val (spec, ret) = (txLookup, funLookup) match {
                    case (None, None) =>
                        logError(s, MethodUndefinedError(extractSimpleType(context("this")).get, name))
                        return context
                    case (_, Some(Transaction(_, txArgs, txRet, _, _))) => (txArgs, txRet)
                    case (Some(Func(_, funArgs, funRet, _)), _) => (funArgs, funRet)
                }

                val (types, contextPrime) = checkExprs(context, args)
                assertArgsCorrect(s, name, spec::Nil, types)

                if (ret.isDefined && translateType(ret.get).isInstanceOf[OwnedRef]) {
                    logError(s, LeakReturnValueError(name))
                }

                contextPrime

            case Invocation(recipient: Expression, name, args: Seq[Expression]) =>

                val (tRecipient, contextPrime) = checkExpr(context, recipient)
                val (txLookup, funLookup) = extractSimpleType(tRecipient) match {
                    case Some(StateType(cName, sName)) =>
                        val contract = progInfo.contract(cName).get
                        (contract.state(sName).get.function(name),
                            contract.state(sName).get.transaction(name))
                        contract.function(name)
                    case Some(ContractType(cName)) =>
                        val contract = progInfo.contract(cName).get
                        (contract.function(name), contract.transaction(name))
                    case _ =>
                        logError(s, NonInvokeableError(tRecipient))
                        return contextPrime
                }

                val (spec, ret) = (txLookup, funLookup) match {
                    case (None, None) =>
                        logError(s, MethodUndefinedError(extractSimpleType(tRecipient).get, name))
                        return context
                    case (Some(Transaction(_, sp, rt, _, _)), _) => (sp, rt)
                    case (_, Some(Func(_, sp, rt, _))) => (sp, rt)
                }

                val (types, contextPrime2) = checkExprs(context, args)
                assertArgsCorrect(s, name, spec::Nil, types)

                if (ret.isDefined && translateType(ret.get).isInstanceOf[OwnedRef]) {
                    logError(s, LeakReturnValueError(name))
                }

                contextPrime2

            case Construction(name, args: Seq[Expression]) =>
                val constructors = progInfo.contract(name) match {
                    case None =>
                        logError(s, ContractUndefinedError(name))
                        return context
                    case Some(contract) =>
                        contract.constructors
                }

                val (argTypes, contextPrime) = checkExprs(context, args)
                assertArgsCorrect(s, s"Constructor for $name", constructors.map(_.args), argTypes)

                contextPrime

            /* expressions are statements, but we prune out expressions with no side effects */
            case _ =>
                logError(s, NoEffectsError(s))
                context
        }
    }

    private def checkSimpleType(st: SimpleType): Option[String] = {
        st match {
            case ContractType(name) =>
                val lookup = progInfo.contract(name)
                if (lookup.isEmpty) Some(s"Couldn't find a contract named $name")
                else None
            case StateType(cName, sName) =>
                val ctLookup = progInfo.contract(cName)
                if (ctLookup.isEmpty) return Some(s"Couldn't find a contract named $cName")
                val stLookup = ctLookup.get.state(sName)
                if (stLookup.isEmpty) return Some(s"Couldn't find a state named $sName in contract $cName")
                None
        }
    }

    private def checkField(field: Field): Unit = {
        translateType(field.typ) match {
            case OwnedRef(simple) => checkSimpleType(simple)

            case SharedRef(StateType(_, _)) =>
                logError(field, StateSpecificSharedError())
            case SharedRef(simple) => checkSimpleType(simple)

            case ReadOnlyRef(StateType(_, _)) =>
                logError(field, StateSpecificReadOnlyError())
            case ReadOnlyRef(simple) => checkSimpleType(simple)

            case _ => None
        }
    }

    private def checkTransaction(
                                    tx: Transaction,
                                    insideOfContract: Either[IndexedState, IndexedContract]
                                ): Unit = {
        var initContext = new TreeMap[String, Type]()

        for (arg <- tx.args) {
            initContext = initContext.updated(arg.varName, translateType(arg.typ))
        }

        val thisType = insideOfContract match {
            case Left(indexed) =>
                OwnedRef(StateType(indexed.contract.name, indexed.name))
            case Right(indexed) =>
                OwnedRef(ContractType(indexed.name))
        }
        val cName = insideOfContract.fold(_.contract.name, _.name)

        initContext = initContext.updated("this", thisType)

        val outputContext =
            checkStatementSequence(Left(tx), initContext, tx.body)

        assertSubType(tx, outputContext("this"), OwnedRef(ContractType(cName)))

        for ((x, t) <- outputContext) {
            if (t.isInstanceOf[OwnedRef] && x != "this") {
                logError(tx, UnusedOwnershipError(x))
            }
        }

        // todo: analyze that there is a return in every branch
    }

    private def checkFunc(func: Func, insideOf: Contract): Unit = {
        None // todo
    }

    private def checkState(insideOf: IndexedContract, state: State): Unit = {
        val indexed = insideOf.state(state.name).get
        for (decl <- state.declarations) {
            decl match {
                case t@Transaction(_,_,_,_,_) => checkTransaction(t, Left(indexed))
                case f@Field(_,_) => checkField(f)
                case _ => () // TODO
            }
        }
    }

    private def checkContract(contract: Contract): Unit = {
        val indexed = progInfo.contract(contract.name).get
        for (decl <- contract.declarations) {
            decl match {
                case t@Transaction(_,_,_,_,_) => checkTransaction(t, Right(indexed))
                case s@State(_,_) => checkState(indexed, s)
                case f@Field(_,_) => checkField(f)
                case _ => () // TODO
            }
        }
    }

    private def indexDecl(decls: Seq[Declaration]
                         ): (Map[String, Field], Map[String, Transaction], Map[String, Func]) = {
        var fieldLookup = new TreeMap[String, Field]()
        var txLookup = new TreeMap[String, Transaction]()
        var funLookup = new TreeMap[String, Func]()

        for (decl <- decls) {
            decl match {
                case f@Field(_, name) =>
                    fieldLookup = fieldLookup.updated(name, f)
                case t@Transaction(name, _, _, _, _) =>
                    txLookup = txLookup.updated(name, t)
                case f@Func(name, _, _, _) =>
                    funLookup = funLookup.updated(name, f)
                case _ => ()
            }
        }

        (fieldLookup, txLookup, funLookup)
    }

    private def indexState(state: State, insideOf: IndexedContract): IndexedState = {
        val (fieldLookup, txLookup, funLookup) = indexDecl(state.declarations)

        new IndexedState(state, insideOf, fieldLookup, txLookup, funLookup)
    }

    private def indexContract(contract: Contract): IndexedContract = {
        val (fieldLookup, txLookup, funLookup) = indexDecl(contract.declarations)

        /* this is mutable essentially in order to easily allow the construction of a circular reference between
         * an [IndexedState] object and its containing [IndexedContract] */
        val stateLookup = new collection.mutable.HashMap[String, IndexedState]()

        val indexed = new IndexedContract(contract, stateLookup, fieldLookup, txLookup, funLookup)

        /* now that we have the [IndexedContract], we index states and put them in the map */

        for (decl <- contract.declarations) {
            decl match {
                case s@State(name, _) => stateLookup.put(name, indexState(s, indexed))
                case _ => ()
            }
        }

        indexed
    }

    /* this doesn't actually check anything, but indexes data by name so that searching for a
     * contract/field/tx/function is easy/fast */
    private def indexProgram(program: Program): Unit = {
        var contractLookup = new TreeMap[String, IndexedContract]
        for (contract <- program.contracts) {
            contractLookup = contractLookup.updated(contract.name, indexContract(contract))
        }

        progInfo = new IndexedProgram(program, contractLookup)
    }

    /* [true] if no type errors, [false] otherwise */
    def checkProgramAndPrintErrors(program: Program): Boolean = {
        val errs = checkProgram(program)

        for (err <- errs) {
            val location = err.loc
            val msg = err.msg
            println(s"At $location: $msg")
        }

        errs.isEmpty
    }

    /* just returns the errors from the program */
    def checkProgram(program: Program): Seq[Error] = {
        indexProgram(program)
        for (contract <- program.contracts) {
            checkContract(contract)
        }

        errors.reverse
    }
}

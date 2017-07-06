package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeMap
import scala.collection.Map
import scala.util.parsing.input.Position

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
        case StateType(cName, sName) => s"Field $fName is not defined in state $sName of contract $cName"
    }
}
case class DereferenceError(typ: Type) extends Error {
    val msg: String = s"Type $typ cannot be dereferenced"
}
case class MethodUndefinedError(insideOf: SimpleType, name: String) extends Error {
    val msg: String = insideOf match {
        case JustContractType(cName) =>
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
    val msg: String = s"Variable $name holds ownership, but is unused at the end of its scope"
}
case class ConstructorNameError(contractName: String) extends Error {
    val msg: String = s"Invalid constructor name for contract $contractName"
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

class Checker(table: SymbolTable) {

    val errors = new collection.mutable.ArrayStack[Error]()

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
            case (JustContractType(c1), JustContractType(c2)) => c1 == c2
            case (StateType(c1, s1), StateType(c2, s2)) => c1 == c2 && s1 == s2

            // we can drop state information
            case (StateType(c1, _), JustContractType(c2)) => c1 == c2

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
            case (JustContractType(c1), JustContractType(c2)) =>
                if (c1 == c2) Some(JustContractType(c1)) else None
            case (StateType(c1, s1), StateType(c2, s2)) =>
                if (c1 == c2) {
                    if (s1 == s2) Some(StateType(c1, s1))
                    else Some(JustContractType(c1))
                } else {
                    None
                }
            case (StateType(c1, _), c@JustContractType(c2)) =>
                if (c1 == c2) Some(c) else None
            case (c@JustContractType(c1), StateType(c2, _)) =>
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

    private def translateType(ast: AST, t: AstType): Type = {

        val modifier = (t: SimpleType, mods: Seq[TypeModifier], contractName: String) => {
            val contractOpt = table.contract(contractName)
            val contract = contractOpt match {
                case Some(c) => c.ast
                case None =>
                    logError(ast, ContractUndefinedError(contractName))
                    return BottomType()
            }

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
                // TODO: support for full paths
                modifier(JustContractType(name), mods, name)
            case AstStateType(mods, cName, sName) =>
                modifier(StateType(cName, sName), mods, cName)
            // TODO handle paths
            case AstPathContractType(mods, p, name) =>
                modifier(JustContractType(name), mods, name)
            case AstPathStateType(mods, p, cName, sName) =>
                modifier(StateType(cName, sName), mods, cName)
        }
    }


    private def checkExpr(
                             decl: Declaration,
                             context: Context,
                             e: Expression
                         ): (Type, Context) = {

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

         e match {
             case Variable(x) =>
                 (context get x, indexedOfThis(context).field(x)) match {
                     case (Some(t), _) => t match {
                         case OwnedRef(simp) => (OwnedRef(simp), context.updated(x, ReadOnlyRef(simp)))
                         case _ => (t, context)
                     }
                     case (_, Some(f)) =>
                         // TODO handle cases for e.g. if the field is owned
                         (translateType(e, f.typ), context)
                     case (None, None) =>
                         logError(e, VariableUndefinedError(x))
                         (BottomType(), context)
                 }
             case NumLiteral(_) => (IntType(), context)
             case StringLiteral(_) => (StringType(), context)
             case TrueLiteral() => (BoolType(), context)
             case FalseLiteral() => (BoolType(), context)
             case This() =>
                 val baseType = indexedOfThis(context).simpleTypeOf
                 decl match {
                     // if we're in a transaction (or constructor?), we can consider [this] to be [owned]
                     case Transaction(_,_,_,_,_,_) | Constructor(_,_,_,_) => (OwnedRef(baseType), context)
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
             case Dereference(e: Expression, name) =>
                 val (t, contextPrime) = checkExpr(decl, context, e)
                 val simple = extractSimpleType(t)
                 simple match {
                     case Some(JustContractType(contractName)) => val contract = table.contract(contractName).get
                         contract.field(name) match {
                             case Some(f) => (translateType(e, f.typ), contextPrime)
                             case None =>
                                 logError(e, FieldUndefinedError(simple.get, name))
                                 (BottomType(), context)
                         }
                     case Some(StateType(stateName, contractName)) => val contract = table.contract(contractName).get
                         contract.state(stateName).get.field(name) match {
                             case Some(f) => (translateType(e, f.typ), contextPrime)
                             case None =>
                                 logError(e, FieldUndefinedError(simple.get, name))
                                 (BottomType(), context)
                         }
                     case _ =>
                         if (!(t == BottomType())) {
                             logError(e, DereferenceError(t))
                         }
                         (BottomType(), contextPrime)
                 }

             case LocalInvocation(name, args: Seq[Expression]) =>
                 val contract = indexedOfThis(context).contract

                 val (funLookup, txLookup) = extractSimpleType(context("this")) match {
                     case Some(StateType(_, sName)) =>
                         (contract.state(sName).get.function(name),
                           contract.state(sName).get.transaction(name))
                     case _ => (contract.function(name), contract.transaction(name))
                 }

                 val (spec, ret) = (funLookup, txLookup) match {
                     case (None, None) =>
                         logError(e, MethodUndefinedError(extractSimpleType(context("this")).get, name))
                         return (BottomType(), context)
                     case (_, Some(t: Transaction)) => (t.args, t.retType)
                     case (Some(f: Func), _) => (f.args, f.retType)
                 }


                 val (argTypes, contextPrime) = checkExprs(decl, context, args)
                 assertArgsCorrect(e, name, spec::Nil, argTypes)

                 ret match {
                     case Some(rType) => (translateType(e, rType), contextPrime)
                     case None => (BottomType(), contextPrime)
                 }

             case Invocation(recipient: Expression, name, args: Seq[Expression]) =>
                 val (recipType, contextPrime) = checkExpr(decl, context, recipient)
                 val (tx, fun) = extractSimpleType(recipType) match {
                     case Some(JustContractType(contractName)) =>
                         val contract = table.contract(contractName).get
                         (contract.transaction(name), contract.function(name))
                     case Some(StateType(contractName, stateName)) =>
                         val contract = table.contract(contractName).get
                         (contract.state(stateName).get.transaction(name),
                          contract.state(stateName).get.function(name))
                     case _ =>
                         if (!(recipType == BottomType())) {
                             logError(e, NonInvokeableError(recipType))
                         }
                         return (BottomType(), contextPrime)
                 }

                 val (spec, ret) = (tx, fun) match {
                     case (None, None) =>
                         val err = MethodUndefinedError(extractSimpleType(recipType).get, name)
                         logError(e, err)
                         return (BottomType(), context)
                     case (Some(t: Transaction), _) => (t.args, t.retType)
                     case (_, Some(f: Func)) => (f.args, f.retType)
                 }

                 val (argTypes, contextPrime2) = checkExprs(decl, context, args)
                 assertArgsCorrect(e, name, spec::Nil, argTypes)

                 ret match {
                     case Some(rType) => (translateType(e, rType), contextPrime2)
                     case None => (BottomType(), contextPrime2)
                 }

             case Construction(name, args: Seq[Expression]) =>
                 table.contract(name) match {
                     case Some(c) =>
                         val (argTypes, contextPrime) = checkExprs(decl, context, args)
                         val constrSpecs = c.constructors.map(constr => constr.args)
                         assertArgsCorrect(e, s"constructor of $name", constrSpecs, argTypes)
                         c.ast.mod match {
                             case Some(IsOwned()) =>
                                 (OwnedRef(JustContractType(name)), contextPrime)
                             case Some(IsShared()) =>
                                 (SharedRef(JustContractType(name)), contextPrime)
                             case Some(IsMain()) =>
                                 (SharedRef(JustContractType(name)), contextPrime)// todo : is main contract shared?
                             case None =>
                                 (SharedRef(JustContractType(name)), contextPrime) // todo : what goes here?
                         }
                     case None => (BottomType(), context)
                 }
         }
    }


    private def checkExprs(
                                    decl: Declaration,
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
                                          decl: Declaration,
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

    // returns [Some(err)] if [spec] and [args] don't match, [None] if they do
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
                val expectedType = translateType(ast, specList.next().typ)
                if (!subTypeOf(argType, expectedType)) {
                    /* this is the one place we can't simply log the error
                     * because we don't know if it should be shown to the user or not */
                    val err = SubTypingError(argType, expectedType)
                    errList = (ast, err)::errList
                }
            }
            if (errList.isEmpty) None
            else Some(errList)
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

    private def indexedOfThis(context: Context): DeclarationTable = {
        extractSimpleType(context("this")).get match {
            case JustContractType(c) => table.contract(c).get
            case StateType(c, s) => table.contract(c).get.state(s).get
        }
    }

    private def indexedOf(t: Type): Option[DeclarationTable] = {
        extractSimpleType(t) match {
            case Some(JustContractType(cName)) =>
                table.contract(cName).flatMap((c: ContractTable) => {
                        Some(c)
                    })
            case Some(StateType(cName, sName)) =>
                table.contract(cName).flatMap((c: ContractTable) => {
                        c.state(sName).flatMap((s: StateTable) => {
                                Some(s)
                            })
                    })
            case None => None
        }
    }

    private def checkStatement(
                                  decl: Declaration,
                                  context: Context,
                                  s: Statement
                              ): Context = {
        val checkExpr = (context: Context, e: Expression) =>
            this.checkExpr(decl, context, e)
        val checkExprs = (context: Context, es: Seq[Expression]) =>
            this.checkExprs(decl, context, es)

        s match {
            case VariableDecl(typ: AstType, name) =>
                context.updated(name, translateType(s, typ))

            case VariableDeclWithInit(typ: AstType, name, e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                val tDecl = typ match {
                    case AstContractType(_, name) => table.contract(name) match {
                        case None =>
                            logError(s, ContractUndefinedError(name))
                            BottomType()
                        case Some(_) => translateType(s, typ)
                    }
                    case AstStateType(_, cName, _) => table.contract(cName) match {
                        case None =>
                            logError(s, ContractUndefinedError(cName))
                            BottomType()
                        case Some(_) => translateType(s, typ)
                    }
                    case _ => translateType(s, typ)
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
                val (t, contextPrime) = checkExpr(context, e)
                val tRet = decl match {
                    /* must be no return type */
                    case t: Transaction if t.retType.isDefined =>
                        translateType(s, t.retType.get)
                    case f: Func if f.retType.isDefined =>
                        translateType(s, f.retType.get)
                    case _ =>
                        logError(s, CannotReturnError(decl.name))
                        return contextPrime
                }
                assertSubType(s, t, tRet)
                contextPrime

            case Transition(newStateName, updates: Seq[(Variable, Expression)]) =>
                val ContractTable = indexedOfThis(context).contract

                val (cName, indexedOldState) = decl match {
                    case Constructor(name, _, _, _) => (name, None)
                    case _ => extractSimpleType(context("this")) match {
                        case Some(StateType(ct, st)) => (ct, ContractTable.state(st))
                        case _ =>
                            logError(s, TransitionError())
                            return context
                    }
                }

                if (ContractTable.state(newStateName).isEmpty) {
                    logError(s, StateUndefinedError(cName, newStateName))
                    return context
                }

                val indexedNewState = ContractTable.state(newStateName).get

                val oldFields = indexedOldState match {
                    case None => Nil
                    case Some(s) =>
                        s.ast.declarations
                          .filter(_.isInstanceOf[Field])
                          .map(_.asInstanceOf[Field].name)
                }
                val newFields = indexedNewState.ast.declarations
                    .filter(_.isInstanceOf[Field])
                    .map(_.asInstanceOf[Field].name)

                val toInitialize = newFields.toSet.diff(oldFields.toSet)

                val updated = updates.map(_._1.name).toSet
                val uninitialized = toInitialize.diff(updated)
                val badInitializations = updated.diff(newFields.toSet)
                if (uninitialized.nonEmpty) logError(s, TransitionUpdateError(uninitialized))
                if (badInitializations.nonEmpty) {
                    for (s <- badInitializations) {
                        val err = FieldUndefinedError(extractSimpleType(context("this")).get, s)
                        logError(updates.find(_._1.name == s).get._1, err)
                    }
                }

                var types = new ListBuffer[Type]()
                types = BottomType() +: types

                var contextPrime = context
                for ((Variable(f), e) <- updates) {
                    if(newFields.contains(f)) {
                        val (t, contextPrime2) = checkExpr(contextPrime, e)
                        contextPrime = contextPrime2
                        val fieldType = translateType(s, indexedNewState.field(f).get.typ)
                        assertSubType(s, t, fieldType)
                    }
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
                    else assertSubType(e, t, translateType(s, fieldLookup.get.typ))
                }
                else {
                    if (t != BottomType()) {
                        assertSubType(s, t, contextType.get)
                    }

                }
                contextPrime

            case Assignment(Dereference(eDeref, f), e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                val (derefType, contextPrime2) = checkExpr(contextPrime, eDeref)
                val fieldType = extractSimpleType(derefType) match {
                    // [e] is of contract type
                    case Some(simple@JustContractType(cName)) =>

                        val lookup = table.contract(cName)
                            .flatMap(_.field(f))

                        if (lookup.isEmpty) {
                            logError(s, FieldUndefinedError(simple, f))
                            return contextPrime2
                        }
                        translateType(s, lookup.get.typ)

                    // [e] is of state type
                    case Some(simple@StateType(cName, sName)) =>

                        val lookup = table.contract(cName)
                            .flatMap(_.state(sName))
                            .flatMap(_.field(f))

                        if (lookup.isEmpty) {
                            logError(s, FieldUndefinedError(simple, f))
                            return contextPrime2
                        }
                        translateType(s, lookup.get.typ)

                    // [e] is a primitive type
                    case None =>
                        if (!(derefType == BottomType())) {
                            logError(s, DereferenceError(derefType))
                        }
                        return contextPrime2
                }

                assertSubType(s, t, fieldType)
                contextPrime2

            // assignment target is neither a variable nor a field
            case Assignment(_, e: Expression) =>
                val (_, contextPrime) = checkExpr(context, e)
                logError(s, AssignmentError())
                contextPrime

            case Throw() => Context(context.underlying, isThrown = true)

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
                assertSubType(s, t, BoolType())
                val contextIfTrue = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body),
                    contextPrime)
                mergeContext(s, contextPrime, contextIfTrue)

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
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
                val (t, contextPrime) = checkExpr(context, e)
                indexedOf(t) match {
                    case Some(st: StateTable) => logError(st.ast, AlreadyKnowStateError(e, st.name))
                    case _ => ()
                }

                val ContractTable = indexedOf(t).map(_.contract)

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
                val contract = indexedOfThis(context).contract
                val (funLookup, txLookup) = extractSimpleType(context("this")) match {
                    case Some(StateType(_, sName)) =>
                        (contract.state(sName).get.function(name),
                         contract.state(sName).get.transaction(name))
                    case _ => (contract.function(name), contract.transaction(name))
                }

                val (spec, ret) = (funLookup, txLookup) match {
                    case (None, None) =>
                        logError(s, MethodUndefinedError(extractSimpleType(context("this")).get, name))
                        return context
                    case (_, Some(t: Transaction)) =>
                        (t.args, t.retType)
                    case (Some(f: Func), _) => (f.args, f.retType)
                }

                val (types, contextPrime) = checkExprs(context, args)
                assertArgsCorrect(s, name, spec::Nil, types)

                if (ret.isDefined && translateType(s, ret.get).isInstanceOf[OwnedRef]) {
                    logError(s, LeakReturnValueError(name))
                }

                contextPrime

            case Invocation(recipient: Expression, name, args: Seq[Expression]) =>

                val (tRecipient, contextPrime) = checkExpr(context, recipient)
                val (funLookup, txLookup) = extractSimpleType(tRecipient) match {
                    case Some(StateType(cName, sName)) =>
                        val contract = table.contract(cName).get
                        (contract.state(sName).get.function(name),
                            contract.state(sName).get.transaction(name))
                        contract.function(name)
                    case Some(JustContractType(cName)) =>
                        val contract = table.contract(cName).get
                        (contract.function(name), contract.transaction(name))
                    case _ =>
                        if (!(tRecipient == BottomType())) {
                            logError(s, NonInvokeableError(tRecipient))
                        }
                        return contextPrime
                }

                val (spec, ret) = (funLookup, txLookup) match {
                    case (None, None) =>
                        logError(s, MethodUndefinedError(extractSimpleType(tRecipient).get, name))
                        return context
                    case (_, Some(t: Transaction)) => (t.args, t.retType)
                    case (Some(f: Func), _) => (f.args, f.retType)
                }

                val (types, contextPrime2) = checkExprs(context, args)
                assertArgsCorrect(s, name, spec::Nil, types)

                if (ret.isDefined && translateType(s, ret.get).isInstanceOf[OwnedRef]) {
                    logError(s, LeakReturnValueError(name))
                }

                contextPrime2

            case Construction(name, args: Seq[Expression]) =>
                val constructors = table.contract(name) match {
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
            case JustContractType(name) =>
                val lookup = table.contract(name)
                if (lookup.isEmpty) Some(s"Couldn't find a contract named $name")
                else None
            case StateType(cName, sName) =>
                val ctLookup = table.contract(cName)
                if (ctLookup.isEmpty) return Some(s"Couldn't find a contract named $cName")
                val stLookup = ctLookup.get.state(sName)
                if (stLookup.isEmpty) return Some(s"Couldn't find a state named $sName in contract $cName")
                None
        }
    }

    private def checkField(field: Field): Unit = {
        translateType(field, field.typ) match {
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
                                    insideOfContract: Either[StateTable, ContractTable]
                                ): Unit = {
        var initContext = Context(new TreeMap[String, Type](), isThrown = false)

        for (arg <- tx.args) {
            initContext = initContext.updated(arg.varName, translateType(tx, arg.typ))
        }

        val thisType = insideOfContract match {
            case Left(indexed) =>
                OwnedRef(StateType(indexed.contract.name, indexed.name))
            case Right(indexed) =>
                OwnedRef(JustContractType(indexed.name))
        }
        val cName = insideOfContract.fold(_.contract.name, _.name)

        initContext = initContext.updated("this", thisType)

        val outputContext =
            checkStatementSequence(tx, initContext, tx.body)

        assertSubType(tx, outputContext("this"), OwnedRef(JustContractType(cName)))

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
                case f: Field => checkField(f)
                case _ => () // TODO
            }
        }
    }

    private def checkConstructor(c: Constructor, indexed: ContractTable): Unit = {

        //maybe this error should be handled in the parser
        if(c.name != indexed.name) {
            logError(c, ConstructorNameError(indexed.name))
        }

        var initContext = Context(new TreeMap[String, Type](), isThrown = false)

        for (arg <- c.args) {
            initContext = initContext.updated(arg.varName, translateType(c, arg.typ))
        }

        //should it be owned?
        val thisType = OwnedRef(JustContractType(indexed.name))

        initContext = initContext.updated("this", thisType)

        val outputContext =
            checkStatementSequence(c, initContext, c.body)

        assertSubType(c, outputContext("this"), OwnedRef(JustContractType(indexed.name)))

        for ((x, t) <- outputContext.underlying) {
            if (t.isInstanceOf[OwnedRef] && x != "this") {
                logError(c, UnusedOwnershipError(x))
            }
        }

    }

    private def checkContract(contract: Contract): Unit = {
        val indexed = table.contract(contract.name).get
        for (decl <- contract.declarations) {
            decl match {
                case t: Transaction => checkTransaction(t, Right(indexed))
                case s: State => checkState(indexed, s)
                case f: Field => checkField(f)
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
        for (contract <- table.ast.contracts) {
            checkContract(contract)
        }

        errors.reverse
    }
}

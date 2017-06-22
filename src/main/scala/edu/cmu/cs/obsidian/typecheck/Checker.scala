package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeMap
import scala.collection.Map

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


class IndexedState(
                      state: State,
                      insideOf: IndexedContract,
                      fieldLookup: Map[String, Field],
                      txLookup: Map[String, Transaction],
                      funLookup: Map[String, Func]
                  ) {
    def getAst: State = state

    def getContract: IndexedContract = insideOf

    def getField(name: String): Option[Field] = {
        fieldLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.getField(name)
        }
    }
    def getTransaction(name: String): Option[Transaction] = {
        txLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.getTransaction(name)
        }
    }
    def getFunction(name: String): Option[Func] = {
        funLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.getFunction(name)
        }
    }
}

class IndexedContract(
                         contract: Contract,
                         stateLookup: Map[String, IndexedState],
                         fieldLookup: Map[String, Field],
                         txLookup: Map[String, Transaction],
                         funLookup: Map[String, Func]
                     ) {
    def getAst: Contract = contract
    def getField: Function[String, Option[Field]] = fieldLookup.get
    def getTransaction: Function[String, Option[Transaction]] = txLookup.get
    def getFunction: Function[String, Option[Func]] = funLookup.get
    def getState: Function[String, Option[IndexedState]] = stateLookup.get

    def getConstructors: Seq[Constructor] = {
        var constructors: List[Constructor] = Nil
        for (rawDecl <- contract.declarations) {
            rawDecl match {
                case c: Constructor =>
                    constructors = c::constructors
            }

        }
        constructors
    }
}

class IndexedProgram(
                        program: Program,
                        contractLookup: Map[String, IndexedContract]) {
    def getAst: Program = program
    def getContract: Function[String, Option[IndexedContract]] = contractLookup.get
}

class Checker {

    type Context = Map[String, Type]
    type Error = String

    val errors = new collection.mutable.ArrayStack[Error]()
    var progInfo: IndexedProgram = _

    private def logError(msg: String): Unit = {
        errors.push(msg)
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
            case OwnedRef(s) => OwnedRef(newSimple)
            case SharedRef(s) => SharedRef(newSimple)
            case ReadOnlyRef(s) => ReadOnlyRef(newSimple)
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
            case (StateType(c1, s1), ContractType(c2)) => c1 == c2

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

    private def assertSubType(t1: Type, t2: Type): Unit = {
        if (!subTypeOf(t1, t2)) {
            logError(s"Type $t1 cannot be used as $t2")
        }
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

        val getModifier = (t: SimpleType, mods: Seq[TypeModifier], contractName: String) => {
            val contract = progInfo.getContract(contractName).get.getAst
            // TODO: what is the behavior of a contract that is not labeled?
            val mod = contract.mod match {
                case Some(m) => m
                case None => IsOwned
            }

            /* if a reference is 'readonly', it is labeled as such; otherwise, it is 'owned'/'shared', based on the
             * declaration of the contract itself */
            if (mods.contains(IsReadOnly)) {
                ReadOnlyRef(t)
            } else if (mod == IsOwned) {
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
                getModifier(ContractType(name), mods, name)
            case AstStateType(mods, nameC, nameS) =>
                getModifier(StateType(nameC, nameS), mods, nameC)
        }
    }


    private def checkExpr(
                             insideOfMethod: Either[Transaction, Func],
                             insideOfContract: Either[IndexedState, IndexedContract],
                             context: Context,
                             e: Expression
                         ): (Type, Context) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: Type, c: Context): (Type, Context) = {
            val (tPrime, contextPrime) = checkExpr(insideOfMethod, insideOfContract, c, e)
            if (t == tPrime) (t, contextPrime) else {
                logError(f"$e must have type $t")
                (BottomType(), contextPrime)
            }
        }

        def assertOperationType(e1: Expression, e2: Expression, t: Type): (Type, Context) = {
            val (t1, c1) = assertTypeEquality(e1, t, context)
            val (t2, c2) = assertTypeEquality(e2, t, c1)
            if (t1 == BottomType() || t2 == BottomType()) (BottomType(), c2) else (t, c2)
        }

        def assertComparisonType(e1: Expression, e2: Expression): (Type, Context) = {
            val (t1, c1) = assertTypeEquality(e1, IntType(), context)
            val (t2, c2) = assertTypeEquality(e2, IntType(), c1)
            if (t1 == BottomType() || t2 == BottomType()){
                logError(f"$e1 and $e2 must have type int")
                (BottomType(), c2)
            }  else (BoolType(), c2)
        }

         e match {
             case Variable(x: String) =>
                 context get x match {
                     case Some(t) => t match {
                         case OwnedRef(simp) => (OwnedRef(simp), context.updated(x, ReadOnlyRef(simp)))
                         case _ => (t, context)
                     }
                     case None =>
                         logError(s"Variable $x undefined")
                         (BottomType(), context)
                 }
             case NumLiteral(value: Int) => (IntType(), context)
             case StringLiteral(value: String) => (StringType(), context)
             case TrueLiteral() => (BoolType(), context)
             case FalseLiteral() => (BoolType(), context)
             case This() =>
                 val baseType = insideOfContract match {
                     case Left(s) => StateType(s.getContract.getAst.name, s.getAst.name)
                     case Right(c) => ContractType(c.getAst.name)
                 }
                 insideOfMethod match {
                     // if we're in a transaction, we can consider [this] to be [owned]
                     case Left(tx) => (OwnedRef(baseType), context)
                     // if we're in a function, [this] must be deemed [readonly]
                     case Right(fun) => (ReadOnlyRef(baseType), context)
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
                 val (t1, c1) = checkExpr(insideOfMethod, insideOfContract, context, e1)
                 val (t2, c2) = checkExpr(insideOfMethod, insideOfContract, c1, e2)
                 if (t1 == t2) (t1, c2) else {
                     logError(f"Expressions '$e1' and '$e2' do not have same type")
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
                 val (t1, c1) = checkExpr(insideOfMethod, insideOfContract, context, e1)
                 val (t2, c2) = checkExpr(insideOfMethod, insideOfContract, c1, e2)
                 if (t1 == t2) (t1, c2) else {
                     logError(f"Expressions '$e1' and '$e2' do not have same type")
                     (BottomType(), c2)
                 }
             case Dereference(e: Expression, name: String) =>
                 val (t, contextPrime) = checkExpr(insideOfMethod, insideOfContract, context, e)
                 extractSimpleType(t) match {
                     case Some(ContractType(contractName)) => val contract = progInfo.getContract(contractName).get
                         contract.getField(name) match {
                             case Some(f) => (translateType(f.typ), contextPrime)
                             case None =>
                                 logError(s"$contractName has no field named '$name'")
                                 (BottomType(), context)
                         }
                     case Some(StateType(stateName, contractName)) => val contract = progInfo.getContract(contractName).get
                         contract.getState(stateName).get.getField(name) match {
                             case Some(f) => (translateType(f.typ), contextPrime)
                             case None =>
                                 logError(s"$contractName has no field named '$name'")
                                 (BottomType(), context)
                         }
                     case _ =>
                         logError(s"Cannot dereference on type $t")
                         return (BottomType(), contextPrime)
                 }

             case LocalInvocation(name: String, args: Seq[Expression]) =>
                 val contract = insideOfContract match {
                 case Left(s) => s.getContract
                 case Right(c) => c
             }
                 val (txLookup, funLookup) = extractSimpleType(context("this")) match {
                     case Some(StateType(_, sName)) =>
                         (contract.getState(sName).get.getFunction(name),
                           contract.getState(sName).get.getTransaction(name))
                     case _ => (contract.getFunction(name), contract.getTransaction(name))
                 }

                 val (spec, ret) = (txLookup, funLookup) match {
                     case (None, None) =>
                         logError(s"No function or transaction named $name found")
                         return (BottomType(), context)
                     case (_, Some(Transaction(_, txArgs, txRet, _, _))) => (txArgs, txRet)
                     case (Some(Func(_, funArgs, funRet, _)), _) => (funArgs, funRet)
                 }


                 val (argTypes, contextPrime) = checkExprs(insideOfMethod, insideOfContract, context, args)
                 assertArgsCorrect(spec::Nil, argTypes)

                 ret match {
                     case Some(rType) => (translateType(rType), contextPrime)
                     case None => (BottomType(), contextPrime)
                 }

             case Invocation(recipient: Expression, name: String, args: Seq[Expression]) =>
                 val (recipType, contextPrime) = checkExpr(insideOfMethod, insideOfContract, context, recipient)
                 val (tx, fun) = extractSimpleType(recipType) match {
                     case Some(ContractType(contractName)) =>
                         val contract = progInfo.getContract(contractName).get
                         (contract.getTransaction(name), contract.getFunction(name))
                     case Some(StateType(stateName, contractName)) =>
                         val contract = progInfo.getContract(contractName).get
                         (contract.getState(stateName).get.getTransaction(name),
                          contract.getState(stateName).get.getFunction(name))
                     case _ =>
                         logError(s"Cannot invoke functions or transactions on type $recipType")
                         return (BottomType(), contextPrime)
                 }

                 val (spec, ret) = (tx, fun) match {
                     case (None, None) =>
                         logError(s"No function or transaction named $name found")
                         return (BottomType(), context)
                     case (Some(Transaction(_, txArgs, txRet, _, _)), _) => (txArgs, txRet)
                     case (_, Some(Func(_, funArgs, funRet, _))) => (funArgs, funRet)
                 }

                 val (argTypes, contextPrime2) = checkExprs(insideOfMethod, insideOfContract, context, args)
                 assertArgsCorrect(spec::Nil, argTypes)

                 ret match {
                     case Some(rType) => (translateType(rType), contextPrime2)
                     case None => (BottomType(), contextPrime2)
                 }

             case Construction(name: String, args: Seq[Expression]) =>
                 progInfo.getContract(name) match {
                     case Some(c) =>
                         val (argTypes, contextPrime) = checkExprs(insideOfMethod, insideOfContract, context, args)
                         val constrSpecs = c.getConstructors.map(constr => constr.args)
                         assertArgsCorrect(constrSpecs, argTypes)
                         c.getAst.mod match {
                             case Some(IsOwned) => (OwnedRef(ContractType(name)), context)
                             case Some(IsShared) => (SharedRef(ContractType(name)), context)
                             case Some(IsMain) => (SharedRef(ContractType(name)), context)// is main contract shared?
                             case None => (SharedRef(ContractType(name)), context)// what goes here?
                         }
                     case None =>
                         logError(s"Contract '$name' does not exist")
                         (BottomType(), context)
                 }
         }
    }


    private def checkExprs(
                                    insideOfMethod: Either[Transaction, Func],
                                    insideOfContract: Either[IndexedState, IndexedContract],
                                    context: Context,
                                    es: Seq[Expression]
                                ): (Seq[Type], Context) = {
        val types = new ListBuffer[Type]()
        var contextPrime = context
        for (e <- es) {
            val (t, contextPrime2) =
                checkExpr(insideOfMethod, insideOfContract, contextPrime, e)
            contextPrime = contextPrime2
            types.append(t)
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

    private def mergeContext(context1: Context, context2: Context): Context = {
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
                    logError(s"Variable $x is incompatibly typed as both $t1 and $t2 after branch")
            }
        }

        for (x <- onlyIn1) {
            context1(x) match {
                case OwnedRef(_) =>
                    logError(s"Variable $x is owned in at least one branch, but is dropped in others")
                case _ => ()
            }
        }

        for (x <- onlyIn2) {
            context2(x) match {
                case OwnedRef(_) =>
                    logError(s"Variable $x is owned in at least one branch, but is dropped in others")
                case _ => ()
            }
        }

        mergedContext
    }

    // returns [Some(err)] if [spec] and [args] don't match, [None] if they do
    private def checkArgs(spec: Seq[VariableDecl], args: Seq[Type]): Option[Seq[Error]] = {
        val (specL, argsL) = (spec.length, args.length)
        if (specL > argsL) {
            Some(s"Too few arguments: expected $specL but only found $argsL"::Nil)
        } else if (specL < argsL) {
            Some(s"Too many arguments: expected $specL but found $argsL"::Nil)
        } else {
            val specList = spec.toIterator
            var errList: List[Error] = Nil
            for (argType <- args) {
                val expectedType = translateType(specList.next().typ)
                if (!subTypeOf(argType, expectedType)) {
                    errList = s"Type $argType cannot be used as $expectedType"::errList
                }
            }
            Some(errList)
        }
    }

    private def assertArgsCorrect(specs: Seq[Seq[VariableDecl]], args: Seq[Type]): Unit = {
        var errs: List[Error] = Nil
        for (spec <- specs) {
            checkArgs(spec, args) match {
                case None => return
                case Some(newErrs) => errs = newErrs.toList ++ errs
            }
        }
        errs.foreach(logError)
    }

    private def getThisIndexed(context: Context): Either[IndexedState, IndexedContract] = {
        extractSimpleType(context("this")) match {
            case Some(ContractType(c)) => Right(progInfo.getContract(c).get)
            case Some(StateType(c, s)) => Left(progInfo.getContract(c).get.getState(s).get)
            case _ => throw new Exception() // shouldn't happen
        }
    }

    private def getThisIndexedContract(context: Context): IndexedContract = {
        getThisIndexed(context).fold(_.getContract, identity)
    }

    private def checkStatement(
                                  insideOfMethod: Either[Transaction, Func],
                                  context: Context,
                                  s: Statement
                              ): Context = {

        val checkExpr = (context: Context, e: Expression) =>
            this.checkExpr(insideOfMethod, getThisIndexed(context), context, e)
        val checkExprs = (context: Context, es: Seq[Expression]) =>
            this.checkExprs(insideOfMethod, getThisIndexed(context), context, es)

        s match {
            case VariableDecl(typ: AstType, varName: String) =>
                context.updated(varName, translateType(typ))

            case VariableDeclWithInit(typ: AstType, varName: String, e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                val tDecl = translateType(typ)
                assertSubType(t, tDecl)
                contextPrime.updated(varName, tDecl)

            case Return =>
                insideOfMethod match {
                    /* the tx/function must have no return type */
                    case Left(Transaction(_,_, None ,_,_)) | Right(Func(_,_, None ,_)) => context
                    case _ =>
                        logError("Must return a value of the specified return type")
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
                        logError("Cannot return a value for a method with no return type")
                        return contextPrime
                }
                assertSubType(t, tRet)
                contextPrime

            case Transition(newStateName: String, updates: Seq[(Variable, Expression)]) =>
                val (cName, sName) = extractSimpleType(context("this")) match {
                    case Some(StateType(ct, st)) => (ct, st)
                    case _ =>
                        logError("'this' must be typed to a particular state to transition")
                        return context
                }

                val indexedContract = getThisIndexedContract(context)
                val indexedOldState = indexedContract.getState(sName).get

                if (indexedContract.getState(newStateName).isEmpty) {
                    logError(s"State $newStateName not found in contract $cName")
                    return context
                }

                val indexedNewState = indexedContract.getState(newStateName).get

                val oldFields = indexedOldState.getAst.declarations
                    .filter(_.isInstanceOf[Field])
                    .map(_.asInstanceOf[Field].fieldName)
                val newFields = indexedNewState.getAst.declarations
                    .filter(_.isInstanceOf[Field])
                    .map(_.asInstanceOf[Field].fieldName)

                val toInitialize = newFields.toSet - oldFields.toSet

                if (updates.map(_._1.x).toSet != toInitialize) {
                    logError(s"Transition from $sName to $newStateName must update exactly the following set of" +
                             s" fields: $toInitialize")
                }

                var types = new ListBuffer[Type]()
                types = BottomType() +: types

                var contextPrime = context
                for ((Variable(f), e) <- updates) {
                    val (t, contextPrime2) = checkExpr(contextPrime, e)
                    contextPrime = contextPrime2
                    val fieldType = translateType(indexedNewState.getField(f).get.typ)
                    assertSubType(t, fieldType)
                }

                val newType = updateSimpleType(context("this"), StateType(cName, newStateName))
                contextPrime.updated("this", newType)

            case Assignment(assignTo: Expression, e: Expression) =>
                val (t, contextPrime) = checkExpr(context, e)
                assignTo match {

                    // assigment is of the form "x = e"
                    case Variable(x) =>
                        val contextType = context.get(s"$x")
                        if (contextType.isEmpty) logError(s"Variable $x not found in context")
                        else assertSubType(t, contextType.get)
                        contextPrime

                    // assignment is of the form "e1.f = e2"
                    case Dereference(e, f) =>
                        val (derefType, contextPrime2) = checkExpr(contextPrime, e)
                        val fieldType = extractSimpleType(derefType) match {
                            // [e] is of contract type
                            case Some(ContractType(cName)) =>

                                val lookup = progInfo.getContract(cName)
                                    .flatMap(_.getField(f))

                                if (lookup.isEmpty) {
                                    logError(s"Field $f not found in contract $cName")
                                    return contextPrime2
                                }
                                translateType(lookup.get.typ)

                            // [e] is of state type
                            case Some(StateType(cName, sName)) =>

                                val lookup = progInfo.getContract(cName)
                                    .flatMap(_.getState(sName))
                                    .flatMap(_.getField(f))

                                if (lookup.isEmpty) {
                                    logError(s"Field $f not found in state $cName.$sName")
                                    return contextPrime2
                                }
                                translateType(lookup.get.typ)

                            // [e] is a primitive type
                            case None =>
                                logError(s"Cannot dereference expression $e of type $derefType")
                                return contextPrime2
                        }

                        assertSubType(t, fieldType)
                        contextPrime2

                    // assignment target is neither a variable nor a field
                    case _ =>
                        logError("Assignment target must be a local variable or a field")
                        contextPrime
                }

            case Throw() => context

            case If(eCond: Expression, s: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
                assertSubType(t, BoolType())
                checkStatementSequence(insideOfMethod, contextPrime, s)

            case IfThenElse(eCond: Expression, s1: Seq[Statement], s2: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
                assertSubType(t, BoolType())
                val contextIfTrue =
                    checkStatementSequence(insideOfMethod, contextPrime, s1)
                val contextIfFalse =
                    checkStatementSequence(insideOfMethod, contextPrime, s2)
                mergeContext(contextIfFalse, contextIfTrue)

            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) =>
                val contextIfTry =
                    checkStatementSequence(insideOfMethod, context, s1)
                val contextIfCatch =
                    checkStatementSequence(insideOfMethod, context, s2)
                mergeContext(contextIfTry, contextIfCatch)

            case Switch(e: Expression, cases: Seq[SwitchCase]) =>
                val (t, contextPrime) = checkExpr(context, e)
                val indexedContract = getThisIndexedContract(contextPrime)

                getThisIndexed(context).fold(
                    st => {
                            val sName = st.getAst.name
                            logError(s"Expression $e is known to be in state " +
                                     s"$sName: a dynamic check is not needed")
                        },
                    _ => ()
                )

                var mergedContext = contextPrime
                for (SwitchCase(sName, body) <- cases) {
                    val newThisTypeSimple = indexedContract.getState(sName) match {
                        case Some(indexedState) =>
                            StateType(indexedContract.getAst.name, indexedState.getAst.name)
                        case None =>
                            logError(s"State $sName not found")
                            ContractType(indexedContract.getAst.name)
                    }

                    val newThisType = updateSimpleType(context("this"), newThisTypeSimple)

                    val startContext = contextPrime.updated("this", newThisType)
                    val endContext = checkStatementSequence(insideOfMethod, startContext, body)
                    mergedContext = mergeContext(mergedContext, endContext)
                }

                mergedContext

            case LocalInvocation(name: String, args: Seq[Expression]) =>
                val contract = getThisIndexedContract(context)
                val (txLookup, funLookup) = extractSimpleType(context("this")) match {
                    case Some(StateType(_, sName)) =>
                        (contract.getState(sName).get.getFunction(name),
                         contract.getState(sName).get.getTransaction(name))
                    case _ => (contract.getFunction(name), contract.getTransaction(name))
                }

                val (spec, ret) = (txLookup, funLookup) match {
                    case (None, None) =>
                        logError(s"No function or transaction named $name found")
                        return context
                    case (_, Some(Transaction(_, txArgs, txRet, _, _))) => (txArgs, txRet)
                    case (Some(Func(_, funArgs, funRet, _)), _) => (funArgs, funRet)
                }

                val (types, contextPrime) = checkExprs(context, args)
                assertArgsCorrect(spec::Nil, types)

                if (ret.isDefined && translateType(ret.get).isInstanceOf[OwnedRef]) {
                    logError(s"Call to $name leaks ownership of the return value")
                }

                contextPrime

            case Invocation(recipient: Expression, name: String, args: Seq[Expression]) =>

                val (tRecipient, contextPrime) = checkExpr(context, recipient)
                val (txLookup, funLookup) = extractSimpleType(tRecipient) match {
                    case Some(StateType(cName, sName)) =>
                        val contract = progInfo.getContract(cName).get
                        (contract.getState(sName).get.getFunction(name),
                            contract.getState(sName).get.getTransaction(name))
                        contract.getFunction(name)
                    case Some(ContractType(cName)) =>
                        val contract = progInfo.getContract(cName).get
                        (contract.getFunction(name), contract.getTransaction(name))
                    case _ =>
                        logError(s"Cannot invoke functions or transactions on type $tRecipient")
                        return contextPrime
                }

                val (spec, ret) = (txLookup, funLookup) match {
                    case (None, None) =>
                        logError(s"No function or transaction named $name found")
                        return context
                    case (Some(Transaction(_, sp, rt, _, _)), _) => (sp, rt)
                    case (_, Some(Func(_, sp, rt, _))) => (sp, rt)
                }

                val (types, contextPrime2) = checkExprs(context, args)
                assertArgsCorrect(spec::Nil, types)

                if (ret.isDefined && translateType(ret.get).isInstanceOf[OwnedRef]) {
                    logError(s"Call to $name leaks ownership of the return value")
                }

                contextPrime2

            case Construction(name: String, args: Seq[Expression]) =>
                val constructors = progInfo.getContract(name) match {
                    case None =>
                        logError(s"Cannot find contract with name $name")
                        return context
                    case Some(contract) =>
                        contract.getConstructors
                }

                val (argTypes, contextPrime) = checkExprs(context, args)
                assertArgsCorrect(constructors.map(_.args), argTypes)

                contextPrime

            case expr =>
                logError(s"Statement $expr has no side effects")
                context
        }
    }

    private def checkSimpleType(st: SimpleType): Option[String] = {
        st match {
            case ContractType(name) =>
                val lookup = progInfo.getContract(name)
                if (lookup.isEmpty) Some(s"Couldn't find a contract named $name")
                else None
            case StateType(cName, sName) =>
                val ctLookup = progInfo.getContract(cName)
                if (ctLookup.isEmpty) return Some(s"Couldn't find a contract named $cName")
                val stLookup = ctLookup.get.getState(sName)
                if (stLookup.isEmpty) return Some(s"Couldn't find a state named $sName in contract $cName")
                None
        }
    }

    private def checkField(field: Field): Unit = {
        translateType(field.typ) match {
            case OwnedRef(simple) => checkSimpleType(simple)

            case SharedRef(StateType(_, _)) =>
                logError(s"State-specific types are not safe for 'shared' references")
            case SharedRef(simple) => checkSimpleType(simple)

            case ReadOnlyRef(StateType(_, _)) =>
                logError(s"State-specific types are not safe for 'readonly' references")
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
                OwnedRef(StateType(indexed.getContract.getAst.name, indexed.getAst.name))
            case Right(indexed) =>
                OwnedRef(ContractType(indexed.getAst.name))
        }
        val cName = insideOfContract.fold(_.getContract.getAst.name, _.getAst.name)

        initContext = initContext.updated("this", thisType)

        val outputContext =
            checkStatementSequence(Left(tx), initContext, tx.body)

        assertSubType(outputContext("this"), OwnedRef(ContractType(cName)))

        for ((x, t) <- outputContext) {
            if (t.isInstanceOf[OwnedRef] && x != "this") {
                logError(s"Variable $x holds ownership, but is unused at the end of the transaction")
            }
        }

        // todo: analyze that there is a return in every branch
    }

    private def checkFunc(func: Func, insideOf: Contract): Unit = {
        None // todo
    }

    private def checkState(insideOf: IndexedContract, state: State): Unit = {
        val indexed = insideOf.getState(state.name).get
        for (decl <- state.declarations) {
            decl match {
                case t@Transaction(_,_,_,_,_) => checkTransaction(t, Left(indexed))
                case f@Field(_,_) => checkField(f)
                case _ => () // TODO
            }
        }
    }

    private def checkContract(contract: Contract): Unit = {
        val indexed = progInfo.getContract(contract.name).get
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
                case f@Field(_, name) => fieldLookup = fieldLookup.updated(name, f)
                case t@Transaction(name, _, _, _, _) => txLookup = txLookup.updated(name, t)
                case f@Func(name, _, _, _) => funLookup = funLookup.updated(name, f)
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
    def checkProgram(program: Program): Boolean = {
        indexProgram(program)
        for (contract <- program.contracts) {
            checkContract(contract)
        }

        for (err <- errors.reverse) println(err)

        errors.isEmpty
    }
}

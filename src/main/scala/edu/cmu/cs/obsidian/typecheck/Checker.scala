package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.Map
import scala.collection.mutable.ListBuffer

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
    def getAst = state

    def getContract = insideOf

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
    def getAst = contract
    def getField = fieldLookup.get(_)
    def getTransaction = txLookup.get(_)
    def getFunction = funLookup.get(_)
    def getState = stateLookup.get(_)

    def getConstructors(): Set[Constructor] = {
        var constructors = new TreeSet[Constructor]()
        for (rawDecl <- contract.declarations;
             rawDecl.isInstanceOf[Constructor];
             decl: Constructor <- rawDecl.asInstanceOf[Constructor]) {
            constructors = constructors + decl
        }
        constructors
    }
}

class IndexedProgram(
                        program: Program,
                        contractLookup: Map[String, IndexedContract]) {
    def getAst = program
    def getContract = contractLookup.get(_)
}

class Checker {

    type Context = Map[String, Type]
    type Error = String

    val errors = new collection.mutable.ArrayStack[Error]()
    var progInfo: IndexedProgram = null

    private def logError(msg: String): Unit = {
        errors + msg
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
            case (StateType(c1, s1), c@ContractType(c2)) =>
                if (c1 == c2) Some(c) else None
            case (c@ContractType(c1), State(c2, s2)) =>
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
                new ReadOnlyRef(t)
            } else if (mod == IsOwned) {
                new OwnedRef(t)
            } else {
                // TODO: are main contracts always deemed shared by the type system?
                new SharedRef(t)
            }
        }

        t match {
            case AstIntType() => new IntType()
            case AstBoolType() => new BoolType()
            case AstStringType() => new StringType()
            case AstContractType(mods, name) =>
                getModifier(new ContractType(name), mods, name)
            case AstStateType(mods, nameC, nameS) =>
                getModifier(new StateType(nameC, nameS), mods, nameC)
        }
    }

    private def checkExpr(
                             insideOfMethod: Either[Transaction, Func],
                             insideOfContract: Either[IndexedState, IndexedContract],
                             context: Context,
                             e: Expression
                         ): (Type, Context) = {
        (BottomType(), context)
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
                                          insideOfContract: Either[IndexedState, IndexedContract],
                                          context: Context,
                                          s: Seq[Statement]
                                      ): Context = {
        s.foldLeft(context)((prevContext: Context, s: Statement) =>
                checkStatement(insideOfMethod, insideOfContract, prevContext, s))
    }

    private def mergeContext(context1: Context, context2: Context): Context = {
        var mergedContext = new TreeMap[String, Type]()

        val onlyIn1 = context1.keys.toSet - context2.keys.toSet
        val onlyIn2 = context2.keys.toSet - context1.keys.toSet
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

    // returns [Some(err)] if [spec] and [args] match, [None] if they do
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

    private def checkStatement(
                                  insideOfMethod: Either[Transaction, Func],
                                  insideOfContract: Either[IndexedState, IndexedContract],
                                  context: Context,
                                  s: Statement
                              ): Context = {

        val checkExpr = (context: Context, e: Expression) =>
            this.checkExpr(insideOfMethod, insideOfContract, context, e)
        val checkExprs = (context: Context, es: Seq[Expression]) =>
            this.checkExprs(insideOfMethod, insideOfContract, context, es)

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
                    case Left(Transaction(_, _, Some(t), _, _)) => translateType(t)
                    case Right(Func(_, _, Some(t), _)) => translateType(t)
                    case _ =>
                        logError("Cannot return a value for a method with no return type")
                        return contextPrime
                }
                assertSubType(t, tRet)
                contextPrime

            case Transition(newStateName: String, updates: Seq[(Variable, Expression)]) =>
                val (cName, sName) = extractSimpleType(context("this")) match {
                    case Some(StateType(c, s)) => (c, s)
                    case _ =>
                        logError("'this' must be typed to a particular state to transition")
                        return context
                }

                val indexedContract = insideOfContract.fold(_.getContract, identity)
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
                        return contextPrime2

                    // assignment target is neither a variable nor a field
                    case _ =>
                        logError("Assignment target must be a local variable or a field")
                        return contextPrime
                }

            case Throw() => context

            case If(eCond: Expression, s: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
                assertSubType(t, BoolType())
                checkStatementSequence(insideOfMethod, insideOfContract, contextPrime, s)

            case IfThenElse(eCond: Expression, s1: Seq[Statement], s2: Seq[Statement]) =>
                val (t, contextPrime) = checkExpr(context, eCond)
                assertSubType(t, BoolType())
                val contextIfTrue =
                    checkStatementSequence(insideOfMethod, insideOfContract, contextPrime, s1)
                val contextIfFalse =
                    checkStatementSequence(insideOfMethod, insideOfContract, contextPrime, s2)
                mergeContext(contextIfFalse, contextIfTrue)

            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) =>
                val contextIfTry =
                    checkStatementSequence(insideOfMethod, insideOfContract, context, s1)
                val contextIfCatch =
                    checkStatementSequence(insideOfMethod, insideOfContract, context, s2)
                mergeContext(contextIfTry, contextIfCatch)

            case Switch(e: Expression, cases: Seq[SwitchCase]) => context

            case LocalInvocation(name: String, args: Seq[Expression]) =>
                val contract = insideOfContract.fold(_.getContract, identity)
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
                    case (Some(Transaction(_, args, ret, _, _)), _) => (args, ret)
                    case (_, Some(Func(_, args, ret, _))) => (args, ret)
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
                        contract.getConstructors()
                }

                val (argTypes, contextPrime) = checkExprs(context, args)
                assertArgsCorrect(constructors.map(_.args).toSeq, argTypes)

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

    private def checkField(field: Field): Option[String] = {
        translateType(field.typ) match {
            case OwnedRef(simple) => checkSimpleType(simple)

            case SharedRef(StateType(_, _)) => Some(s"State-specific types are not safe for 'shared' references")
            case SharedRef(simple) => checkSimpleType(simple)

            case ReadOnlyRef(StateType(_, _)) => Some(s"State-specific types are not safe for 'readonly' references")
            case ReadOnlyRef(simple) => checkSimpleType(simple)

            case _ => None
        }
    }

    private def checkTransaction(tx: Transaction, insideOf: Contract): Option[String] = {
        None // todo
    }

    private def checkFunc(func: Func, insideOf: Contract): Option[String] = {
        None // todo
    }

    private def checkContract(contract: Contract): Option[String] = {
        None // todo
    }

    private def indexDecl(decls: Seq[Declaration]): (Map[String, Field], Map[String, Transaction], Map[String, Func]) = {
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

    def checkProgram(program: Program): Option[String] = {
        indexProgram(program)
        for (contract <- program.contracts) {
            checkContract(contract) match {
                case err@Some(_) => return err
                case _ => ()
            }
        }

        None
    }
}

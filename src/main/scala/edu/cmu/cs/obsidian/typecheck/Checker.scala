package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.{HashSet, TreeMap, TreeSet}



/* Either a raw type or a primitive type. This class of types is important because
 * they can be translated purely syntactically from AST types (i.e. no resolving of
 * path/contract/state names is required */


/* We define a custom type to store a special flag for if a context in after a "throw".
 * In the formalism, we allow throw to result in any type: in the implementation, we don't know
 * immediately which type this needs to be in order for type checking to work
 * transitionFieldsInitialized is a set of (state, field, AST) triples that are guaranteed to have been initialized.
 * The AST is for error message generation.
 */

case class Context(underlyingVariableMap: Map[String, ObsidianType], isThrown: Boolean, transitionFieldsInitialized: Set[(String, String, AST)]) {
    def keys: Iterable[String] = underlyingVariableMap.keys

    def updated(s: String, t: ObsidianType): Context =
        Context(underlyingVariableMap.updated(s, t), isThrown, transitionFieldsInitialized)
    def updatedWithInitialization(stateName: String, fieldName: String, ast: AST): Context =
        Context(underlyingVariableMap, isThrown, transitionFieldsInitialized + ((stateName, fieldName, ast)))

    def updatedWithoutAnyTransitionFieldsInitialized(): Context =
        Context(underlyingVariableMap, isThrown, Set.empty)

    def get(s: String): Option[ObsidianType] = underlyingVariableMap.get(s)

    def apply(s: String): ObsidianType = underlyingVariableMap(s)

    def unresolvedContext: Map[String, PotentiallyUnresolvedType] = {
        var rawContext = TreeMap[String, PotentiallyUnresolvedType]()
        for ((x, t) <- underlyingVariableMap) {
            t match {
                case BottomType() => ()
                case u@UnresolvedNonprimitiveType(identifiers, mods) => rawContext = rawContext.updated(x, u)
                case np@NonPrimitiveType(table, typ, mods) => rawContext.updated(x, np)
                case prim@IntType() => rawContext = rawContext.updated(x, prim)
                case prim@StringType() => rawContext = rawContext.updated(x, prim)
                case prim@BoolType() => rawContext = rawContext.updated(x, prim)
            }
        }
        rawContext
    }

    def fieldIsInitialized(stateName: String, fieldName: String): Boolean =
        transitionFieldsInitialized.find((e) => e._1 == stateName && e._2 == fieldName).isDefined

    def makeThrown: Context = this.copy(isThrown = true)


    @deprecated("todo", "todo")
    def tableOfThis: DeclarationTable = this ("this").asInstanceOf[NonPrimitiveType].table

    def contractTable: ContractTable = thisType.table.contractTable

    def thisType: NonPrimitiveType = get("this").get.asInstanceOf[NonPrimitiveType]


    // Looks up fields, transactions, etc., checking to make sure they're available in all
    // possible current states of "this".
    private def doLookup[FoundType <: IsAvailableInStates](toFind: String,
                                                           lookupFunction: (String => Option[FoundType])): Option[FoundType] = {
        val foundOption: Option[FoundType] = lookupFunction(toFind)
        if (foundOption.isEmpty) {
            // The thing isn't defined on this contract or state.
            None
        }
        else {
            val foundObject = foundOption.get
            // Make sure that no matter what state "this" is in, the field is available.
            val isAvailable: Boolean = thisType.extractSimpleType match {
                case Some(StateUnionType(c, possibleStateNames)) =>
                    // Make sure the variable is available in all of the states we might be in.
                    // missingStateNames are the states that do NOT include the field we need.
                    val availableInStatesOpt = foundObject.availableIn
                    if (availableInStatesOpt.isEmpty) {
                        // The field is available in all states, so no matter what states we might be in, we're okay.
                        true
                    }
                    else {
                        val availableInStates = availableInStatesOpt.get.map(_._1)
                        val missingStateNames = possibleStateNames.filterNot((possibleStateName: String) =>
                            availableInStates.contains(possibleStateName))
                        missingStateNames.isEmpty
                    }

                case Some(JustContractType(contractName)) =>
                    // Make sure the variable is available in every state of the contract.\
                    foundObject.availableIn == None
                case Some(StateType(contractName, stateName)) =>
                    // The variable has to be available because we knew what state "this" was in at lookup time.
                    true
                case None =>
                    // Somehow this is a primitive type?
                    assert(false, "this has invalid type")
                    true
            }
            if (isAvailable) {
                Some(foundObject)
            }
            else {
                None
            }
        }
    }

    // The field has to be available in all possible states.
    def lookupFieldTypeInThis(fieldName: String): Option[ObsidianType] = {
        val foundField = doLookup(fieldName, (field: String) => thisType.table.lookupField(field))

        foundField match {
            case None => None
            case Some(field) => Some(field.typ)
        }
    }

    def lookupTransactionInThis(transactionName: String): Option[Transaction] = {
        lookupTransactionInType(thisType)(transactionName)
    }

    def lookupFunctionInThis(functionName: String): Option[Func] = {
        lookupFunctionInType(thisType)(functionName)
    }

    def lookupTransactionInType(typ: ObsidianType) (transactionName: String): Option[Transaction] = {
        typ.tableOpt match {
            case None => None
            case Some (table) => doLookup(transactionName, (transaction: String) => table.lookupTransaction(transaction))
        }
    }

    def lookupFunctionInType(typ: ObsidianType) (functionName: String): Option[Func] = {
        typ.tableOpt match {
            case None => None
            case Some (table) => doLookup(functionName, (function: String) => table.lookupFunction(function))
        }
    }
}

class Checker(globalTable: SymbolTable, verbose: Boolean = false) {
    /* only stores [PotentiallyUnresolvedType]s; all types in the context can thus be
     * resolved purely syntactically */
    type UnresolvedContext = Map[String, PotentiallyUnresolvedType]

    val errors = new collection.mutable.ArrayStack[ErrorRecord]()

    /* an error is associated with an AST node to indicate where the error took place */
    private def logError(where: AST, err: Error): Unit = {
        assert(where.loc.line >= 1)
        errors.push(ErrorRecord(err, where.loc))

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

    private def updatedSimpleType(t: UnpermissionedType, newSimple: SimpleType): UnpermissionedType = {
        t match {
            case NoPathType(_) => NoPathType(newSimple)
            case PathType(p, _) => PathType(p, newSimple)
        }
    }

    private def updatedSimpleType(t: ObsidianType, newSimple: SimpleType): ObsidianType = {
        t match {
            case NonPrimitiveType(table, typ, mods) => NonPrimitiveType(table, updatedSimpleType(typ, newSimple), mods)
            case ts => ts
        }
    }

    private def updatedUnpermissionedType(t: ObsidianType, newRaw: UnpermissionedType): ObsidianType = {
        t match {
            case NonPrimitiveType(table, typ, mods) => NonPrimitiveType(table, newRaw, mods)
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
            case (StateUnionType(c, ss1), JustContractType(c2)) =>
                c == c2
            case _ => false
        }
    }

    /* true iff [t1 <: t2] */
    private def isSubtype(t1: ObsidianType, t2: ObsidianType): Option[Error] = {
        (t1, t2) match {
            case (BottomType(), _) => None
            case (IntType(), IntType()) => None
            case (BoolType(), BoolType()) => None
            case (StringType(), StringType()) => None
            case (NonPrimitiveType(_, typ1, mods1), NonPrimitiveType(_, typ2, mods2)) =>
                val mainSubtype = (typ1, typ2) match {
                    case (NoPathType(ts1), NoPathType(ts2)) => isSimpleSubtype(ts1, ts2)
                    case (PathType(p1, ts1), PathType(p2, ts2)) if p1 == p2 =>
                        isSimpleSubtype(ts1, ts2)
                    case _ => false
                }

                // For now, just make sure we don't get ownership from an expression that doesn't have it.
                val modifierSubtype = if (mods2.contains(IsOwned())) {
                    mods1.contains(IsOwned())
                }
                else {
                    true
                }

                if (!mainSubtype) Some(SubTypingError(t1, t2))
                else if (!modifierSubtype) Some(OwnershipSubtypingError(t1, t2))
                else None
            case _ => Some(SubTypingError(t1, t2))
        }
    }

    /* returns [t1] if [t1 <: t2], logs an error and returns [BottomType] otherwise */
    private def checkIsSubtype(ast: AST, t1: ObsidianType, t2: ObsidianType): ObsidianType = {
        val errorOpt = isSubtype(t1, t2)
        if (errorOpt.isDefined) {
            logError(ast, errorOpt.get)
            BottomType()
        }
        else t1
    }

    //-------------------------------------------------------------------------
    /* Helper functions to easily make new types */

    /* Determines what sort of simple type should be used, given a set a possible states */
    private def simpleOfWithStateNames(cName: String, states: Option[Set[String]]): SimpleType = {
        states match {
            case None => JustContractType(cName)
            case Some(ss) => if (ss.size > 1) {
                StateUnionType(cName, ss)
            } else {
                StateType(cName, ss.head)
            }
        }
    }

    // Usually we have the identifiers available, so this is the main entry point.
    private def simpleOf(lexicallyInsideOf: DeclarationTable, cName: String, states: Option[Set[Identifier]]): SimpleType = {
        val stateNames = states match {
            case None => None
            case Some(ss) => Some(ss.map(_._1))
        }
        simpleOfWithStateNames(cName, stateNames)
    }

    private def unpermissionedOf(simple: SimpleType, path: Option[Seq[String]]): UnpermissionedType = {
        path match {
            case None => NoPathType(simple)
            case Some(p) => PathType(p, simple)
        }
    }

    //-------------------------------------------------------------------------
    /* the upper bound U of two types T1 and T2 is a type such that
     * subtypeOf(T1, U) and subtypeOf(t2, U). Such a type doesn't always exist. */

    private def simpleUpperBound(
            t1: SimpleType,
            t2: SimpleType): Option[SimpleType] = {
        def handleStateUnion(ss1: Set[String], ss2: Set[String]): Option[SimpleType] = {
            val c = t1.contractName
            val unionStates = ss1.union(ss2)
            Some(simpleOfWithStateNames(c, Some(unionStates)))
        }
        if (t1.contractName != t2.contractName) return None
        (t1, t2) match {
            case (_, JustContractType(_)) => Some(t2)
            case (JustContractType(_), _) => Some(t1)
            case (StateType(c, s1), StateType(_, s2)) =>
                if (s1 == s2) Some(StateType(c, s1))
                else handleStateUnion(TreeSet[String]() + s1, TreeSet[String]() + s2)
            case (StateUnionType(_, ss1), StateUnionType(_, ss2)) =>
                handleStateUnion(ss1, ss2)
            case (StateUnionType(_, ss), StateType(_, s)) =>
                handleStateUnion(ss, TreeSet[String]() + s)
            case (StateType(_, s), StateUnionType(_, ss)) =>
                handleStateUnion(ss, TreeSet[String]() + s)
            case _ => None
        }
    }

    private def unpermissionedUpperBound(
                                 t1: UnpermissionedType,
                                 t2: UnpermissionedType): Option[UnpermissionedType] = {
        (t1, t2) match {
            case (NoPathType(ts1), NoPathType(ts2)) =>
                simpleUpperBound(ts1, ts2).map(NoPathType)
            case (PathType(p1, ts1), PathType(p2, ts2)) if p1 == p2 =>
                simpleUpperBound(ts1, ts2).map(PathType(p1, _))
            case _ => None
        }
    }

    private def upperBound(t1: ObsidianType, t2: ObsidianType): Option[ObsidianType] = {
        (t1, t2) match {
            case (IntType(), IntType()) => Some(IntType())
            case (BoolType(), BoolType()) => Some(BoolType())
            case (StringType(), StringType()) => Some(StringType())
            case (NonPrimitiveType(table, typ1, mods1), NonPrimitiveType(_, typ2, mods2)) => assert(mods1 == mods2);
                unpermissionedUpperBound(typ1, typ2).flatMap(s => Some(NonPrimitiveType(table, s, mods1)))
            case _ => None
        }
    }


    /* assumes that [t] is not a primitive type */
    private def extractModifiers(t: ObsidianType): Set[TypeModifier] = {
        t match {
            case typ@NonPrimitiveType(_, _, mods) => mods
            case _ => Set.empty[TypeModifier]
        }
    }

    /* adds the modifier from [t] to [tr], assuming that [table] is the
     * symbol table of the type [tr] */
    private def addModifiers(
                               tr: UnpermissionedType,
                               table: DeclarationTable,
                               mods: Set[TypeModifier]): ObsidianType = {
        NonPrimitiveType(table, tr, mods)
    }


    //-------------------------------------------------------------------------
    // Checking definitions for language constructs begins here

    private def inferAndCheckExpr(decl: InvokableDeclaration,
                                  context: Context,
                                  e: Expression,
                                  consumeOwnershipIfOwned: Boolean = false): (ObsidianType, Context) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: ObsidianType, c: Context): (ObsidianType, Context) = {
            val (tPrime, contextPrime) = inferAndCheckExpr(decl, c, e)
            (checkIsSubtype(e, tPrime, t), contextPrime)
        }

        def assertOperationType(e1: Expression, e2: Expression, t: ObsidianType): (ObsidianType, Context) = {
            val (_, c1) = assertTypeEquality(e1, t, context)
            val (_, c2) = assertTypeEquality(e2, t, c1)
            (t, c2)
        }

        def assertComparisonType(e1: Expression, e2: Expression): (ObsidianType, Context) = {
            val (_, c1) = assertTypeEquality(e1, IntType(), context)
            val (_, c2) = assertTypeEquality(e2, IntType(), c1)
            (BoolType(), c2)
        }

        def handleInvocation(
                context: Context,
                receiverType: ObsidianType,
                name: String,
                receiver: Expression,
                args: Seq[Expression]): (ObsidianType, Context) = {
            // Lookup the invocation

            val foundTransaction =
                if (receiverType == context.thisType) {
                    context.lookupTransactionInThis(name)
                }
                else {
                    receiverType.tableOpt.get.lookupTransaction(name)
                }
            val foundFunction =
                if (receiverType == context.thisType) {
                    context.lookupFunctionInThis(name)
                }
                else {
                    receiverType.tableOpt.get.lookupFunction(name)
                }


            val invokable: InvokableDeclaration = (foundTransaction, foundFunction) match {
                case (None, None) =>
                    val err = MethodUndefinedError(receiverType.extractSimpleType.get, name)
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
            val retTypeCalleePoV: UnpermissionedType = astType match {
                case prim: PrimitiveType => return (prim, contextPrime)
                case np: NonPrimitiveType => np.extractUnpermissionedType.get
                case u: UnresolvedNonprimitiveType =>
                    assert(false); NoPathType(JustContractType("ERROR"))
                case b: BottomType => assert(false); NoPathType(JustContractType("ERROR"))
            }

            toCallerPoV(calleeToCaller, retTypeCalleePoV) match {
                case Right(retTypeCallerPoV) =>
                    (astType, contextPrime)
                case Left((first, badExpr)) =>
                    val err = CannotConvertPathError(first, badExpr, retTypeCalleePoV)
                    logError(e, err)
                    (BottomType(), contextPrime)
            }
        }

         e match {
             case Variable(x) =>
                 (context get x, context.lookupFieldTypeInThis(x)) match {
                     case (Some(t), _) =>
                         if (consumeOwnershipIfOwned) {
                             (t, context.updated(x, t.residualType))
                         }
                         else {
                             (t.residualType, context)
                         }
                     case (_, Some(t)) =>
                         // TODO handle cases for e.g. if the field is owned
                         (t, context)
                     case (None, None) =>
                         logError(e, VariableUndefinedError(x, context.thisType.toString))
                         (BottomType(), context)
                 }
             case OwnershipTransfer(e) =>
                 e match {
                     case Variable(x) =>
                         inferAndCheckExpr(decl, context, Variable(x), consumeOwnershipIfOwned = true)
                     case _ =>
                         val (typ, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = true)
                         if (!typ.isOwned) {
                             logError(e, InvalidOwnershipTransfer(e, typ))
                         }
                         (typ, contextPrime)
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
                 assert(false, "TODO: re-add support for parents")
                 /*
                 val thisTable = context.tableOfThis.contractTable
                 if (thisTable.hasParent) {
                     val parentTable = thisTable.parent.get
                     val ts = parentTable.simpleType
                     val tr = if (parentTable.hasParent) {
                         PathType("this"::"parent"::"parent"::Nil, ts)
                     } else {
                         NoPathType(ts)
                     }

                     (addModifiers(tr, parentTable, Set()), context)

                 } else {
                     logError(e, NoParentError(thisTable.name))
                     (BottomType(), context)
                 }
                 */
                 (BottomType(), context)
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

                 (fieldAST.typ, contextPrime)

             case LocalInvocation(name, args: Seq[Expression]) =>
                 handleInvocation(context, context.thisType, name, This(), args)

             case Invocation(receiver: Expression, name, args: Seq[Expression]) =>
                 val (receiverType, contextPrime) = inferAndCheckExpr(decl, context, receiver)

                 receiverType match {
                     case BottomType() => return (BottomType(), contextPrime)
                     case IntType() | BoolType() | StringType() =>
                         logError(e, NonInvokeableError(receiverType))
                         return (BottomType(), contextPrime)
                     // [get] is safe because [receiverType] must be non-primitive
                     case _ =>
                         if (!receiverType.tableOpt.isDefined)
                             return (BottomType(), contextPrime)

                 }

                 handleInvocation(contextPrime, receiverType, name, receiver, args)

             case Construction(name, args: Seq[Expression]) =>
                 val tableLookup = context.contractTable.lookupContract(name)
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

                 // TODO: https://github.com/mcoblenz/Obsidian/issues/63
                 val (simpleType, modifiers: Set[TypeModifier]) = result match {
                     // Even if the args didn't check, we can still output a type
                     case None => (JustContractType(name), Set.empty[TypeModifier])
                     case Some((constr, _)) =>
                         (simpleOf(contextPrime.contractTable, name, constr.endsInState), if (constr.isOwned) Set(IsOwned()) else Set())
                 }

                 val unpermissionedType = unpermissionedOf(simpleType, path)

                 (NonPrimitiveType(contextPrime.contractTable, unpermissionedType, modifiers), contextPrime)
             case Disown(e) =>
                 // The expression "disown e" evaluates to an unowned value but also side-effects the context
                 // so that e is no longer owned (if it is a variable).
                 val (typ, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = true)
                 if (!typ.isOwned) {
                    logError(e, DisownUnowningExpressionError(e))
                 }

                 val newTyp = typ match {
                     case NonPrimitiveType(table, t, mods) => NonPrimitiveType(table, t, mods - IsOwned())
                     case t => t
                 }

                 // If e is a variable, then we need to update the context to indicate that it's no longer owned.
                 val finalContext = e match {
                     case Variable(x) => contextPrime.updated(x, newTyp)
                     case _ => contextPrime
                 }
                 (newTyp, finalContext)
             case StateInitializer(stateName, fieldName) =>
                 // A state initializer expression has its field's type.

                 val stateOption = context.contractTable.state(stateName._1)
                 val fieldType = stateOption match {
                     case None => logError(e, StateUndefinedError(context.contractTable.name, stateName._1)); BottomType()
                     case Some(stateTable) =>
                         stateTable.lookupField(fieldName._1) match {
                             case None => logError(e, FieldUndefinedError(stateTable.simpleType, fieldName._1)); BottomType()
                             case Some(field) => field.typ
                         }
                 }

                 (fieldType, context)
         }
    }


    private def inferAndCheckExprs(
                                    decl: InvokableDeclaration,
                                    context: Context,
                                    es: Seq[Expression]
                                ): (Seq[(ObsidianType, Expression)], Context) = {
        val types = new ListBuffer[(ObsidianType, Expression)]()
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


    // Checks for unused owned variables in the context, ignoring the ones listed as exceptions.
    private def checkForUnusedOwnershipErrors(ast: AST, context: Context, exceptions: Set[String]) = {
        for ((x, typ) <- context.underlyingVariableMap) {
            if (!exceptions.contains(x)) {
                typ match {
                    case t: NonPrimitiveType =>
                        val contract = t.table.contract
                        if (t.isOwned && contract.isResource) logError(ast, UnusedOwnershipError(x))
                    case _ => ()
                }
            }
        }
    }

    private def checkForUnusedStateInitializers(context: Context) = {
        for (remainingInitialization <- context.transitionFieldsInitialized) {
            logError(remainingInitialization._3, InvalidStateFieldInitialization(remainingInitialization._1, remainingInitialization._2))
        }
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

        checkForUnusedOwnershipErrors(ast, branchContext, oldContext.keys.toSet)

        Context(newContext.underlyingVariableMap, isThrown = branchContext.isThrown, newContext.transitionFieldsInitialized)
    }

    private def mergeContext(
            ast: AST,
            context1: Context,
            context2: Context): Context = {
        /* If we're merging with a context from a "throw", just take the other context
        * emit no errors */
        if (context1.isThrown && !context2.isThrown) return context2
        if (!context1.isThrown && context2.isThrown) return context1

        var mergedMap = new TreeMap[String, ObsidianType]()

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

        Context(mergedMap, context1.isThrown, context1.transitionFieldsInitialized.intersect(context2.transitionFieldsInitialized))
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
            tr: UnpermissionedType): Either[(String, Expression), UnpermissionedType] = {
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
    private def fixUnpermissionedType(context: Context, tr: UnpermissionedType): UnpermissionedType = {
        tr match {
            case PathType(inContext +: "parent" +: rest, ts) if inContext != "this" =>
                context.get(inContext) match {
                    case Some(t) =>
                        t.extractUnpermissionedType match {
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
            args: Seq[(ObsidianType, Expression)]): Either[Seq[(AST, Error)], Map[String, Expression]] = {

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

            val specCallerPoV: Seq[ObsidianType] = spec.map(arg => {
                arg.typ match {
                    case prim: PrimitiveType => prim
                    case np@NonPrimitiveType(table, unpermissionedType, mods) =>
                        toCallerPoV(calleeToCaller, unpermissionedType) match {
                            case Left((head, e)) =>
                                return Left((ast, CannotConvertPathError(head, e, unpermissionedType))::Nil)
                            case Right(trNew) => NonPrimitiveType(table, fixUnpermissionedType(context, trNew), mods)
                        }
                    case b@BottomType() => BottomType()
                    case u@UnresolvedNonprimitiveType(_, _) => assert(false); u
                }
            })

            var errList: List[(AST, Error)] = Nil
            for (i <- args.indices) {
                val (argTypeCallerPoV, _) = args(i)
                val specTypeCallerPoV = specCallerPoV(i)

                if (isSubtype(argTypeCallerPoV, specTypeCallerPoV).isDefined) {
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
            args: Seq[(ObsidianType, Expression)]): Option[(U, Map[String, Expression])] = {

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
                name: String,
                receiver: Expression,
                args: Seq[Expression]): Context = {
            // Lookup the invocation
            val (receiverType, contextAfterReceiver) = inferAndCheckExpr(decl, context, receiver)
            val txLookup = contextAfterReceiver.lookupTransactionInType(receiverType)(name)
            val funLookup = contextAfterReceiver.lookupFunctionInType(receiverType)(name)

            val invokable: InvokableDeclaration = (txLookup, funLookup) match {
                case (None, None) =>
                    val err = MethodUndefinedError(contextAfterReceiver.thisType.extractSimpleType.get, name)
                    logError(s, err)
                    return contextAfterReceiver
                case (_, Some(f)) => f
                case (Some(t), _) => t
            }

            // check arguments
            val (argTypes, contextPrime) = inferAndCheckExprs(decl, contextAfterReceiver, args)
            val specList = (invokable.args, invokable)::Nil

            val (correctInvokable, calleeToCaller) =
                checkArgs(s, name, contextAfterReceiver, specList, receiver, argTypes) match {
                    case None => return contextPrime
                    case Some(x) => x
                }

            val spec = correctInvokable.args

            val retOpt = correctInvokable.retType

            if (retOpt.isDefined) {
                retOpt.get match {
                    // TODO: Is this check actually necessary?
                    case nonprimitiveType@NonPrimitiveType(_, unpermissionedType, _) =>
                        val unpermissionedTypeOurPoV = toCallerPoV(calleeToCaller, unpermissionedType)
                        if (unpermissionedTypeOurPoV.isLeft) {
                            val (first, badExpr) = unpermissionedTypeOurPoV.left.get
                            val err = CannotConvertPathError(first, badExpr, unpermissionedType)
                            logError(s, err)
                        }
                    case _ => ()
                }

                // check that no ownership is leaked by the (necessarily unused) return value
                if (retOpt.get.isOwned) {
                    logError(s, LeakReturnValueError(name))
                }
            }

            contextPrime
        }

        s match {
            case VariableDecl(typ: ObsidianType, name) =>
                context.updated(name, typ)

            case VariableDeclWithInit(typ: ObsidianType, name, e: Expression) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e)
                val tDecl = typ match {
                    case NonPrimitiveType(table, unpermissionedType, mods) =>
                        val simpleType = unpermissionedType.extractSimpleType
                        val contractName = simpleType.contractName

                        globalTable.contract(contractName) match {
                            case None =>
                                logError(s, ContractUndefinedError(contractName))
                                BottomType()
                            case Some(_) => typ
                        }
                    case BottomType() => BottomType()
                    case t => t
                }
                if (tDecl != BottomType()) {
                    checkIsSubtype(s, t, tDecl)
                }
                contextPrime.updated(name, tDecl)

            case Return() =>
                decl match {
                    /* the tx/function must have no return type */
                    case tx: Transaction if tx.retType.isEmpty =>
                        checkForUnusedOwnershipErrors(s, context, Set("this"))
                        context.makeThrown
                    case f: Func if f.retType.isEmpty =>
                        checkForUnusedOwnershipErrors(s, context, Set("this"))
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
                        val variablesToExcludeFromOwnershipCheck =
                            if (tx.retType.get.isOwned) {
                                e match {
                                    case Variable(xOther) => Set(xOther, "this")
                                    case _ => Set("this")
                                }
                            }
                            else {
                                Set("this")
                            }
                        checkForUnusedOwnershipErrors(s, contextPrime, variablesToExcludeFromOwnershipCheck)

                        (tx.retType.get, tx.retType.get)
                    case f: Func if f.retType.isDefined =>
                        val variablesToExcludeFromOwnershipCheck =
                            if (f.retType.get.isOwned) {
                                e match {
                                    case Variable(xOther) => Set(xOther, "this")
                                    case _ => Set("this")
                                }
                            }
                            else {
                                Set("this")
                            }
                        checkForUnusedOwnershipErrors(s, contextPrime, variablesToExcludeFromOwnershipCheck)

                        (f.retType.get, f.retType.get)
                    case _ =>
                        logError(s, CannotReturnError(decl.name))
                        return contextPrime.makeThrown
                }

                if (!tRet.isBottom) checkIsSubtype(s, t, tRet)
                contextPrime.makeThrown

            case Transition(newStateName, updates: Option[Seq[(Variable, Expression)]]) =>
                val thisTable = context.contractTable

                if (thisTable.state(newStateName).isEmpty) {
                    logError(s, StateUndefinedError(thisTable.name, newStateName))
                    return context
                }

                val newStateTable = thisTable.state(newStateName).get

                val oldType = context.thisType

                // First we focus on the fields declared in states individually.
                // oldFields is the set of fields declared in the old state, which are definitely going away.
                // maybeOldFields is the set of fields from the old state that MAY be going away — 
                //   we can't be sure when the current state is a union.

                val possibleCurrentStates = oldType.extractSimpleType.get match {
                    case JustContractType(contractName) => thisTable.possibleStates
                    case StateType(contractName, stateName) => Set(stateName)
                    case StateUnionType(contractName, stateNames) => stateNames
                }


                val oldStateTables = possibleCurrentStates.map((name: String) => thisTable.state(name).get)
                var oldFieldSets: Set[Set[Declaration]] = oldStateTables.map((t: StateTable) => t.ast.declarations.toSet)

                val (oldFields: Set[(String, ObsidianType)], maybeOldFields: Set[(String, ObsidianType)]) =
                // We don't have a statically-fixed set of old fields because we don't know statically
                // which specific state we're in. We take a conservative approach:
                // take the intersection to ensure that all fields might need to be initialized will be initialized.
                    if (decl.isInstanceOf[Constructor] || oldFieldSets.isEmpty) {
                        (Set.empty[(String, ObsidianType)], Set.empty[(String, ObsidianType)])
                    }
                    else {
                        val oldFieldNamesSets: Set[Set[(String, ObsidianType)]] = oldFieldSets.map(
                            (decls: Set[Declaration]) => decls.map((d: Declaration) => (d.name, d.asInstanceOf[Field].typ))
                        )

                        (oldFieldNamesSets.tail.foldLeft(oldFieldNamesSets.head) {
                            ((intersections, next) => intersections.intersect(next))
                        },
                            oldFieldNamesSets.tail.foldLeft(oldFieldNamesSets.head) {
                                ((unions, next) => unions.union(next))
                            })

                    }

                val newStateFields = newStateTable.ast.declarations
                                .filter(_.isInstanceOf[Field])
                                .map((decl: Declaration) => (decl.asInstanceOf[Field].name, decl.asInstanceOf[Field].typ))
                val contractDeclarations = thisTable.contractTable.ast.asInstanceOf[Contract].declarations
                val contractFieldDeclarationsAvailableInNewState: Seq[Declaration] =
                    contractDeclarations.filter(
                        (d: Declaration) => if (d.tag == FieldDeclTag) {
                            val availableIn = d.asInstanceOf[Field].availableIn
                            availableIn match {
                                case None => false // We're not interested in fields that are available in all states because they were available in the old state too.
                                case Some(stateIdentifiers) =>
                                    val availableInStateNames = stateIdentifiers.map(_._1)
                                    availableInStateNames.contains(newStateName)
                            }
                        }
                        else {
                            false
                        }
                    )
                val fieldNamesInThisState = contractFieldDeclarationsAvailableInNewState.map(
                    (decl: Declaration) => (decl.asInstanceOf[Field].name, decl.asInstanceOf[Field].typ))
                val newFields: Seq[(String, ObsidianType)] = newStateFields ++ fieldNamesInThisState

                val toInitialize = newFields.toSet.diff(oldFields) // All the fields that must be initialized.

                // We require that all the fields of the new state that don't exist in the current state be initialized.
                // However, shared fields may be initialized too.

                val updatedInTransition: Set[String] = updates match {
                    case Some(u) => u.map(_._1.name).toSet
                    case None => Set.empty
                }
                val testStateMatch = (updateInfo: (String, String, AST)) => updateInfo._1 == newStateName

                val updatedViaAssignment: Set[String] = context.transitionFieldsInitialized.filter(testStateMatch).map(_._2)


                val updatedViaAssignmentToWrongState = context.transitionFieldsInitialized.filterNot(testStateMatch)
                for (invalidAssignment <- updatedViaAssignmentToWrongState) {
                    logError(invalidAssignment._3, InvalidStateFieldInitialization(invalidAssignment._1, invalidAssignment._2))
                }

                val updated = updatedInTransition.union(updatedViaAssignment) // Fields updated by either assignment or transition initialization
                val uninitialized = toInitialize.filterNot((p: (String, ObsidianType)) => updated.contains(p._1))

                if (uninitialized.nonEmpty) logError(s, TransitionUpdateError(uninitialized.map(_._1)))

                val badInitializations = updated.diff(newFields.map(_._1).toSet) // We don't allow updates to fields that don't exist in the target state.
                for (s <- badInitializations) {
                    val err = FieldUndefinedError(newStateTable.simpleType, s)
                    logError(updates.get.find(_._1.name == s).get._1, err)
                }

                var contextPrime = context
                if (updates.isDefined) {
                    for ((Variable(f), e) <- updates.get) {
                        if (newFields.contains(f)) {
                            val fieldAST = newStateTable.lookupField(f).get
                            val (t, contextPrime2) = inferAndCheckExpr(decl, contextPrime, e)
                            contextPrime = contextPrime2
                            checkIsSubtype(s, t, fieldAST.typ)
                        }
                    }
                }

                // Check for potentially-dropped resources.
                val toCheckForDroppedResources = maybeOldFields.diff(newFields.toSet) // fields that we might currently have minus fields we're initializing now
                for (oldField <- toCheckForDroppedResources) {
                    val fieldType = oldField._2
                    if (fieldType.isOwned && fieldType.isResourceReference) {
                        logError(s, PotentiallyUnusedOwnershipError(oldField._1))
                    }
                }

                val newTypeTable = thisTable.contractTable.state(newStateName).get
                val newSimpleType = StateType(thisTable.name, newStateName)

                val newType = NonPrimitiveType(newTypeTable, NoPathType(newSimpleType), oldType.extractModifiers)


                contextPrime.updated("this", newType).updatedWithoutAnyTransitionFieldsInitialized()

            case Assignment(Variable(x), e: Expression, transfersOwnership) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = transfersOwnership)

                // If we're going to transfer ownership, make sure we're starting with something owned.
                if (transfersOwnership) {
                    if (!t.isOwned) {
                        logError(s, InvalidOwnershipTransfer(e, t))
                    }
                }

                val contextType = context.get(s"$x")

                /* if the variable is not in the context, see if it's a field */
                if (contextType.isEmpty) {
                    val fieldType = contextPrime.lookupFieldTypeInThis(x)

                    /* if it's not a field either, log an error */
                    if (fieldType.isEmpty) logError(s, VariableUndefinedError(x, context.thisType.toString))
                    else checkIsSubtype(e, t, fieldType.get)
                }
                else {
                    if (t != BottomType()) {
                        checkIsSubtype(s, t, contextType.get)
                    }

                }
                contextPrime

            case Assignment(Dereference(eDeref, f), e: Expression, transfersOwnership) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = transfersOwnership)
                val (derefType, contextPrime2) = inferAndCheckExpr(decl, contextPrime, eDeref)

                // If we're going to transfer ownership, make sure we're starting with something owned.
                if (transfersOwnership) {
                    if (!t.isOwned) {
                        logError(s, InvalidOwnershipTransfer(e, t))
                    }
                }

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

                checkIsSubtype(s, t, fieldAST.typ)
                contextPrime2

            case Assignment(StateInitializer(stateName, fieldName), e, transfersOwnership) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = transfersOwnership)

                // If we're going to transfer ownership, make sure we're starting with something owned.
                if (transfersOwnership) {
                    if (!t.isOwned) {
                        logError(s, InvalidOwnershipTransfer(e, t))
                    }
                }

                val stateOption = context.contractTable.state(stateName._1)
                val fieldType = stateOption match {
                    case None => logError(s, StateUndefinedError(context.contractTable.name, stateName._1)); BottomType()
                    case Some(stateTable) =>
                        stateTable.lookupField(fieldName._1) match {
                            case None => logError(s, FieldUndefinedError(stateTable.simpleType, fieldName._1)); BottomType()
                            case Some(field) => field.typ
                        }
                }

                checkIsSubtype(s, t, fieldType)
                if (fieldType == BottomType()) {
                    contextPrime
                }
                else {
                    contextPrime.updatedWithInitialization(stateName._1, fieldName._1, s)
                }

            // assignment target is neither a variable nor a field
            case Assignment(_, e: Expression, transfersOwnership) =>
                val (_, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = transfersOwnership)
                logError(s, AssignmentError())
                contextPrime

            case Throw() => Context(context.underlyingVariableMap, isThrown = true, Set.empty)

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, eCond)
                checkIsSubtype(s, t, BoolType())
                val contextIfTrue = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body),
                    contextPrime)
                mergeContext(s, contextPrime, contextIfTrue)

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, eCond)
                checkIsSubtype(s, t, BoolType())
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
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e)

                t.tableOpt match {
                    case Some(st: StateTable) => logError(st.ast, AlreadyKnowStateError(e, st.name))
                    case Some(_) => ()
                    case None =>
                        logError(e, SwitchError(t))
                        return contextPrime
                }

                val contractTable = t.tableOpt.get.contractTable

                var mergedContext = contextPrime
                for (SwitchCase(sName, body) <- cases) {
                    val newType =
                        contractTable.state(sName) match {
                            case Some(stTable) =>
                                val newSimple = StateType(contractTable.name, stTable.name)
                                updatedSimpleType(t, newSimple)
                            case None =>
                                logError(s, StateUndefinedError(contractTable.name, sName))
                                val newSimple = JustContractType(contractTable.name)
                                updatedSimpleType(t, newSimple)
                        }

                    /* special case to allow types to change in the context if we match on a variable */
                    val startContext = e match {
                        case This() =>
                            /* reading "this" as an expression takes the residual of "this",
                             * so we want "this" in the context to have the old permission of
                             * "this" with the new state information in the unpermissioned type */
                            val newContextThisType =
                                updatedUnpermissionedType(context("this"), newType.extractUnpermissionedType.get)
                            contextPrime.updated("this", newContextThisType)
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
                handleInvocation(context, name, This(), args)

            case Invocation(receiver: Expression, name, args: Seq[Expression]) =>
                val (receiverType, contextPrime) = inferAndCheckExpr(decl, context, receiver)
                if (receiverType.isBottom) return contextPrime
                receiverType match {
                    case BottomType() => return contextPrime
                    case IntType() | BoolType() | StringType() =>
                        logError(s, NonInvokeableError(receiverType))
                        return contextPrime
                    case _ =>
                        if (receiverType.tableOpt.isEmpty)
                            return contextPrime
                }

                handleInvocation(contextPrime, name, receiver, args)

            // TODO maybe allow constructors as statements later, but it's not very important
            case d@Disown (e) =>
                val (typ, contextPrime) = inferAndCheckExpr(decl, context, d)
                contextPrime
            /* expressions are statements, but we prune out expressions with no side effects */
            case e: Expression =>
                val (typ, contextPrime) = inferAndCheckExpr(decl, context, e)
                if (typ.isOwned) {
                    logError(s, UnusedExpressionOwnershipError(e))
                }
                logError(s, NoEffectsError(s))
                contextPrime
            case _ =>
                logError(s, NoEffectsError(s))
                context
        }
    }

    private def checkField(field: Field, lexicallyInsideOf: ContractTable): Unit = {
        // TODO: this non-state-specific check should be elsewhere, since it applies to all non-owned variables (not just fields).
        def checkNonStateSpecific(simple: SimpleType, err: Error): Unit = {
            simple match {
                case JustContractType(_) => ()
                case StateType(_,_) | StateUnionType(_,_) =>
                    logError(field, err)
            }
        }
        field.typ match {
            case typ: NonPrimitiveType =>
                if (typ.isOwned) {
                    // Only resources can own other resources (since otherwise they might go out of scope improperly).
                    val referencedContractIsResource = typ.table.contract.modifiers.contains(IsResource())
                    if (referencedContractIsResource && !lexicallyInsideOf.contract.modifiers.contains(IsResource())) {
                        logError(field, NonResourceOwningResourceError(lexicallyInsideOf.name, field))
                    }
                }
                else if (typ.isReadOnlyState) {
                    val simpleType = typ.extractSimpleType.get
                    checkNonStateSpecific(simpleType, StateSpecificReadOnlyError())
                }
                else {
                    val simpleType = typ.extractSimpleType.get
                    checkNonStateSpecific(simpleType, StateSpecificSharedError())
                }
            case _ => None
        }
    }

    private def checkTransactionInState(tx: Transaction,
                                        lexicallyInsideOf: DeclarationTable,
                                        initContext: Context): Unit = {

        var context = initContext

        for (arg <- tx.args) {
            context = initContext.updated(arg.varName, arg.typ)
        }

        // Check the body; ensure [this] is well-typed after, and check for leaked ownership
        val outputContext =
            checkStatementSequence(tx, initContext, tx.body)

        // Check that all the states the transaction can end in are valid, named states
        if (tx.endsInState.isDefined) {
            for (state <- tx.endsInState.get) {
                val stateTableOpt = lexicallyInsideOf.contractTable.state(state._1)
                stateTableOpt match {
                    case None => logError(tx, StateUndefinedError(lexicallyInsideOf.contract.name, state._1))
                    case Some(_) => ()
                }
            }
        }

        val expectedType =
            NonPrimitiveType(lexicallyInsideOf,
                NoPathType(simpleOf(lexicallyInsideOf, lexicallyInsideOf.name, tx.endsInState)), Set(IsOwned()))
        checkIsSubtype(tx, outputContext("this"), expectedType)

        checkForUnusedStateInitializers(outputContext)

        if (tx.retType.isDefined & !hasReturnStatement(tx, tx.body)) {
            logError(tx.body.last, MustReturnError(tx.name))
        }
        else if (!tx.retType.isDefined) {
            // We check for unused ownership errors at each return; if there isn't guaranteed to be one at the end, check separately.
            checkForUnusedOwnershipErrors(tx, outputContext, Set("this"))
        }

        // todo: check that every declared variable is initialized before use
    }


    private def checkTransaction(tx: Transaction, lexicallyInsideOf: DeclarationTable): Unit = {


        // Construct the set of states that the transaction might start in.
        val startStates: Set[StateTable] =
            if (tx.availableIn.isDefined) {
                var allStates = Set.empty[StateTable]
                for (containingStateName <- tx.availableIn.get) {
                    // Look up each state name and make sure it's a real state.
                    val stateTableOpt = lexicallyInsideOf.contractTable.state(containingStateName._1)
                    stateTableOpt match {
                        case None => logError(tx, StateUndefinedError(lexicallyInsideOf.contract.name, containingStateName._1))
                        case Some(stateTable) =>
                            allStates = allStates + stateTable
                    }
                }
                allStates
            }
        else {
            // All states are possible.
            val stateSet: Set[(String, StateTable)] = lexicallyInsideOf.contractTable.stateLookup.toSet
            stateSet.map(s => s._2)
        }

        val stateNames = if (tx.availableIn.isDefined) {
            Some(startStates.map(t => t.name))
        }
        else {
            None
        }
        val simpleType = simpleOfWithStateNames(lexicallyInsideOf.contract.name, stateNames)
        val thisUnpermissionedType = NoPathType(simpleType)

        // TODO: consider path case. Previously it was something like:
        // PathType("this"::"parent"::Nil, lexicallyInsideOf.simpleType)
        val table = simpleType match {
            case StateType(_, stateName) => lexicallyInsideOf.contractTable.state(stateName).get
            case _ => lexicallyInsideOf
        }
        val thisType = NonPrimitiveType(table, thisUnpermissionedType, Set(IsOwned()))

        // Construct the context that the body should start with
        var initContext = Context(new TreeMap[String, ObsidianType](), isThrown = false, Set.empty)
        initContext = initContext.updated("this", thisType)

        // first create this context so we can resolve types
        var context = new TreeMap[String, ObsidianType]()

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- tx.args) {
            initContext = initContext.updated(arg.varName, arg.typ)
        }

        checkTransactionInState(tx, lexicallyInsideOf, initContext)
    }

    private def checkFunc(func: Func, lexicallyInsideOf: Contract): Unit = {
        None // todo
    }

    private def checkState(lexicallyInsideOf: ContractTable, state: State): Unit = {
        val table = lexicallyInsideOf.state(state.name).get
        for (decl <- state.declarations) {
            decl match {
                case t: Transaction => checkTransaction(t, table) // Unsupported for now but leaving this here just in case.
                case f: Field => checkField(f, table.contractTable)
                case _ => () // TODO
            }
        }
    }

    private def checkConstructor(constr: Constructor, table: ContractTable, hasStates: Boolean): Unit = {

        // maybe this error should be handled in the parser
        if(constr.name != table.name) {
            logError(constr, ConstructorNameError(table.name))
        }

        if (table.contract.isResource && !constr.isOwned) {
            logError(constr, ResourceContractConstructorError(table.name))
        }

        // first create this unchecked context so we can resolve types
        var context = new TreeMap[String, ObsidianType]()

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- constr.args) {
            context = context.updated(arg.varName, arg.typ)
        }

        val stateSet: Set[(String, StateTable)] = table.stateLookup.toSet
        var initContext = Context(new TreeMap[String, ObsidianType](), isThrown = false, Set.empty)

        //should it be owned?
        val thisType = NonPrimitiveType(table, NoPathType(JustContractType(table.name)), Set(IsOwned()))

        initContext = initContext.updated("this", thisType)

        for (arg <- constr.args) {
            initContext = initContext.updated(arg.varName, context(arg.varName))
        }

        val outputContext =
            checkStatementSequence(constr, initContext, constr.body)

        // Check that all the states the constructor can end in are valid, named states
        if (constr.endsInState.isDefined) {
            for (stateName <- constr.endsInState.get.map(_._1)) {
                val stateTableOpt = table.contractTable.state(stateName)
                stateTableOpt match {
                    case None => logError(constr, StateUndefinedError(table.contract.name, stateName))
                    case Some(_) => ()
                }
            }
        }

        val expectedThisType =
            NonPrimitiveType(table, NoPathType(simpleOf(table, table.name, constr.endsInState)), Set(IsOwned()))
        checkIsSubtype(constr, outputContext("this"), expectedThisType)

        checkForUnusedOwnershipErrors(constr, outputContext, Set("this"))
        checkForUnusedStateInitializers(outputContext)

        // if the contract contains states, its constructor must contain a state transition
        if (hasStates && !hasTransition(constr.body)) {
            logError(constr, NoStartStateError(constr.name))
        }

    }

    private def checkForMainContract(ast: Program) = {
        val c: Option[Contract] = ast.contracts.find((c: Contract) =>
            c.modifiers.contains(IsMain()))
        if (c == None) logError(ast, NoMainContractError())

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


    /* just returns the errors from the program */
    def checkProgram(): Seq[ErrorRecord] = {
        checkForMainContract(globalTable.ast)

        for (contract <- globalTable.ast.contracts) {
            checkContract(contract)
        }

        errors
    }
}

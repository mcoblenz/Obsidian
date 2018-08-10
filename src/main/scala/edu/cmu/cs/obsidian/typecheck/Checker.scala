package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.parser._

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.{HashSet, TreeMap, TreeSet}



/* We define a custom type to store a special flag for if a context in after a "throw".
 * In the formalism, we allow throw to result in any type: in the implementation, we don't know
 * immediately which type this needs to be in order for type checking to work
 * transitionFieldsInitialized is a set of (state, field, AST) triples that are guaranteed to have been initialized.
 * The AST is for error message generation.
 */

case class Context(table: DeclarationTable,
                   underlyingVariableMap: Map[String, ObsidianType],
                   isThrown: Boolean,
                   transitionFieldsInitialized: Set[(String, String, AST)],
                   thisFieldTypes: Map[String, ObsidianType]) {
    def keys: Iterable[String] = underlyingVariableMap.keys

    def updated(s: String, t: ObsidianType): Context =
        Context(contractTable, underlyingVariableMap.updated(s, t), isThrown, transitionFieldsInitialized, thisFieldTypes)
    def updatedWithInitialization(stateName: String, fieldName: String, ast: AST): Context =
        Context(contractTable, underlyingVariableMap, isThrown, transitionFieldsInitialized + ((stateName, fieldName, ast)), thisFieldTypes)

    def updatedAfterTransition(): Context =
        Context(contractTable, underlyingVariableMap, isThrown, Set.empty, Map.empty)

    def updatedThisFieldType(fieldName: String, newType: ObsidianType): Context =
        Context(contractTable, underlyingVariableMap, isThrown, transitionFieldsInitialized, thisFieldTypes.updated(fieldName, newType))

    def get(s: String): Option[ObsidianType] = underlyingVariableMap.get(s)

    def apply(s: String): ObsidianType = underlyingVariableMap(s)

    def fieldIsInitialized(stateName: String, fieldName: String): Boolean =
        transitionFieldsInitialized.find((e) => e._1 == stateName && e._2 == fieldName).isDefined

    def makeThrown: Context = this.copy(isThrown = true)

    def thisType: NonPrimitiveType = get("this").get.asInstanceOf[NonPrimitiveType]

    def contractTable: ContractTable = table.contractTable

    // Looks up fields, transactions, etc., checking to make sure they're available in all
    // possible current states of "this".
    private def doLookup[FoundType <: IsAvailableInStates](lookupFunction: (DeclarationTable => Option[FoundType]),
                                                           inType: ObsidianType): Option[FoundType] = {
        if (inType.isBottom) {
            return None
        }

        val foundOpt: Option[FoundType] = inType match {
            case np: NonPrimitiveType =>
                // Look up the type in the current scope, NOT with lookupFunction.
                val contractTableOpt = contractTable.lookupContract(np.contractName)
                if (contractTableOpt.isEmpty) {
                    return None
                }

                // Look inside the contract.
                val insideContractResult = contractTableOpt.flatMap(lookupFunction)


                val possibleCurrentStateNames: Iterable[String] = np match {
                    case ContractReferenceType(contractName, _, _) => contractTableOpt.get.stateLookup.values.map((s: StateTable) => s.name)
                    case StateType(contractName, stateNames, _) =>
                        stateNames
                    case InterfaceContractType(name, interfaceInnerType) =>
                        contractTableOpt.get.stateLookup.values.map((s: StateTable) => s.name)
                }

                // It's weird that the way we find the available state names depends on the current state; this is an artifact
                // of the fact that lookup for things defined in exactly one state is different from lookup for things defined in more than one state
                // (or the whole contract).
                val availableInStateNames: Iterable[String] = {
                    if (insideContractResult.isDefined) {
                        insideContractResult.get.availableIn match {
                            case None => // This identifier is available in all states of the contract.
                                contractTableOpt.get.stateLookup.map(_._1)
                            case Some(identifiers) => identifiers
                        }
                    }
                    else {
                        Set()
                    }
                }

                val isAvailable = possibleCurrentStateNames.toSet.subsetOf(availableInStateNames.toSet)
                if (isAvailable) {
                    insideContractResult
                }
                else {
                    None
                }
            case _ => None
        }

        foundOpt

    }



    def lookupTransactionInThis(transactionName: String): Option[Transaction] = {
        lookupTransactionInType(thisType)(transactionName)
    }

    def lookupDeclaredFieldTypeInThis(fieldName: String): Option[ObsidianType] = {
        lookupDeclaredFieldTypeInType(thisType)(fieldName)
    }

    def lookupCurrentFieldTypeInThis(fieldName: String): Option[ObsidianType] = {
        lookupCurrentFieldTypeInType(thisType)(fieldName)
    }

    def lookupCurrentFieldTypeInType(typ: ObsidianType)(fieldName: String): Option[ObsidianType] = {
        thisFieldTypes.get(fieldName) match {
            case Some(typ) => Some(typ)
            case None =>
                doLookup((declTable: DeclarationTable) => declTable.lookupField(fieldName), typ) match {
                case None => None
                case Some(field) => Some(field.typ)
            }
        }
    }

    def lookupDeclaredFieldTypeInType(typ: ObsidianType)(fieldName: String): Option[ObsidianType] = {
        doLookup((declTable: DeclarationTable) => declTable.lookupField(fieldName), typ) match {
            case None => None
            case Some(field) => Some(field.typ)
        }
    }

    def lookupTransactionInType(typ: ObsidianType) (transactionName: String): Option[Transaction] = {
        if (typ.isBottom) {
            return None
        }

        typ match {
            case np: NonPrimitiveType =>
                // Look up the type in the current scope, NOT with lookupFunction.
                val contractTableOpt = contractTable.lookupContract(np.contractName)
                contractTableOpt match {
                    case None => None
                    case Some(contractTable) => contractTable.lookupTransaction(transactionName)
                }
            case _ => None
        }
    }

    def isPacked: Boolean = {
        // TODO
        assert(false, "TODO")
        true
    }
}

class Checker(globalTable: SymbolTable, verbose: Boolean = false) {
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

    //--------------------------------------------------------------------------
    // Splitting

    private def canSplit(t1: ObsidianType, t2: ObsidianType, t3: ObsidianType, contextContractTable: ContractTable): Boolean = {
        t1 match {
            case np1: NonPrimitiveType =>
                t2 match {
                    case np2: NonPrimitiveType =>
                        t3 match {
                            case np3: NonPrimitiveType =>
                                (np3.permission == Unowned()) ||
                                  (np1.permission == Shared() && np2.permission == Shared() && np3.permission == Shared()) ||
                                  (np1.permission == Owned() && np2.permission == Shared() && np3.permission == Shared() && np1.isResourceReference(contextContractTable) == No())
                            case _ => false // can't split non-reference types
                        }
                    case _ => false // can't split non-reference types
                }
            case _ => false // can't split non-reference types
        }
    }

    //-------------------------------------------------------------------------
    /* Subtyping definitions */

    /* true iff [t1 <: t2] */
    private def isSubtype(t1: ObsidianType, t2: ObsidianType): Option[Error] = {
        (t1, t2) match {
            case (BottomType(), _) => None
            case (_, BottomType()) => None
            case (IntType(), IntType()) => None
            case (BoolType(), BoolType()) => None
            case (StringType(), StringType()) => None
            case (np1: NonPrimitiveType, np2: NonPrimitiveType) =>
                val mainSubtype: Boolean = (np1, np2) match {
                    case (ContractReferenceType(c1, c1p, _), ContractReferenceType(c2, c2p, _)) =>
                        c1 == c2 && isSubpermission(c1p, c2p)
                    case (StateType(c1, ss1, _), StateType(c2, ss2, _)) =>
                        c1 == c2 && ss1.subsetOf(ss2)
                    case (StateType(c, ss1, _), ContractReferenceType(c2, c2p, _)) =>
                        c2 == ContractType(c)
                    case _ => false
                }
                if (!mainSubtype) Some(SubtypingError(t1, t2))
                // TODO: make sure this is unnecessary: else if (!modifierSubtype) Some(OwnershipSubtypingError(t1, t2))
                else None
            case _ => Some(SubtypingError(t1, t2))
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


    // returns true iff [p1 <: p2]
    private def isSubpermission(p1: Permission, p2: Permission): Boolean = {
        p1 match {
            case Owned() => true
            case Unowned() => p2 == Unowned()
            case Shared() => (p2 == Shared()) || (p2 == Unowned())
        }
    }
    //-------------------------------------------------------------------------
    private def nonPrimitiveMergeTypes(
            t1: NonPrimitiveType,
            t2: NonPrimitiveType): Option[NonPrimitiveType] = {
        if (t1.contractName != t2.contractName) return None
        (t1, t2) match {
            case (ContractReferenceType(_, p1, _), ContractReferenceType(_, p2, _)) =>
                if (p1 == p2) {
                    Some(t1)
                }
                else {
                    p1 match {
                        case Owned() => None
                        case Unowned() => if (p2 == Shared()) Some(t2) else None
                        case Shared() => if (p2 == Unowned()) Some(t1) else None
                        case Inferred() => assert(false, "Inferred types should be removed"); None
                    }
                }
            case (ContractReferenceType(_, Owned(), _), StateType(_, _, _)) =>
                Some(t1)
            case (StateType(_, _, _), ContractReferenceType(_, Owned(), _)) =>
                Some(t2)
            case (StateType(_, ss1, _), StateType(_, ss2, _)) =>
                val c = t1.contractName
                val unionStates = ss1.union(ss2)
                Some(StateType(c, unionStates, false))
            case _ => None
        }
    }

    private def mergeTypes(t1: ObsidianType, t2: ObsidianType): Option[ObsidianType] = {
        (t1, t2) match {
            case (IntType(), IntType()) => Some(IntType())
            case (BoolType(), BoolType()) => Some(BoolType())
            case (StringType(), StringType()) => Some(StringType())
            case (np1: NonPrimitiveType, np2: NonPrimitiveType) =>
                nonPrimitiveMergeTypes(np1, np2).flatMap(s => Some(s))
            case _ => None
        }
    }

    //-------------------------------------------------------------------------
    // Checking definitions for language constructs begins here

    private def inferAndCheckExpr(decl: InvokableDeclaration,
                                  context: Context,
                                  e: Expression,
                                  consumeOwnershipIfOwned: Boolean): (ObsidianType, Context) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: ObsidianType, c: Context): (ObsidianType, Context) = {
            val (tPrime, contextPrime) = inferAndCheckExpr(decl, c, e, false)
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
                name: String,
                receiver: Expression,
                args: Seq[Expression]): (ObsidianType, Context) = {
            val (receiverType, contextAfterReceiver) = inferAndCheckExpr(decl, context, receiver, false)

            // Eliminate things we can't invoke methods on first.
            val nonPrimitiveReceiverType = receiverType match {
                case BottomType() => return (BottomType(), contextAfterReceiver)
                case UnitType() | IntType() | BoolType() | StringType() =>
                    logError(e, NonInvokeableError(receiverType))
                    return (BottomType(), contextAfterReceiver)
                case u: UnresolvedNonprimitiveType => assert(false, "Should have resolved unresolved types already"); return (BottomType(), contextAfterReceiver)
                case np: NonPrimitiveType => np
            }


            val foundTransaction = contextAfterReceiver.lookupTransactionInType(receiverType)(name)

            val invokable: InvokableDeclaration = foundTransaction match {
                case None =>
                    val err = MethodUndefinedError(nonPrimitiveReceiverType, name)
                    logError(e, err)
                    return (BottomType(), contextAfterReceiver)
                case Some(t) => t
            }

            if (receiverType.isInstanceOf[InterfaceContractType] && !foundTransaction.get.isStatic) {
                val err = NonStaticAccessError(foundTransaction.get.name, receiver.toString)
                logError(e, err)
                return (BottomType(), contextAfterReceiver)
            }

            if (!invokable.isStatic && isSubtype(receiverType, invokable.thisType).isDefined) {
                logError(e, ReceiverTypeIncompatibleError(name, receiverType, invokable.thisType))
            }

            // Check field types for private invocations
            for ((fieldName, requiredInitialFieldType) <- foundTransaction.get.initialFieldTypes) {
                val currentFieldType = context.lookupCurrentFieldTypeInThis(fieldName)
                currentFieldType match {
                   case None => ()
                   case Some(cft) => if(isSubtype(cft, requiredInitialFieldType).isDefined) {
                       logError(e, FieldSubtypingError(fieldName, cft, requiredInitialFieldType))
                   }
                }
            }

            // check arguments
            val spec = invokable.args
            val specList = (spec, invokable)::Nil

            val (contextAfterArgs, correctInvokable) =
                checkArgs(e, contextAfterReceiver, specList, args) match {
                    case None => return (BottomType(), contextAfterReceiver)
                    case Some(x) => x
                }

            val resultType = correctInvokable.retType match {
                case None => UnitType()
                case Some(typ) => typ
            }

            // XXX TODO: check to make sure type of "this" is tracked correctly!
            val contextPrime =
                correctInvokable match {
                    case t: Transaction =>
                        updateReceiverTypeInContext(receiver, receiverType, t, contextAfterArgs)
                    case _ => contextAfterArgs
                }

            // Update field types if we invoked a private method.
            val contextAfterPrivateInvocation = updateFieldsForPrivateInvocation(contextPrime, foundTransaction.get)

            (resultType, contextAfterPrivateInvocation)

        }

         e match {
             case ReferenceIdentifier(x) =>
                 (context get x, context.lookupCurrentFieldTypeInThis(x)) match {
                     case (Some(t), _) =>
                         // We always want x to have the type according to the context, but sometimes we're going to consume ownership.
                         if (consumeOwnershipIfOwned) {
                             (t, context.updated(x, t.residualType))
                         }
                         else {
                             (t, context)
                         }
                     case (_, Some(t)) =>
                         if (consumeOwnershipIfOwned) {
                             (t, context.updatedThisFieldType(x, t.residualType))
                         }
                         else {
                             (t, context)
                         }
                     case (None, None) =>
                         val tableLookup = context.contractTable.lookupContract(x)
                         if (!tableLookup.isEmpty) {
                             val contractTable = tableLookup.get
                             val nonPrimitiveType = ContractReferenceType(contractTable.contractType, Shared(), false)
                             (InterfaceContractType(contractTable.name, nonPrimitiveType), context)
                         }
                         else {
                             logError(e, VariableUndefinedError(x, context.thisType.toString))
                             (BottomType(), context)
                         }
                 }
             case NumLiteral(_) => (IntType(), context)
             case StringLiteral(_) => (StringType(), context)
             case TrueLiteral() => (BoolType(), context)
             case FalseLiteral() => (BoolType(), context)
             case This() =>
                 val thisType = context.thisType
                 val newContext = if (consumeOwnershipIfOwned) context.updated("this", thisType.residualType) else context
                 (thisType, newContext)
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
                         ts
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
             case Negate(e: Expression) =>
                 assertTypeEquality(e, IntType(), context)
             case Equals(e1: Expression, e2: Expression) =>
                 val (t1, c1) = inferAndCheckExpr(decl, context, e1, false)
                 val (t2, c2) = inferAndCheckExpr(decl, c1, e2, false)
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
                 val (t1, c1) = inferAndCheckExpr(decl, context, e1, false)
                 val (t2, c2) = inferAndCheckExpr(decl, c1, e2, false)
                 if (t1 == t2) (BoolType(), c2) else {
                     logError(e, DifferentTypeError(e1, t1, e2, t2))
                     (BottomType(), c2)
                 }

             case Dereference(eDeref: Expression, fieldName) =>
                 val (derefType, contextPrime) = inferAndCheckExpr(decl, context, eDeref, true)
                 if (derefType.isBottom) {
                     return (BottomType(), contextPrime)
                 }

                 val fieldType =  context.lookupDeclaredFieldTypeInType(derefType)(fieldName) match {
                     case Some(typ) => typ
                     case None =>
                         derefType match {
                             case np: NonPrimitiveType =>
                                 logError(e, FieldUndefinedError(np, fieldName))
                                 return (BottomType(), contextPrime)
                             case _ => logError(e, DereferenceError(derefType))
                                 return (BottomType(), contextPrime)
                         }
                 }

                 (fieldType, contextPrime)

             case LocalInvocation(name, args: Seq[Expression]) =>
                 handleInvocation(context, name, This(), args)

             case Invocation(receiver: Expression, name, args: Seq[Expression]) =>
                 handleInvocation(context, name, receiver, args)

             case Construction(name, args: Seq[Expression]) =>
                 val tableLookup = context.contractTable.lookupContract(name)
                 if (tableLookup.isEmpty) {
                     logError(e, ContractUndefinedError(name))
                     return (BottomType(), context)
                 }

                 val ctTableOfConstructed = tableLookup.get

                 val constrSpecs = ctTableOfConstructed
                                    .constructors
                                    .map(constr => (constr.args, constr))

                 val result = checkArgs(e, context, constrSpecs, args)

                 val (simpleType, contextPrime) = result match {
                     // Even if the args didn't check, we can still output a type
                     case None => (ContractReferenceType(ctTableOfConstructed.contractType, Owned(), false), context)
                     case Some((cntxt, constr)) => (constr.asInstanceOf[Constructor].resultType, cntxt)
                 }

                 (simpleType, contextPrime)
             case Disown(e) =>
                 // The expression "disown e" evaluates to an unowned value but also side-effects the context
                 // so that e is no longer owned (if it is a variable).
                 val (typ, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = true)
                 if (!typ.isOwned) {
                    logError(e, DisownUnowningExpressionError(e))
                 }

                 val newTyp = typ match {
                     case n: NonPrimitiveType => n.residualType
                     case t => t
                 }

                 // If e is a variable, then we need to update the context to indicate that it's no longer owned.
                 val finalContext = e match {
                     case ReferenceIdentifier(x) => contextPrime.updated(x, newTyp)
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
                             case None => logError(e, FieldUndefinedError(stateTable.nonPrimitiveType, fieldName._1)); BottomType()
                             case Some(field) => field.typ
                         }
                 }

                 (fieldType, context)
         }
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
                case Return() | ReturnExpr(_) | Throw() => hasRet = true
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
                errorIfNotDisposable(x, typ, context, ast)
            }
        }
    }

    private def checkForUnusedStateInitializers(context: Context) = {
        for (remainingInitialization <- context.transitionFieldsInitialized) {
            logError(remainingInitialization._3, InvalidStateFieldInitialization(remainingInitialization._1, remainingInitialization._2))
        }
    }

    private def checkFieldTypeConsistency(context: Context, tx: Transaction): Unit = {
        // First check fields that may be of inconsistent type with their declarations to make sure they match
        // either the declarations or the specified final types.
        for ((field, typ) <- context.thisFieldTypes) {
            val requiredFieldType =
                if (tx.finalFieldTypes.contains(field)) {
                    if (context.lookupDeclaredFieldTypeInThis(field).isEmpty) {
                        // There is a final field type declaration for a nonexistent field.
                        logError(tx, InvalidFinalFieldTypeDeclarationError(field))
                        Some(BottomType())
                    }
                    else {
                        tx.finalFieldTypes.get(field)
                    }
                }
                else {
                    context.lookupDeclaredFieldTypeInThis(field)
                }

            requiredFieldType match {
                case None =>
                    assert(false, "Bug: invalid field in field type context")
                case Some(declaredFieldType) =>
                    if (isSubtype(typ, declaredFieldType).isDefined) {
                        logError(tx, InvalidInconsistentFieldType(field, typ, declaredFieldType))
                    }
            }
        }

        // Next check the specified final field types to make sure any that are NOT overridden in thisFieldTypes are satisfied.

        for ((field, requiredTyp) <- tx.finalFieldTypes) {
            if (!context.thisFieldTypes.contains(field)) { // otherwise it was already checked above
                val currentType = context.lookupDeclaredFieldTypeInThis(field)
                currentType match {
                    case None =>
                        logError(tx, InvalidFinalFieldTypeDeclarationError(field))
                    case Some(currentTyp) =>
                        if (isSubtype(currentTyp, requiredTyp).isDefined) {
                            logError(tx, InvalidInconsistentFieldType(field, currentTyp, requiredTyp))
                        }
                }
            }
        }
    }

    /* returns a context that is the same as [branchContext], except only with
     * those variables bound which [oldContext] actually assign a value to */
    private def pruneContext(ast: AST, branchContext: Context, oldContext: Context): Context = {
        assert(branchContext.contractTable == oldContext.contractTable)
        var newContext = oldContext

        for (x <- oldContext.keys) {
            val t = branchContext.get(x) match {
                case Some(tBranch) => tBranch
                case None => oldContext(x)
            }
            newContext = newContext.updated(x, t)
        }

        checkForUnusedOwnershipErrors(ast, branchContext, oldContext.keys.toSet)

        Context(oldContext.contractTable, newContext.underlyingVariableMap, isThrown = branchContext.isThrown, newContext.transitionFieldsInitialized, branchContext.thisFieldTypes)
    }

    private def errorIfNotDisposable(variable: String, typ: ObsidianType, context: Context, ast: AST): Unit = {
        typ match {
            case t: NonPrimitiveType =>
                if (t.isOwned && t.isResourceReference(context.contractTable) != No()) logError(ast, UnusedOwnershipError(variable))
            case _ => ()
        }
    }

    private def mergeContext(
            ast: AST,
            context1: Context,
            context2: Context): Context = {
        /* If we're merging with a context from a "throw", just take the other context
        * emit no errors */
        assert(context1.contractTable == context2.contractTable)

        if (context1.isThrown && !context2.isThrown) return context2
        if (!context1.isThrown && context2.isThrown) return context1

        def mergeMaps(map1: Map[String, ObsidianType], map2: Map[String, ObsidianType]): Map[String, ObsidianType] = {
            var mergedMap = new TreeMap[String, ObsidianType]()

            val inBoth = map1.keys.toSet.intersect(map2.keys.toSet)

            for (x <- inBoth) {
                val t1 = map1(x)
                val t2 = map2(x)
                mergeTypes(t1, t2) match {
                    case Some(u) => mergedMap = mergedMap.updated(x, u)
                    case None =>
                        logError(ast, MergeIncompatibleError(x, t1, t2))
                }
            }

            // Make sure anything that is not in both is disposable.

            val inOnlyContext1 = map1.keys.toSet -- inBoth
            val inOnlyContext2 = map2.keys.toSet -- inBoth

            inOnlyContext1.foreach((x: String) => errorIfNotDisposable(x, map1(x), context1, ast))
            inOnlyContext2.foreach((x: String) => errorIfNotDisposable(x, map2(x), context2, ast))

            mergedMap
        }

        val mergedVariableMap = mergeMaps(context1.underlyingVariableMap, context2.underlyingVariableMap)
        val mergedThisFieldMap = mergeMaps(context1.thisFieldTypes, context2.thisFieldTypes)

        Context(context1.contractTable, mergedVariableMap, context1.isThrown, context1.transitionFieldsInitialized.intersect(context2.transitionFieldsInitialized), mergedThisFieldMap)
    }

    /* if [e] is of the form ReferenceIdentifier(x), This(), or if [e] is a sequence of
     * dereferences on ReferenceIdentifier(x) or This(), [extractPath] extracts the list
     * of identifiers on the path. If [e] isn't this form, returns None */
    private def extractPath(e: Expression): Option[Seq[String]] = {
        e match {
            case ReferenceIdentifier(x) => Some(x::Nil)
            case This() => Some("this"::Nil)
            case Parent() => Some("this"::"parent"::Nil)
            case Dereference(ePrime, f) => extractPath(ePrime).map(_ ++ (f::Nil))
            case _ => None
        }
    }

    // Returns [Left(errs)] if [spec] and [args] don't match, and returns [Right(context)] if they do.
    // Typechecks the arguments, and transfers ownership unless the spec is Unowned
    private def checkArgsWithSpec(
            ast: AST,
            decl: InvokableDeclaration,
            context: Context,
            spec: Seq[VariableDeclWithSpec],
            args: Seq[Expression]): Either[Seq[(AST, Error)], Context] = {

        var errList: List[(AST, Error)] = Nil
        val (specL, argsL) = (spec.length, args.length)

        if (specL != argsL) {
            val name = if (decl.isInstanceOf[Constructor]) s"constructor of ${decl.name}" else decl.name
            Left((ast, WrongArityError(specL, argsL, name))::errList)
        } else {

            val specTypes = spec.map(_.typIn)

            var contextAfterArgs = context
            for (i <- args.indices) {
                val arg = args(i)
                val specType = specTypes(i)

                val transferOwnership: Boolean =
                    specType match {
                        case np: NonPrimitiveType => np.permission != Unowned()
                        case _ => true
                    }

                val (argType, contextAfterArg) = inferAndCheckExpr(decl, contextAfterArgs, arg, transferOwnership)


                if (isSubtype(argType, specType).isDefined) {
                    val err = ArgumentSubtypingError(decl.name, spec(i).varName, argType, specType)
                    errList = (ast, err)::errList
                }

                contextAfterArgs = contextAfterArg
            }

            if (errList.isEmpty) Right(contextAfterArgs)
            else Left(errList)
        }
    }

    // checks the arguments based on possibly many specifications, such as multiple constructors
    // logs all errors at the end
    private def checkArgs(
            ast: AST,
            context: Context,
            specs: Seq[(Seq[VariableDeclWithSpec], InvokableDeclaration)],
            args: Seq[Expression]): Option[(Context, InvokableDeclaration)] = {

        var errs: List[(AST, Error)] = Nil
        for ((spec, invokable) <- specs) {
            checkArgsWithSpec(ast, invokable, context, spec, args) match {
                case Right(context) => return Some((context, invokable))
                case Left(newErrs) =>
                    errs = newErrs.toList ++ errs
            }
        }
        errs.foreach((err: (AST, Error)) => logError(err._1, err._2))
        None
    }

    private def updateFieldsForPrivateInvocation(context: Context, tx: Transaction): Context = {
        var finalContext = context
        for ((fieldName, finalFieldType) <- tx.finalFieldTypes) {
            finalContext = context.updatedThisFieldType(fieldName, finalFieldType)
        }

        finalContext
    }

    // updates the type of an identifier in the context based on the transaction invoked on it
    private def updateReceiverTypeInContext(receiver: Expression,
                                            receiverType: ObsidianType,
                                            invokable: Transaction,
                                            context: Context): Context = {

        val contextPrime = receiver match {
            case ReferenceIdentifier(x) => {
                receiverType match {
                    case typ: NonPrimitiveType => {
                        val newType = invokable.thisFinalType
                        if (newType.permission == Unowned()) {
                            // The transaction promised not to change the state of the receiver.
                            context
                        }
                        else {
                            if (context.get(x).isDefined) {
                                // This was a local variable.
                                context.updated(x, newType)
                            }
                            else {
                                // This was a field or a static invocation. If it was a field, update the field's type.
                                if (context.lookupCurrentFieldTypeInThis(x).isDefined) {
                                    context.updatedThisFieldType(x, newType)
                                }
                                else {
                                    // No need to update anything for static invocations.
                                    context
                                }
                            }
                        }
                    }
                    case _ => context
                }
            }
            case _ => context
        }
        contextPrime
    }

    private def checkStatement(
                                  decl: InvokableDeclaration,
                                  context: Context,
                                  s: Statement
                              ): Context = {

        def checkAssignment(x: String, e: Expression, context: Context, mustBeField: Boolean): Context = {
            val (exprType, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = true)
            val localVariableType =
                if (mustBeField) {
                    // Don't look for a matching local variable if this is required to be a field.
                    None
                }
                else {
                    contextPrime.get(s"$x")
                }

            val (variableType, isField) =
                localVariableType match {
                case None =>
                    // Not a local variable. Maybe a field?
                    val fieldType = contextPrime.lookupCurrentFieldTypeInThis(x)

                    /* if it's not a field either, log an error */
                    fieldType match {
                        case None =>
                            if (mustBeField) {
                                logError(s, FieldUndefinedError(contextPrime.thisType, x))
                            }
                            else {
                                logError(s, VariableUndefinedError(x, contextPrime.thisType.toString))
                            }
                            (BottomType(), false)
                        case Some(typ) => (typ, true)
                    }
                case Some(typ) =>
                    // Local variable.
                    (typ, false)
                }

            (exprType, variableType) match {
                case (exprNPType: NonPrimitiveType, variableNPType: NonPrimitiveType) =>
                    if (exprNPType.contractName != variableNPType.contractName) {
                        logError(s, InconsistentContractTypeError(variableNPType.contractName, exprNPType.contractName))
                        contextPrime
                    }
                    else if (isField) {
                        contextPrime.updatedThisFieldType(x, exprType)
                    }
                    else {
                        contextPrime.updated(x, exprType)
                    }
                case (_, _) =>
                    checkIsSubtype(s, exprType, variableType)
                    contextPrime
            }
        }


        s match {
            case VariableDecl(typ: ObsidianType, name) =>
                context.updated(name, typ)

            case VariableDeclWithInit(typ: ObsidianType, name, e: Expression) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = true)
                val tDecl = typ match {
                    case np: NonPrimitiveType =>
                        val contractName = np.contractName

                        val tableLookup = context.contractTable.lookupContract(contractName)

                        tableLookup match {
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
                    case _ =>
                        logError(s, MustReturnError(decl.name))
                        context.makeThrown
                }

            case ReturnExpr(e: Expression) =>
                val retTypeOpt = decl match {
                    /* must be no return type */
                    case tx: Transaction if tx.retType.isDefined => tx.retType
                    case _ =>
                        logError(s, CannotReturnError(decl.name))
                        return context.makeThrown
                }

                val consumeOwnership = retTypeOpt match {
                    case None => false
                    case Some(retType) => retType.isOwned
                }

                val (typ, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnership)

                val variablesToExcludeFromOwnershipCheck = e match {
                    case ReferenceIdentifier(xOther)
                        if retTypeOpt.isDefined && retTypeOpt.get.isOwned => Set(xOther, "this")
                    case _ => Set("this")
                }

                checkForUnusedOwnershipErrors(s, contextPrime, variablesToExcludeFromOwnershipCheck)

                if (retTypeOpt.isDefined && !retTypeOpt.get.isBottom) checkIsSubtype(s, typ, retTypeOpt.get)
                contextPrime.makeThrown

            case Transition(newStateName, updates: Option[Seq[(ReferenceIdentifier, Expression)]]) =>
                val thisTable = context.contractTable

                if (thisTable.state(newStateName).isEmpty) {
                    logError(s, StateUndefinedError(thisTable.name, newStateName))
                    return context
                }

                val newStateTable = thisTable.state(newStateName).get

                val oldType = context.thisType

                if (oldType.permission == Unowned()) {
                    logError(s, TransitionNotAllowedError())
                }

                // First we focus on the fields declared in states individually.
                // oldFields is the set of fields declared in the old state, which are definitely going away.
                // maybeOldFields is the set of fields from the old state that MAY be going away — 
                //   we can't be sure when the current state is a union.

                val possibleCurrentStates = oldType match {
                    case ContractReferenceType(_, _, _) => thisTable.possibleStates
                    case InterfaceContractType(_, _) => thisTable.possibleStates
                    case StateType(_, stateNames, _) => stateNames
                }

                // For each state that we might be in, compute the set of fields that could be available.
                val allContractFields = thisTable.allFields
                val oldFieldSets: Set[Set[Field]] = possibleCurrentStates.map((stateName: String) =>
                    // Take this state name and find all fields that are applicable.
                    allContractFields.filter((f: Field) =>
                        if (f.availableIn.isEmpty) {
                            // available in all states
                            true
                        }
                        else {
                            f.availableIn.get.contains(stateName)
                        }
                ))

                val (oldFields: Set[(String, ObsidianType)], maybeOldFields: Set[(String, ObsidianType)]) =
                // We don't have a statically-fixed set of old fields because we don't know statically
                // which specific state we're in. We take a conservative approach:
                // take the intersection to ensure that all fields might need to be initialized will be initialized.
                    if (decl.isInstanceOf[Constructor] || oldFieldSets.isEmpty) {
                        (Set.empty[(String, ObsidianType)], Set.empty[(String, ObsidianType)])
                    }
                    else {
                        val oldFieldNamesSets: Set[Set[(String, ObsidianType)]] = oldFieldSets.map(
                            (decls: Set[Field]) => decls.map((d: Field) => (d.name, d.typ))
                        )

                        (oldFieldNamesSets.tail.foldLeft(oldFieldNamesSets.head) {
                            ((intersections, next) => intersections.intersect(next))
                        },
                            oldFieldNamesSets.tail.foldLeft(oldFieldNamesSets.head) {
                                ((unions, next) => unions.union(next))
                            })

                    }

                val newStateFields = newStateTable.ast.fields
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
                                    stateIdentifiers.contains(newStateName)
                            }
                        }
                        else {
                            false
                        }
                    )
                val fieldNamesInNewState = contractFieldDeclarationsAvailableInNewState.map(
                    (decl: Declaration) => (decl.asInstanceOf[Field].name, decl.asInstanceOf[Field].typ))
                val newFields: Seq[(String, ObsidianType)] = newStateFields ++ fieldNamesInNewState

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
                    val err = FieldUndefinedError(newStateTable.nonPrimitiveType, s)
                    logError(updates.get.find(_._1.name == s).get._1, err)
                }

                var contextPrime = context
                if (updates.isDefined) {
                    for ((ReferenceIdentifier(f), e) <- updates.get) {
                        val fieldAST = newStateTable.lookupField(f)
                        if (fieldAST.isDefined) {
                            val (t, contextPrime2) = inferAndCheckExpr(decl, contextPrime, e, true)
                            contextPrime = contextPrime2
                            checkIsSubtype(s, t, fieldAST.get.typ)
                        }
                    }
                }

                // Check for potentially-dropped resources.
                val toCheckForDroppedResources = maybeOldFields.diff(newFields.toSet) // fields that we might currently have minus fields we're initializing now
                for (oldField <- toCheckForDroppedResources) {
                    val fieldType = contextPrime.thisFieldTypes.getOrElse(oldField._1, oldField._2)
                    if (fieldType.isResourceReference(thisTable) != No() && fieldType.isOwned) {
                        logError(s, PotentiallyUnusedOwnershipError(oldField._1))
                    }
                }

                val newTypeTable = thisTable.contractTable.state(newStateName).get
                val newSimpleType = StateType(thisTable.name, newStateName, false)

                contextPrime.updated("this", newSimpleType).updatedAfterTransition()

            case Assignment(ReferenceIdentifier(x), e: Expression) =>
                checkAssignment(x, e, context, false)

            case Assignment(Dereference(eDeref, f), e: Expression) =>
                if (eDeref != This()) {
                    logError(s, InvalidNonThisFieldAssignment())
                    context
                }
                else {
                    checkAssignment(f, e, context, true)
                }
            case Assignment(StateInitializer(stateName, fieldName), e) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = true)

                val stateOption = context.contractTable.state(stateName._1)
                val fieldType = stateOption match {
                    case None => logError(s, StateUndefinedError(context.contractTable.name, stateName._1)); BottomType()
                    case Some(stateTable) =>
                        stateTable.lookupField(fieldName._1) match {
                            case None => logError(s, FieldUndefinedError(stateTable.nonPrimitiveType, fieldName._1)); BottomType()
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
            case Assignment(_, e: Expression) =>
                val (_, contextPrime) = inferAndCheckExpr(decl, context, e, consumeOwnershipIfOwned = true)
                logError(s, AssignmentError())
                contextPrime

            case Throw() =>
                // If exceptions are ever catchable, we will need to make sure the fields of this have types consistent with their declarations.
                // For now, we treat this like a permanent abort.
                Context(context.contractTable, context.underlyingVariableMap, isThrown = true, Set.empty, Map.empty)

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, eCond, false)
                checkIsSubtype(s, t, BoolType())
                val contextIfTrue = pruneContext(s,
                    checkStatementSequence(decl, contextPrime, body),
                    contextPrime)
                mergeContext(s, contextPrime, contextIfTrue)

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime) = inferAndCheckExpr(decl, context, eCond, false)
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
                val (t, contextPrime) = inferAndCheckExpr(decl, context, e, false)

                val contractName = t match {
                    case np: NonPrimitiveType =>
                        np.contractName
                    case _ =>
                        logError(e, SwitchError(t))
                        return contextPrime
                }

                val contractTable = context.contractTable.lookupContract(contractName) match {
                    case Some(table) => table
                    case None => logError(e, SwitchError(t))
                        return contextPrime
                }

                def contextForSwitchCase(sc: SwitchCase) = {
                    val newType: ObsidianType =
                        contractTable.state(sc.stateName) match {
                            case Some(stTable) =>
                                StateType(contractTable.name, stTable.name, false)
                            case None =>
                                logError(s, StateUndefinedError(contractTable.name, sc.stateName))
                                ContractReferenceType(contractTable.contractType, Owned(), false)
                        }

                    /* special case to allow types to change in the context if we match on a variable */
                    val startContext = e match {
                        case This() =>
                            /* reading "this" as an expression takes the residual of "this",
                             * so we want "this" in the context to have the old permission of
                             * "this" with the new state information in the unpermissioned type */
                            val newContextThisType =
                                newType // is this right?
                            contextPrime.updated("this", newContextThisType)
                        case ReferenceIdentifier(x) => contextPrime.updated(x, newType)
                        case _ => contextPrime
                    }

                    pruneContext(s, checkStatementSequence(decl, startContext, sc.body), startContext)
                }

                val mergedContext: Context = cases.headOption match {
                    case None => contextPrime
                    case Some(switchCase) =>
                        val initialContext = contextForSwitchCase(switchCase)
                        val restCases = cases.tail
                        restCases.foldLeft(initialContext)((prevContext: Context, sc: SwitchCase) =>
                                mergeContext(s, prevContext, contextForSwitchCase(sc)))
                }

                mergedContext

            // TODO maybe allow constructors as statements later, but it's not very important
            case d@Disown (e) =>
                val (typ, contextPrime) = inferAndCheckExpr(decl, context, d, true)
                contextPrime
            case e: Expression =>
                val (typ, contextPrime) = inferAndCheckExpr(decl, context, e, true)
                if (typ.isOwned) {
                    logError(s, UnusedExpressionOwnershipError(e))
                }
                if (!(s.isInstanceOf[LocalInvocation] || s.isInstanceOf[Invocation])) {
                    logError(s, NoEffectsError(s))
                }
                contextPrime
            case StaticAssert(e, allowedStatesOrPermissions) =>
                val (typ, contextPrime) = inferAndCheckExpr(decl, context, e, true)

                def checkStateOrPermissionValid(contractName: String, stateOrPermission: Identifier): Unit = {
                    val stateOrPermissionStr = stateOrPermission._1
                    val resolvedPermission = Parser.resolvePermission(stateOrPermissionStr)
                    resolvedPermission match {
                        case Some(requiredPermission) => ()
                        case None =>
                            // Assume stateOrPermission is a state name.
                            // Make sure it's a valid name, although it wouldn't harm too much it if it were.
                            val contractType = context.contractTable.lookupContract(contractName).get
                            val referencedState = contractType.state(stateOrPermissionStr)
                            if (referencedState.isEmpty) {
                                logError(s, StaticAssertInvalidState(contractName, stateOrPermissionStr))
                            }
                    }
                }

                val typToCheck = typ match {
                    case InterfaceContractType(name, realTyp) => realTyp
                    case _ => typ
                }

                val allowedStatesOrPermissionNames = allowedStatesOrPermissions.map(_._1)
                typToCheck match {
                    case b: BottomType => ()
                    case p: PrimitiveType => logError(s, StaticAssertOnPrimitiveError(e))
                    case ContractReferenceType(contractType, permission, _) =>
                        // Make sure the permission is somewhere in the list of asserted permissions.
                        if (!allowedStatesOrPermissionNames.contains(permission.toString)) {
                            logError(s, StaticAssertFailed(e, allowedStatesOrPermissionNames, typ))
                        }
                        allowedStatesOrPermissions.toSet.foreach((stateName: Identifier) => checkStateOrPermissionValid(contractType.contractName, stateName))

                    case stateType@StateType(contractName, stateNames, _) =>
                        if (!stateNames.subsetOf(allowedStatesOrPermissionNames.toSet)) {
                            logError(s, StaticAssertFailed(e, allowedStatesOrPermissionNames, typ))
                        }
                        allowedStatesOrPermissions.toSet.foreach((stateName: Identifier) => checkStateOrPermissionValid(stateType.contractName, stateName))

                    case InterfaceContractType(name, _) => assert(false, "Should have already eliminated this case")
                    case u: UnresolvedNonprimitiveType =>
                        assert(false, "Should not encounter unresolved nonprimitive types in the typechecker")
                }

                context // Not contextPrime!
            case _ =>
                logError(s, NoEffectsError(s))
                context
        }
    }

    private def checkField(field: Field, lexicallyInsideOf: ContractTable, containingState: Option[State]): Unit = {
        val contractIsResource = lexicallyInsideOf.contract.modifiers.contains(IsResource())

        val containerIsResource = containingState match {
            case None =>
                // This field is lexically inside a contract, not in a state, but it might be available in a particular set of states.

                // If the field is in a contract that was declared as a resource, the settings on the individual states don't matter.
                if (contractIsResource) {
                    true
                }
                else if (field.availableIn.isDefined) {
                    val availableStateNames = field.availableIn.get
                    val availableStates = availableStateNames.map((stateName: String) => lexicallyInsideOf.state(stateName))
                    availableStates.foldLeft(true)((allResourcesSoFar: Boolean, state: Option[StateTable]) => allResourcesSoFar && state.isDefined && state.get.ast.isResource)
                }
                else {
                    contractIsResource
                }
            case Some(state) => contractIsResource || state.isResource
        }

        field.typ match {
            case typ: NonPrimitiveType =>
                if (typ.isOwned) {
                    // Only resources can own other resources (since otherwise they might go out of scope improperly).
                    if (typ.isResourceReference(lexicallyInsideOf) != No() && !containerIsResource) {
                        logError(field, NonResourceOwningResourceError(lexicallyInsideOf.name, field))
                    }
                }
            case _ => ()
        }
    }

    private def checkTransactionInState(tx: Transaction,
                                        lexicallyInsideOf: DeclarationTable,
                                        initContext: Context): Unit = {

        var context = initContext

        for (arg <- tx.args) {
            context = initContext.updated(arg.varName, arg.typIn)
        }

        // Check the body; ensure [this] is well-typed after, and check for leaked ownership
        val outputContext =
            checkStatementSequence(tx, initContext, tx.body)

        val expectedType = tx.thisFinalType
        // Check that all the states the transaction can end in are valid, named states
        expectedType match {
            case StateType(_, states, _) => {
                for (stateName <- states) {
                    val stateTableOpt = lexicallyInsideOf.contractTable.state(stateName)
                    stateTableOpt match {
                        case None => logError(tx, StateUndefinedError(lexicallyInsideOf.contract.name, stateName))
                        case Some(_) => ()
                    }
                }
            }
            case _ => ()
        }

        // TODO: make the permission depend on the transaction's specification
        checkIsSubtype(tx, outputContext("this"), expectedType)

        // Check that the arguments meet the correct specification afterwards
        tx.args.foreach(arg => {
            val actualTypOut = outputContext(arg.varName)

            val errorOpt = isSubtype(actualTypOut, arg.typOut)
            if (errorOpt.isDefined) {
                logError(tx, ArgumentSpecificationError(arg.varName, tx.name, arg.typOut, actualTypOut))
            }
        })

        checkForUnusedStateInitializers(outputContext)

        if (tx.retType.isDefined & !hasReturnStatement(tx, tx.body)) {
            logError(tx.body.last, MustReturnError(tx.name))
        }
        else if (!tx.retType.isDefined) {
            // We check for unused ownership errors at each return; if there isn't guaranteed to be one at the end, check separately.
            checkForUnusedOwnershipErrors(tx, outputContext, Set("this"))
        }

        // Check to make sure all the field types are consistent with their declarations.
        checkFieldTypeConsistency(outputContext, tx)

        // todo: check that every declared variable is initialized before use
    }

    private def checkTransaction(tx: Transaction, lexicallyInsideOf: DeclarationTable): Unit = {
        if (!tx.isPrivate && (!tx.initialFieldTypes.isEmpty || !tx.finalFieldTypes.isEmpty)) {
            logError(tx, FieldTypesDeclaredOnPublicTransactionError(tx.name))
        }

        // Construct the set of states that the transaction might start in.
        val startStates: Set[StateTable] =
            tx.thisType match {
                case StateType(_, stateNames, _) =>
                    var allStates = Set.empty[StateTable]
                    for (containingStateName <- stateNames) {
                        // Look up each state name and make sure it's a real state.
                        val stateTableOpt = lexicallyInsideOf.contractTable.state(containingStateName)
                        stateTableOpt match {
                            case None => logError(tx, StateUndefinedError(lexicallyInsideOf.contract.name, containingStateName))
                            case Some(stateTable) =>
                                allStates = allStates + stateTable
                        }
                    }
                    allStates
                case _ =>
                    // All states are possible.
                    val stateSet: Set[(String, StateTable)] = lexicallyInsideOf.contractTable.stateLookup.toSet
                    stateSet.map(s => s._2)
            }

        val thisType = tx.thisType
        // TODO: consider path case. Previously it was something like:
        // PathType("this"::"parent"::Nil, lexicallyInsideOf.simpleType)
        val table = thisType match {
            case StateType(_, stateNames, _) =>
                if (stateNames.size == 1) {
                    val stateName = stateNames.head
                    lexicallyInsideOf.contractTable.state(stateName).get
                }
                else {
                    lexicallyInsideOf
                }
            case _ => lexicallyInsideOf
        }

        // Construct the context that the body should start with
        var initContext = Context(table, new TreeMap[String, ObsidianType](), isThrown = false, Set.empty, tx.initialFieldTypes)
        initContext = initContext.updated("this", thisType)

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- tx.args) {
            initContext = initContext.updated(arg.varName, arg.typIn)
        }

        checkTransactionArgShadowing(startStates, tx)
        checkTransactionInState(tx, lexicallyInsideOf, initContext)
    }

    private def checkStateFieldShadowing(lexicallyInsideOf: ContractTable, f: Field, s: State): Unit = {
        //check if field also declared in contract
        val fieldInContract = lexicallyInsideOf.lookupField(f.name)
        fieldInContract match {
            case None => ()
            case Some(field) => logError(f, ShadowingError(f.name, s.name, field.loc.line))
        }

        //check if field is in another state too
        val allStateNames: Set[String] = lexicallyInsideOf.possibleStates
        var foundField: Boolean = false
        for (stateName <- allStateNames) {
            if (stateName != s.name) {
                lexicallyInsideOf.state(stateName) match {
                    case None => ()
                    case Some(state: StateTable) => {
                        for (decl <- state.ast.fields) {
                            decl match {
                                case field: Field => {
                                    if ((field.name == f.name) && (field.loc.line < f.loc.line) && (!foundField)) {
                                        logError(f, SharedFieldNameError(field.name, stateName, field.loc.line))
                                        foundField = true
                                    }
                                }
                                case _ => ()
                            }
                        }
                    }
                }
            }
        }
    }

    private def checkTransactionArgShadowing(states: Set[StateTable], t: Transaction): Unit = {
        for (arg <- t.args) {

            // the possible states the transaction could start in
            for (state <- states) {
                val fieldOpt = state.lookupField(arg.varName)
                if (fieldOpt.isDefined) {
                    logError(t, ArgShadowingError(fieldOpt.get.name, t.name, fieldOpt.get.loc.line))
                }
            }
        }
    }

    private def checkContractFieldRepeats(field: Field, contract: Contract): Unit = {
        for (decl <- contract.declarations) {
            decl match {
                case f: Field => {
                    if ((f.name == field.name) && (f.loc.line < field.loc.line)) {
                        (field.availableIn, f.availableIn) match {
                            case (None, _) => logError(field, RepeatContractFields(field.name, f.loc.line, f.loc.line))
                            case (Some(_), None) => logError(field, RepeatContractFields(field.name, field.loc.line, f.loc.line))
                            case (Some(states1), Some(states2)) => {
                                logError(field, CombineAvailableIns(field.name, (states2 | states1).mkString(", "), f.loc.line))
                            }
                        }
                    }
                }
                case _ => () //do nothing
            }
        }

    }

    private def checkState(lexicallyInsideOf: ContractTable, state: State): Unit = {
        val table = lexicallyInsideOf.state(state.name).get
        for (field <- state.fields) {
            checkField(field, table.contractTable, Some(state))
            checkStateFieldShadowing(lexicallyInsideOf, field, state)
        }
    }

    private def checkConstructor(constr: Constructor, table: ContractTable, hasStates: Boolean): Unit = {

        // maybe this error should be handled in the parser
        if(constr.name != table.name) {
            logError(constr, ConstructorNameError(table.name))
        }

        if (table.contract.isResource && !constr.resultType.isOwned) {
            logError(constr, ResourceContractConstructorError(table.name))
        }

        // first create this unchecked context so we can resolve types
        var context = new TreeMap[String, ObsidianType]()

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- constr.args) {
            context = context.updated(arg.varName, arg.typIn)
        }

        val stateSet: Set[(String, StateTable)] = table.stateLookup.toSet
        var initContext = Context(table, new TreeMap[String, ObsidianType](), isThrown = false, Set.empty, Map.empty)

        val thisType = ContractReferenceType(table.contractType, Owned(), false)

        initContext = initContext.updated("this", thisType)

        for (arg <- constr.args) {
            initContext = initContext.updated(arg.varName, context(arg.varName))
        }

        val outputContext =
            checkStatementSequence(constr, initContext, constr.body)

        // Check that all the states the constructor can end in are valid, named states
        constr.resultType match {
            case StateType(_, states, _) => {
                for (stateName <- states) {
                    val stateTableOpt = table.contractTable.state(stateName)
                    stateTableOpt match {
                        case None => logError(constr, StateUndefinedError(table.contract.name, stateName))
                        case Some(_) => ()
                    }
                }
            }
            case _ => ()
        }

        val expectedThisType: NonPrimitiveType = constr.resultType
        checkIsSubtype(constr, outputContext("this"), expectedThisType)

        checkForUnusedOwnershipErrors(constr, outputContext, Set("this"))
        checkForUnusedStateInitializers(outputContext)

        // if the contract contains states, its constructor must contain a state transition
        if (hasStates && !hasTransition(constr.body)) {
            logError(constr, NoStartStateError(constr.name))
        }

    }

    private def checkConstructors(constructors: Seq[Constructor], contract: Contract, table: ContractTable): Unit = {
        if (constructors.isEmpty && table.stateLookup.nonEmpty) {
            logError(contract, NoConstructorError(contract.name))
        }

        if (constructors.length > 1 && contract.isMain) {
            logError(contract, MultipleConstructorsError(contract.name))
        }

        val constructorsByArgTypes = constructors.groupBy(c => c.args.map(_.typIn.topPermissionType))
        val matchingConstructors = constructorsByArgTypes.filter(_._2.size > 1)

        matchingConstructors.foreach(typeAndConstructors => {
            val greatestLine = typeAndConstructors._2.maxBy(_.loc.line)
            logError(greatestLine, RepeatConstructorsError(contract.name))
        })
    }

    private def checkForMainContract(ast: Program) = {
        val c: Option[Contract] = ast.contracts.find((c: Contract) =>
            c.modifiers.contains(IsMain()))
        if (c == None) logError(ast, NoMainContractError())

    }

    private def checkContract(contract: Contract): Unit = {
        val table = globalTable.contractLookup(contract.name)
        for (decl <- contract.declarations) {
            decl match {
                case t: Transaction => checkTransaction(t, table)
                case s: State => checkState(table, s)
                case f: Field => {
                    checkField(f, table, None)
                    checkContractFieldRepeats(f, contract)
                }
                case c: Constructor => checkConstructor(c, table, table.stateLookup.nonEmpty)
                case _ => () // TODO
            }
        }
        checkConstructors(table.constructors, contract, table)

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

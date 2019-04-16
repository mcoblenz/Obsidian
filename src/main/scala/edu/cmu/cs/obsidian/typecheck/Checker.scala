package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.lexer.FalseT
import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.parser._

import scala.collection.immutable.{HashSet, TreeMap, TreeSet}



/* We define a custom type to store a special flag for if a context in after a "throw".
 * In the formalism, we allow throw to result in any type: in the implementation, we don't know
 * immediately which type this needs to be in order for type checking to work
 * transitionFieldsInitialized is a set of (state, field, AST) triples that are guaranteed to have been initialized.
 * The AST is for error message generation.
 *
 * localFieldsInitialized is a set of state and contract fields that have been initialized.
 * valVariables is a set of variables that were declared val instead of var, i.e. reassignment to them is forbidden.
 */

case class Context(table: DeclarationTable,
                   underlyingVariableMap: Map[String, ObsidianType],
                   isThrown: Boolean,
                   transitionFieldsInitialized: Set[(String, String, AST)],
                   localFieldsInitialized: Set[String],
                   thisFieldTypes: Map[String, ObsidianType],
                   valVariables : Set[String]) {
    def keys: Iterable[String] = underlyingVariableMap.keys

    def updated(s: String, t: ObsidianType): Context =
        Context(contractTable,
            underlyingVariableMap.updated(s, t),
            isThrown,
            transitionFieldsInitialized,
            localFieldsInitialized,
            thisFieldTypes,
            valVariables)
    def updatedWithTransitionInitialization(stateName: String, fieldName: String, ast: AST): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsInitialized + ((stateName, fieldName, ast)),
            localFieldsInitialized,
            thisFieldTypes,
            valVariables)

    def updatedAfterTransition(): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            Set.empty,
            Set.empty,
            Map.empty,
            valVariables)

    def updatedWithFieldInitialization(fieldName: String): Context = {
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsInitialized,
            localFieldsInitialized + fieldName,
            thisFieldTypes,
            valVariables)
    }

    def updatedThisFieldType(fieldName: String, newType: ObsidianType): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsInitialized,
            localFieldsInitialized,
            thisFieldTypes.updated(fieldName, newType),
            valVariables)

    def updatedMakingVariableVal(variableName: String): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsInitialized,
            localFieldsInitialized,
            thisFieldTypes,
            valVariables + variableName)


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
    private def consumptionModeForType(typ: ObsidianType) : OwnershipConsumptionMode = {
        typ match {
            case np: NonPrimitiveType =>
                if (np.permission == Shared()) {
                    ConsumingOwnedGivesShared()
                }
                else if (np.isOwned) {
                    ConsumingOwnedGivesUnowned()
                }
                else {
                    NoOwnershipConsumption()
                }
            case _ => NoOwnershipConsumption()
        }
    }

    val errors = new collection.mutable.ArrayBuffer[ErrorRecord]()
    var currentContractSourcePath: String = ""

    /* an error is associated with an AST node to indicate where the error took place */
    private def logError(where: AST, err: Error): Unit = {
        assert(where.loc.line >= 1)
        errors += ErrorRecord(err, where.loc, currentContractSourcePath)

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
                                  (np1.permission == Owned() && np2.permission == Shared() && np3.permission == Shared() && np1.isAssetReference(contextContractTable) == No())
                            case _ => false // can't split non-reference types
                        }
                    case _ => false // can't split non-reference types
                }
            case _ => false // can't split non-reference types
        }
    }

    //-------------------------------------------------------------------------
    /* Subtyping definitions */

    /* method to check if contract is actually implementing an interface */
    private def contractIsSubtype(c1: String, c2: String): Boolean = {
        val c1Table = globalTable.contractLookup(c1)
        val c2Table = globalTable.contractLookup(c2)

        (c1Table.contract, c2Table.contract) match {
            case (obs1: ObsidianContractImpl, obs2: ObsidianContractImpl) => (obs1 == obs2)
            case (jvcon1: JavaFFIContractImpl, jvcon2: JavaFFIContractImpl) => (jvcon1 == jvcon2)
            case (obs: ObsidianContractImpl, jvcon: JavaFFIContractImpl) => false
            case (jvcon: JavaFFIContractImpl, obs:ObsidianContractImpl) => (obs.name == jvcon.interface)
        }
    }


    /* true iff [t1 <: t2] */
    private def isSubtype(t1: ObsidianType, t2: ObsidianType, isThis: Boolean): Option[Error] = {
        (t1, t2) match {
            case (BottomType(), _) => None
            case (_, BottomType()) => None
            case (IntType(), IntType()) => None
            case (BoolType(), BoolType()) => None
            case (StringType(), StringType()) => None
            case (np1: NonPrimitiveType, np2: NonPrimitiveType) =>
                val mainSubtype: Boolean = (np1, np2) match {
                    case (ContractReferenceType(c1, c1p, _), ContractReferenceType(c2, c2p, _)) =>
                        contractIsSubtype(c1.contractName, c2.contractName) && isSubpermission(c1p, c2p)
                    case (StateType(c1, ss1, _), StateType(c2, ss2, _)) =>
                        contractIsSubtype(c1, c2) && ss1.subsetOf(ss2)
                    case (StateType(c, ss1, _), ContractReferenceType(c2, c2p, _)) =>
                        c2 == ContractType(c)
                    case _ => false
                }
                if (!mainSubtype) Some(SubtypingError(t1, t2, isThis))
                // TODO: make sure this is unnecessary: else if (!modifierSubtype) Some(OwnershipSubtypingError(t1, t2))
                else None
            case _ => Some(SubtypingError(t1, t2, isThis))
        }
    }

    /* returns [t1] if [t1 <: t2], logs an error and returns [BottomType] otherwise */
    private def checkIsSubtype(ast: AST, t1: ObsidianType, t2: ObsidianType): ObsidianType = {
        val errorOpt = isSubtype(t1, t2, ast.isInstanceOf[This])
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
            case Inferred() => false
        }
    }
    //-------------------------------------------------------------------------
    private def nonPrimitiveMergeTypes(
            t1: NonPrimitiveType,
            t2: NonPrimitiveType,
            contractTable: ContractTable): Option[NonPrimitiveType] = {
        if (t1.contractName != t2.contractName) return None

        (t1, t2) match {
            case (ContractReferenceType(contractType, p1, isRemote), ContractReferenceType(_, p2, _)) =>
                if (p1 == p2) {
                    Some(t1)
                }
                else {
                    val typeForMismatchedPermissions =
                        if (t1.isAssetReference(contractTable) != No()) {
                            None
                        }
                        else {
                            Some(ContractReferenceType(contractType, Unowned(), isRemote))
                        }
                    p1 match {
                        case Owned() => typeForMismatchedPermissions
                        case Unowned() => if (p2 == Shared()) Some(t2) else typeForMismatchedPermissions
                        case Shared() => if (p2 == Unowned()) Some(t1) else typeForMismatchedPermissions
                        case Inferred() => assert(false, "Inferred types should be removed"); None
                    }
                }
            case (ContractReferenceType(contractType, permission, isRemote), StateType(_, _, _)) =>
                if (permission == Owned()) {
                    Some(t1)
                }
                else {
                    if (t1.isAssetReference(contractTable) != No()) {
                        None
                    }
                    else {
                        Some(ContractReferenceType(contractType, Unowned(), isRemote))
                    }
                }
            case (StateType(_, _, _), ContractReferenceType(contractType, permission, isRemote)) =>
                if (permission == Owned()) {
                    Some(t2)
                }
                else {
                    if (t1.isAssetReference(contractTable) != No()) {
                        None
                    }
                    else {
                        Some(ContractReferenceType(contractType, Unowned(), isRemote))
                    }
                }
            case (StateType(_, ss1, _), StateType(_, ss2, _)) =>
                val c = t1.contractName
                val unionStates = ss1.union(ss2)
                Some(StateType(c, unionStates, false))
            case _ => None
        }
    }

    private def mergeTypes(t1: ObsidianType, t2: ObsidianType, contractTable: ContractTable): Option[ObsidianType] = {
        (t1, t2) match {
            case (IntType(), IntType()) => Some(IntType())
            case (BoolType(), BoolType()) => Some(BoolType())
            case (StringType(), StringType()) => Some(StringType())
            case (np1: NonPrimitiveType, np2: NonPrimitiveType) =>
                nonPrimitiveMergeTypes(np1, np2, contractTable).flatMap(s => Some(s))
            case _ => None
        }
    }

    //-------------------------------------------------------------------------
    // Checking definitions for language constructs begins here

    private def inferAndCheckExpr(decl: InvokableDeclaration,
                                  context: Context,
                                  e: Expression,
                                  ownershipConsumptionMode: OwnershipConsumptionMode): (ObsidianType, Context, Expression) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: ObsidianType, c: Context): (ObsidianType, Context, Expression) = {
            val (tPrime, contextPrime, ePrime) = inferAndCheckExpr(decl, c, e, NoOwnershipConsumption())
            (checkIsSubtype(e, tPrime, t), contextPrime, ePrime)
        }

        //Too many outputs: two expressions are needed so they can be packaged in different binary
        // expression types. TODO: refactor to remove multiple outputs.
        def assertOperationType(e1: Expression, e2: Expression, t: ObsidianType): (ObsidianType, Context, Expression, Expression) = {
            val (_, c1, e1Prime) = assertTypeEquality(e1, t, context)
            val (_, c2, e2Prime) = assertTypeEquality(e2, t, c1)
            (t, c2, e1Prime, e2Prime)
        }

        //Too many outputs: two expressions are needed so they can be packaged in different binary
        // expression types. TODO: refactor to remove multiple outputs.
        def assertComparisonType(e1: Expression, e2: Expression): (ObsidianType, Context, Expression, Expression) = {
            val (_, c1, e1Prime) = assertTypeEquality(e1, IntType(), context)
            val (_, c2, e2Prime) = assertTypeEquality(e2, IntType(), c1)
            (BoolType(), c2, e1Prime, e2Prime)
        }

        def handleInvocation(
                context: Context,
                name: String,
                receiver: Expression,
                args: Seq[Expression]): (ObsidianType, Context, Boolean, Expression, Seq[Expression]) = {
            val (receiverType, contextAfterReceiver, receiverPrime) = inferAndCheckExpr(decl, context, receiver, NoOwnershipConsumption())


            // Terrible special case just for now. TODO: remove this.
            if (name == "sqrt" && args.length == 1) {
                // Int isn't really right either, but it will have to do for now.
                return (IntType(), context, false, receiverPrime, args)
            }

            // Eliminate things we can't invoke methods on first.
            val nonPrimitiveReceiverType = receiverType match {
                case BottomType() => return (BottomType(), contextAfterReceiver, false, receiverPrime, args)
                case UnitType() | IntType() | BoolType() | StringType() =>
                    logError(e, NonInvokeableError(receiverType))
                    return (BottomType(), contextAfterReceiver, false, receiverPrime, args)
                case np: NonPrimitiveType => np
            }

            //Finding out if the Contract is a FFIContract
            val isFFIInvocation = receiverType match {
                case BottomType() | UnitType() | IntType() | BoolType() | StringType() => false
                case np : NonPrimitiveType =>
                    val contractTableOpt = context.contractTable.lookupContract(np.contractName)
                    contractTableOpt match {
                        case None => false
                        case Some(x) => x.contract match {
                            case obsContract : ObsidianContractImpl => false
                            case javaContract : JavaFFIContractImpl => true
                        }
                    }
            }


            val foundTransaction = contextAfterReceiver.lookupTransactionInType(receiverType)(name)

            val invokable: InvokableDeclaration = foundTransaction match {
                case None =>
                    val err = MethodUndefinedError(nonPrimitiveReceiverType, name)
                    logError(e, err)
                    return (BottomType(), contextAfterReceiver, isFFIInvocation, receiverPrime, args)
                case Some(t) => t
            }

            if (receiverType.isInstanceOf[InterfaceContractType] && !foundTransaction.get.isStatic) {
                val err = NonStaticAccessError(foundTransaction.get.name, receiver.toString)
                logError(e, err)
                return (BottomType(), contextAfterReceiver, isFFIInvocation, receiverPrime, args)
            }

            if (!invokable.isStatic && isSubtype(receiverType, invokable.thisType, receiver.isInstanceOf[This]).isDefined) {
                logError(e, ReceiverTypeIncompatibleError(name, receiverType, invokable.thisType))
            }

            // Check field types for private invocations
            for ((fieldName, requiredInitialFieldType) <- foundTransaction.get.initialFieldTypes) {
                val currentFieldType = context.lookupCurrentFieldTypeInThis(fieldName)
                currentFieldType match {
                   case None => ()
                   case Some(cft) => if(isSubtype(cft, requiredInitialFieldType, receiver.isInstanceOf[This]).isDefined) {
                       logError(e, FieldSubtypingError(fieldName, cft, requiredInitialFieldType))
                   }
                }
            }

            // check arguments
            val spec = invokable.args
            val specList = (spec, invokable)::Nil

            val (exprSequence, contextAfterArgs, correctInvokable) =
                checkArgs(e, contextAfterReceiver, specList, args) match {
                    case None => return (BottomType(), contextAfterReceiver, isFFIInvocation, receiverPrime, args)
                    case Some(x) => x
                }

            val resultType = correctInvokable.retType match {
                case None => UnitType()
                case Some(typ) => typ
            }

            val contextPrime =
                correctInvokable match {
                    case t: Transaction =>
                        updateReceiverTypeInContext(receiver, receiverType, t, contextAfterArgs)
                    case _ => contextAfterArgs
                }

            // Update field types if we invoked a private method.
            val contextAfterPrivateInvocation = updateFieldsForPrivateInvocation(contextPrime, foundTransaction.get)

            (resultType, contextAfterPrivateInvocation, isFFIInvocation, receiverPrime, exprSequence)

        }

         e match {
             case ReferenceIdentifier(x) =>
                 (context get x, context.lookupCurrentFieldTypeInThis(x)) match {
                     case (Some(t), _) =>
                         // We always want x to have the type according to the context, but sometimes we're going to consume ownership.

                         // Consuming in a context that expects ownership results in Unowned.
                         // But consuming in a context that expects sharing results in Shared.
                         val newType = t.residualType(ownershipConsumptionMode)
                         if (newType != t) {
                             (t, context.updated(x, newType), e)
                         }
                         else {
                             (t, context, e)
                         }
                     case (_, Some(t)) =>
                         val newType = t.residualType(ownershipConsumptionMode)
                         if (newType != t) {
                             (t, context.updatedThisFieldType(x, newType), e)
                         }
                         else {
                             (t, context, e)
                         }
                     case (None, None) =>
                         val tableLookup = context.contractTable.lookupContract(x)
                         if (!tableLookup.isEmpty) {
                             val contractTable = tableLookup.get
                             val nonPrimitiveType = ContractReferenceType(contractTable.contractType, Shared(), false)
                             (InterfaceContractType(contractTable.name, nonPrimitiveType), context, e)
                         }
                         else {
                             logError(e, VariableUndefinedError(x, context.thisType.toString))
                             (BottomType(), context, e)
                         }
                 }
             case NumLiteral(_) => (IntType(), context, e)
             case StringLiteral(_) => (StringType(), context, e)
             case TrueLiteral() => (BoolType(), context, e)
             case FalseLiteral() => (BoolType(), context, e)
             case This() =>
                 val thisType = context.thisType
                 val newContext =
                     if (ownershipConsumptionMode != NoOwnershipConsumption()) {
                         context.updated("this", thisType.residualType(ownershipConsumptionMode))
                     }
                     else {
                         context
                     }
                 (thisType, newContext, e)
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
                 (BottomType(), context, e)
             case Conjunction(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertOperationType(e1, e2, BoolType())
                 (typ, con, Conjunction(e1Prime, e2Prime).setLoc(e))
             case Disjunction(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertOperationType(e1, e2, BoolType())
                 (typ, con, Disjunction(e1Prime, e2Prime).setLoc(e))
             case LogicalNegation(e: Expression) =>
                 assertTypeEquality(e, BoolType(), context)
             case Add(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertOperationType(e1, e2, IntType())
                 (typ, con, Add(e1Prime, e2Prime).setLoc(e))
             case Subtract(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertOperationType(e1, e2, IntType())
                 (typ, con, Subtract(e1Prime, e2Prime).setLoc(e))
             case Divide(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertOperationType(e1, e2, IntType())
                 (typ, con, Divide(e1Prime, e2Prime).setLoc(e))
             case Multiply(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertOperationType(e1, e2, IntType())
                 (typ, con, Multiply(e1Prime, e2Prime).setLoc(e))
             case Negate(e: Expression) =>
                 assertTypeEquality(e, IntType(), context)
             case Equals(e1: Expression, e2: Expression) =>
                 val (t1, c1, e1Prime) = inferAndCheckExpr(decl, context, e1, NoOwnershipConsumption())
                 val (t2, c2, e2Prime) = inferAndCheckExpr(decl, c1, e2, NoOwnershipConsumption())
                 if (t1 == t2) (BoolType(), c2, Equals(e1Prime, e2Prime).setLoc(e)) else {
                     logError(e, DifferentTypeError(e1, t1, e2, t2))
                     (BottomType(), c2, Equals(e1Prime, e2Prime).setLoc(e))
                 }
             case GreaterThan(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertComparisonType(e1, e2)
                 (typ, con, GreaterThan(e1Prime, e2Prime).setLoc(e))
             case GreaterThanOrEquals(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertComparisonType(e1, e2)
                 (typ, con, GreaterThanOrEquals(e1Prime, e2Prime).setLoc(e))
             case LessThan(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertComparisonType(e1, e2)
                 (typ, con, LessThan(e1Prime, e2Prime).setLoc(e))
             case LessThanOrEquals(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertComparisonType(e1, e2)
                 (typ, con, LessThanOrEquals(e1Prime, e2Prime).setLoc(e))
             case NotEquals(e1: Expression, e2: Expression) =>
                 val (t1, c1, e1Prime) = inferAndCheckExpr(decl, context, e1, NoOwnershipConsumption())
                 val (t2, c2, e2Prime) = inferAndCheckExpr(decl, c1, e2, NoOwnershipConsumption())
                 if (t1 == t2) (BoolType(), c2, NotEquals(e1Prime, e2Prime).setLoc(e)) else {
                     logError(e, DifferentTypeError(e1, t1, e2, t2))
                     (BottomType(), c2, NotEquals(e1Prime, e2Prime).setLoc(e))
                 }

             case Dereference(eDeref: Expression, fieldName) =>
                 eDeref match {
                     case This() =>
                         context.lookupCurrentFieldTypeInThis(fieldName) match {
                             case Some(t) =>
                                 val newType = t.residualType(ownershipConsumptionMode)
                                 if (newType != t) {
                                     (t, context.updatedThisFieldType(fieldName, newType), e)
                                 }
                                 else {
                                     (t, context, e)
                                 }
                             case None =>
                                 logError(e, FieldUndefinedError(context.thisType, fieldName))
                                 (BottomType(), context, e)
                         }

                     case _ =>
                         val (newExpr: Expression, newContext: Context) =
                             inferAndCheckExpr(decl, context, eDeref, ownershipConsumptionMode) match {
                                 case (np: NonPrimitiveType, c, ePrime) =>
                                     logError(e, InvalidNonThisFieldAccess())
                                     (ePrime, c)
                                 case (typ, c, ePrime) =>
                                     if (!typ.isBottom) {
                                         logError(e, DereferenceError(typ))
                                         (ePrime, c)
                                     }
                             }
                         (BottomType(), newContext, newExpr)
                 }

             case LocalInvocation(name, args: Seq[Expression]) =>
                 val (typ, con, _, _, newArgs) = handleInvocation(context, name, This(), args)
                 //This may need correction.
                 (typ, con, LocalInvocation(name, newArgs))

             case Invocation(receiver: Expression, name, args: Seq[Expression], isFFIInvocation) =>
                 val (typ, con, isFFIInv, newReceiver, newArgs) = handleInvocation(context, name, receiver, args)
                 (typ, con, Invocation(newReceiver, name, newArgs, isFFIInv))

             case Construction(name, args: Seq[Expression], isFFIInvocation) =>
                 val tableLookup = context.contractTable.lookupContract(name)
                 val isFFIInv = tableLookup match {
                     case None => false
                     case Some(x) => x.contract match {
                         case obsContract: ObsidianContractImpl => false
                         case javaContract: JavaFFIContractImpl => true
                     }
                 }

                 if (tableLookup.isEmpty) {
                     logError(e, ContractUndefinedError(name))
                     return (BottomType(), context, e)
                 }

                 val ctTableOfConstructed = tableLookup.get

                 val constrSpecs = ctTableOfConstructed
                                    .constructors
                                    .map(constr => (constr.args, constr))

                 val result = checkArgs(e, context, constrSpecs, args)

                 val (exprList, simpleType, contextPrime) = result match {
                     // Even if the args didn't check, we can still output a type
                     case None => (Nil, ContractReferenceType(ctTableOfConstructed.contractType, Owned(), false), context)
                     case Some((newExprSequence, cntxt, constr)) => (newExprSequence, constr.asInstanceOf[Constructor].resultType, cntxt)
                 }

                 (simpleType, contextPrime, Construction(name, exprList, isFFIInv))

             case Disown(e) =>
                 // The expression "disown e" evaluates to an unowned value but also side-effects the context
                 // so that e is no longer owned (if it is a variable).
                 val (typ, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, ownershipConsumptionMode)
                 if (!typ.isOwned) {
                    logError(e, DisownUnowningExpressionError(e))
                 }

                 val newTyp = typ match {
                     case n: NonPrimitiveType => n.residualType(ownershipConsumptionMode)
                     case t => t
                 }

                 // If e is a variable, then we need to update the context to indicate that it's no longer owned.
                 val finalContext = e match {
                     case ReferenceIdentifier(x) => contextPrime.updated(x, newTyp)
                     case _ => contextPrime
                 }
                 (newTyp, finalContext, ePrime)
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

                 (fieldType, context, e)
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
                case Return() | ReturnExpr(_) | Revert(_) => hasRet = true
                case IfThenElse(_, s1, s2) =>
                    hasRet = hasReturnStatement(tx, s1) && hasReturnStatement(tx, s2)
                case IfInState(e, state, s1, s2) =>
                    hasRet = hasReturnStatement(tx, s1) && hasReturnStatement(tx, s2)
                case Switch(e, cases) =>
                    hasRet = cases.foldLeft(true)((prev, aCase) => prev && hasReturnStatement(tx, aCase.body))
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
                case Transition(_, _, _) => transition = true
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
                                      ): (Context, Seq[Statement]) = {
        s.foldLeft((context, Seq.empty[Statement]))((prev: (Context, Seq[Statement]), s: Statement) => {
            val (newContext, newStatement) = checkStatement(decl, prev._1, s)
            (newContext, prev._2 :+ newStatement)
            }
        )
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
                    if (isSubtype(typ, declaredFieldType, false).isDefined) {
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
                        if (isSubtype(currentTyp, requiredTyp, false).isDefined) {
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

        Context(oldContext.contractTable,
            newContext.underlyingVariableMap,
            isThrown = branchContext.isThrown,
            branchContext.transitionFieldsInitialized,
            branchContext.localFieldsInitialized,
            branchContext.thisFieldTypes,
            branchContext.valVariables)
    }

    private def errorIfNotDisposable(variable: String, typ: ObsidianType, context: Context, ast: AST): Unit = {
        typ match {
            case t: NonPrimitiveType =>
                if (t.isOwned && t.isAssetReference(context.contractTable) != No()) logError(ast, UnusedOwnershipError(variable))
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
                mergeTypes(t1, t2, context1.contractTable) match {
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

        Context(context1.contractTable,
            mergedVariableMap,
            context1.isThrown,
            context1.transitionFieldsInitialized.intersect(context2.transitionFieldsInitialized),
            context1.localFieldsInitialized.intersect(context2.localFieldsInitialized),
            mergedThisFieldMap,
            context1.valVariables.intersect(context2.valVariables))
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
            args: Seq[Expression]): Either[Seq[(AST, Error)], (Seq[Expression], Context)] = {

        var errList: List[(AST, Error)] = Nil
        val (specL, argsL) = (spec.length, args.length)

        if (specL != argsL) {
            val name = if (decl.isInstanceOf[Constructor]) s"constructor of ${decl.name}" else decl.name
            Left((ast, WrongArityError(specL, argsL, name))::errList)
        } else {
            var contextAfterArgs = context
            var expressionList = Seq[Expression]()
            for (i <- args.indices) {
                val arg = args(i)
                val specInputType = spec(i).typIn
                val specOutputType = spec(i).typOut

                val transferOwnership: Boolean =
                    specInputType match {
                        case np: NonPrimitiveType =>
                            (np.isOwned && !specOutputType.isOwned)
                        case _ => true
                    }

                val ownershipConsumptionMode =
                    if (specOutputType.isOwned) {
                        NoOwnershipConsumption()
                    }
                    else {
                        consumptionModeForType(specInputType)
                    }

                val (argType, contextAfterArg, e) = inferAndCheckExpr(decl, contextAfterArgs, arg, ownershipConsumptionMode)

                if (isSubtype(argType, specInputType, false).isDefined) {
                    val err = ArgumentSubtypingError(decl.name, spec(i).varName, argType, specInputType)
                    errList = (ast, err)::errList
                }

                contextAfterArgs = contextAfterArg
                expressionList = (expressionList :+ e)
            }

            if (errList.isEmpty) Right((expressionList, contextAfterArgs))
            else Left(errList)
        }
    }

    // checks the arguments based on possibly many specifications, such as multiple constructors
    // logs all errors at the end
    private def checkArgs(
            ast: AST,
            context: Context,
            specs: Seq[(Seq[VariableDeclWithSpec], InvokableDeclaration)],
            args: Seq[Expression]): Option[(Seq[Expression], Context, InvokableDeclaration)] = {

        var errs: List[(AST, Error)] = Nil
        for ((spec, invokable) <- specs) {
            checkArgsWithSpec(ast, invokable, context, spec, args) match {
                case Right((expressionList, context)) => return Some((expressionList, context, invokable))
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

        val nameToUpdate = receiver match {
            case ReferenceIdentifier(x) => Some(x)
            case This() => Some("this")
            case _ => None
        }

        val contextPrime = nameToUpdate match {
            case None => context
            case Some(x) =>
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
                                val currentFieldType = context.lookupCurrentFieldTypeInThis(x)

                                if (currentFieldType.isDefined && currentFieldType.get != newType) {
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
    contextPrime
}

private def checkStatement(
                                  decl: InvokableDeclaration,
                                  context: Context,
                                  s: Statement
                              ): (Context, Statement) = {

        def checkAssignment(x: String, e: Expression, context: Context, mustBeField: Boolean): (Context, Statement, Expression) = {
            // Consuming owned gives unowned because the lvalue is going to be of owning type.
            val (exprType, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, ConsumingOwnedGivesUnowned())
            val localVariableType =
                if (mustBeField) {
                    // Don't look for a matching local variable if this is required to be a field.
                    None
                }
                else {
                    contextPrime.get(x)
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

            val contextWithAssignmentUpdate =
                if (isField) {
                    contextPrime.updatedWithFieldInitialization(x)
                }
                else {
                    // TODO: give errors for uninitialized local variables.
                    contextPrime
                }

            val newContext = (exprType, variableType) match {
                case (exprNPType: NonPrimitiveType, variableNPType: NonPrimitiveType) =>
                    if (variableNPType.isOwned && variableNPType.isAssetReference(context.contractTable) != No()) {
                        logError(s, OverwrittenOwnershipError(x))
                    }
                    if (exprNPType.contractName != variableNPType.contractName) {
                        logError(s, InconsistentContractTypeError(variableNPType.contractName, exprNPType.contractName))
                        contextWithAssignmentUpdate
                    }
                    else if (isField) {
                        if (variableType != exprType) {
                            contextWithAssignmentUpdate.updatedThisFieldType(x, exprType)
                        }
                        else {
                            contextWithAssignmentUpdate
                        }
                    }
                    else {
                        contextWithAssignmentUpdate.updated(x, exprType)
                    }
                case (_, _) =>
                    checkIsSubtype(s, exprType, variableType)
                    contextWithAssignmentUpdate
            }
            (newContext, s, ePrime)
        }


        s match {
            case VariableDecl(typ: ObsidianType, name) =>
                typ match {
                    case np: NonPrimitiveType =>
                        if (np.permission != Inferred()) {
                            logError(s, (InvalidLocalVariablePermissionDeclarationError()))
                        }
                    case _ => // Nothing to check
                }
                (context.updated(name, typ), s)

            case VariableDeclWithInit(typ: ObsidianType, name, e: Expression) =>
                val (exprType, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, ConsumingOwnedGivesUnowned())
                val declaredType = typ match {
                    case np: NonPrimitiveType =>
                        val declaredContractName = np.contractName

                        val tableLookup = context.contractTable.lookupContract(declaredContractName)

                        val resolvedType = tableLookup match {
                            case None =>
                                logError(s, ContractUndefinedError(declaredContractName))
                                BottomType()
                            case Some(_) =>
                                exprType match {
                                    case exprNonPrimitiveType: NonPrimitiveType =>
                                        if (exprNonPrimitiveType.contractName == declaredContractName) {
                                            exprType
                                        }
                                        else {
                                            logError(s, InconsistentContractTypeError(declaredContractName, exprNonPrimitiveType.contractName))
                                            np // Just go with the declaration
                                        }
                                    case BottomType() =>
                                        // Don't emit an extra error message.
                                        BottomType()
                                    case _ =>
                                        logError(s, InconsistentTypeAssignmentError(typ, exprType))
                                        np // Just go with the declaration

                                }
                        }

                        np.permission match {
                            case Inferred() => resolvedType
                            case _ =>
                                logError(s, (InvalidLocalVariablePermissionDeclarationError()))
                                BottomType()
                        }
                    case BottomType() => BottomType()
                    case _ =>
                        checkIsSubtype(s, exprType, typ)
                        typ
                }

                (contextPrime.updated(name, declaredType), VariableDeclWithInit(exprType, name, ePrime))

            case Return() =>
                decl match {
                    /* the tx/function must have no return type */
                    case tx: Transaction if tx.retType.isEmpty =>
                        checkForUnusedOwnershipErrors(s, context, Set("this"))
                        (context, s)
                    case _ =>
                        logError(s, MustReturnError(decl.name))
                        (context, s)
                }

            case ReturnExpr(e: Expression) =>
                val retTypeOpt = decl match {
                    /* must be no return type */
                    case tx: Transaction if tx.retType.isDefined => tx.retType
                    case _ =>
                        logError(s, CannotReturnError(decl.name))
                        return (context, s)
                }

                val consumeOwnership = retTypeOpt match {
                    case None => NoOwnershipConsumption()
                    case Some(retType) => consumptionModeForType(retType)
                }

                val (typ, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, consumeOwnership)

                val thisSetToExclude = e match {
                    case ReferenceIdentifier(xOther)
                        if retTypeOpt.isDefined && retTypeOpt.get.isOwned => Set(xOther, "this")
                    case _ => Set("this")
                }


                val argsSetToExclude =
                    decl match {
                        case tx: Transaction =>
                            val ownedArgs = tx.args.filter((arg: VariableDeclWithSpec) => arg.typOut.isOwned)
                            ownedArgs.map((arg: VariableDeclWithSpec) => arg.varName)
                        case _ => Set.empty
                    }

                checkForUnusedOwnershipErrors(s, contextPrime, thisSetToExclude ++ argsSetToExclude)

                if (retTypeOpt.isDefined && !retTypeOpt.get.isBottom) checkIsSubtype(s, typ, retTypeOpt.get)
                (contextPrime, ReturnExpr(ePrime))

            case Transition(newStateName, updates: Option[Seq[(ReferenceIdentifier, Expression)]], p) =>
                // TO CHECK:
                // 1. Assignments are only to fields that will be available in the new state.
                // (1a: which don't exist anywhere; 1b: which exist but not in the target state)
                // 2. All fields in the target state that MIGHT NOT exist in the current state must be initialized.
                // 3. All asset references in the source state that will not exist in the destination state
                //    must be unowned.


                val thisTable = context.contractTable

                if (thisTable.state(newStateName).isEmpty) {
                    logError(s, StateUndefinedError(thisTable.name, newStateName))
                    return (context, Transition(newStateName, updates, context.thisType.permission).setLoc(s))
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

                val possibleCurrentStates =
                    oldType match {
                        case ContractReferenceType(_, _, _) => thisTable.possibleStates
                        case InterfaceContractType(_, _) => thisTable.possibleStates
                        case StateType(_, stateNames, _) => stateNames
                    }

                def fieldsAvailableInState(stateName: String): Set[Field] = {
                    val allFields = thisTable.allFields
                    allFields.filter((f: Field) =>
                        f.availableIn.isEmpty || f.availableIn.get.contains(stateName))
                }

                // For each state that we might be in, compute the set of fields that could be available.
                val allContractFields = thisTable.allFields
                val possibleCurrentFields =
                    if (possibleCurrentStates.isEmpty) {
                        Set.empty[Field]
                    }
                    else {
                        possibleCurrentStates.map(fieldsAvailableInState).reduce(
                            (s1: Set[Field], s2: Set[Field]) => s1 union s2)
                    }

                val definiteCurrentFields = // These are definitely available now.
                    if (possibleCurrentStates.isEmpty) {
                        Set.empty[Field]
                    }
                    else {
                        possibleCurrentStates.map(fieldsAvailableInState).reduce(
                            (s1: Set[Field], s2: Set[Field]) => s1 intersect s2)
                    }

                val newFields = fieldsAvailableInState(newStateName)


                val fieldsToInitialize = newFields -- definiteCurrentFields // All the fields that must be initialized.

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

                val updatedFieldNames = updatedInTransition ++ updatedViaAssignment // Fields updated by either assignment or transition initialization
                val uninitializedFieldNames = fieldsToInitialize.map((f: Field) => f.name) -- updatedFieldNames

                if (uninitializedFieldNames.nonEmpty) {
                    logError(s, TransitionUpdateError(uninitializedFieldNames))
                }

                val badInitializations = updatedFieldNames -- newFields.map((f: Field) => f.name) // We don't allow updates to fields that don't exist in the target state.
                for (s <- badInitializations) {
                    val err = FieldUndefinedError(newStateTable.nonPrimitiveType, s)
                    logError(updates.get.find(_._1.name == s).get._1, err)
                }

                var contextPrime = context
                val newUpdates = Seq.empty
                var newUpdatesOption :Option[Seq[(ReferenceIdentifier, Expression)]] = None

                if (updates.isDefined) {
                    for ((ReferenceIdentifier(f), e) <- updates.get) {
                        val fieldAST = newStateTable.lookupField(f)
                        if (fieldAST.isDefined) {
                            val (t, contextPrime2, ePrime) = inferAndCheckExpr(decl, contextPrime, e, consumptionModeForType(fieldAST.get.typ))
                            newUpdates :+ (ReferenceIdentifier(f), ePrime)
                            contextPrime = contextPrime2
                            checkIsSubtype(s, t, fieldAST.get.typ)
                        }
                    }
                    newUpdatesOption = Some(newUpdates)
                }


                // Check for potentially-dropped resources.
                val toCheckForDroppedAssets = possibleCurrentFields -- newFields // fields that we might currently have minus fields we're initializing now
                for (oldField <- toCheckForDroppedAssets) {
                    val fieldType = contextPrime.thisFieldTypes.getOrElse(oldField.name, oldField.typ)
                    if (fieldType.isAssetReference(thisTable) != No() && fieldType.isOwned) {
                        logError(s, PotentiallyUnusedOwnershipError(oldField.name))
                    }
                }

                val newTypeTable = thisTable.contractTable.state(newStateName).get
                val newSimpleType =
                    if (oldType.isOwned) {
                        StateType(thisTable.name, newStateName, false)
                    }
                    else {
                        // If the old "this" was unowned, we'd better not steal ownership for ourselves here.
                        oldType
                    }

                (contextPrime.updated("this", newSimpleType).updatedAfterTransition(), Transition(newStateName, newUpdatesOption, context.thisType.permission).setLoc(s))

            case Assignment(ReferenceIdentifier(x), e: Expression) =>
                if (context.valVariables.contains(x)) {
                    logError(s, InvalidValAssignmentError())
                }
                val (contextPrime, statementPrime, ePrime) = checkAssignment(x, e, context, false)
                (contextPrime, Assignment(ReferenceIdentifier(x), ePrime).setLoc(s))

            case Assignment(Dereference(eDeref, f), e: Expression) =>
                if (eDeref != This()) {
                    logError(s, InvalidNonThisFieldAssignment())
                    (context, s)
                }
                else {
                    val (contextPrime, statementPrime, ePrime) = checkAssignment(f, e, context, true)
                    (contextPrime, Assignment(Dereference(eDeref, f), ePrime).setLoc(s))
                }
            case Assignment(StateInitializer(stateName, fieldIdentifier), e) =>
                val stateOption = context.contractTable.state(stateName._1)
                val fieldType = stateOption match {
                    case None => logError(s, StateUndefinedError(context.contractTable.name, stateName._1)); BottomType()
                    case Some(stateTable) =>
                        stateTable.lookupField(fieldIdentifier._1) match {
                            case None => logError(s, FieldUndefinedError(stateTable.nonPrimitiveType, fieldIdentifier._1)); BottomType()
                            case Some(field) => field.typ
                        }
                }

                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, consumptionModeForType(fieldType))

                checkIsSubtype(s, t, fieldType)
                if (fieldType == BottomType()) {
                    (contextPrime, s)
                }
                else {
                    (contextPrime.updatedWithTransitionInitialization(stateName._1, fieldIdentifier._1, s),
                      Assignment(StateInitializer(stateName, fieldIdentifier), ePrime))
                }

            // assignment target is neither a variable nor a field
            case Assignment(subE: Expression, e: Expression) =>
                val (_, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, NoOwnershipConsumption())
                logError(s, AssignmentError())
                (contextPrime, Assignment(subE, ePrime).setLoc(s))

            case Revert(e) =>
                val (contextPrime, ePrime) =
                    e match {
                        case None => (context, e)
                        case Some(expr) =>
                            val (eTyp, exprContext, newE) = inferAndCheckExpr(decl, context, e.get, NoOwnershipConsumption())
                            checkIsSubtype(expr, eTyp, StringType())
                            (exprContext, Some(newE))
                    }

                // If exceptions are ever catchable, we will need to make sure the fields of this have types consistent with their declarations.
                // For now, we treat this like a permanent abort.
                (contextPrime.makeThrown, Revert(ePrime).setLoc(s))

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, eCond, NoOwnershipConsumption())
                checkIsSubtype(s, t, BoolType())
                val (newContext, checkedStatements) = checkStatementSequence(decl, contextPrime, body)
                val contextIfTrue = pruneContext(s,
                    newContext,
                    contextPrime)
                (mergeContext(s, contextPrime, contextIfTrue), If(ePrime, checkedStatements).setLoc(s))

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, eCond, NoOwnershipConsumption())
                checkIsSubtype(s, t, BoolType())
                val (trueContext, checkedTrueStatements) = checkStatementSequence(decl, contextPrime, body1)
                val contextIfTrue = pruneContext(s,
                    trueContext,
                    contextPrime)

                val (falseContext, checkedFalseStatements) = checkStatementSequence(decl, contextPrime, body2)

                val contextIfFalse = pruneContext(s,
                    falseContext,
                    contextPrime)
                (mergeContext(s, contextIfFalse, contextIfTrue), IfThenElse(ePrime, checkedTrueStatements, checkedFalseStatements).setLoc(s))

            case IfInState(e, state, body1, body2) =>

                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, NoOwnershipConsumption())

                val contractName = t match {
                    case np: NonPrimitiveType =>
                        np.contractName
                    case p: PrimitiveType =>
                        if (!t.isBottom) {
                          logError(e, StateCheckOnPrimitiveError())
                        }
                        // There was previously some kind of error. Don't propagate it.
                        return (contextPrime, IfInState(ePrime, state, body1, body2).setLoc(s))
                    case _ =>
                        if (!t.isBottom) {
                            logError(e, SwitchError(t))
                        }
                        // There was previously some kind of error. Don't propagate it.
                        return (contextPrime, IfInState(ePrime, state, body1, body2).setLoc(s))
                }

                val contractTable = context.contractTable.lookupContract(contractName) match {
                    case Some(table) => table
                    case None =>
                        logError(e, SwitchError(t))
                        return (contextPrime, IfInState(ePrime, state, body1, body2).setLoc(s))
                }

                val allStates = contractTable.possibleStates

                var resetOwnership: Option[(String, NonPrimitiveType)] = None

                val (contextForCheckingTrueBranch, contextForCheckingFalseBranch) =
                    t match {
                        case p: PrimitiveType =>
                            logError(s, StateCheckOnPrimitiveError())
                            (contextPrime, contextPrime)
                        case np: NonPrimitiveType =>
                            val ident = e match {
                                // If e is a variable, we might be able to put it in the context with the appropriate state.
                                // If it's not a variable, we just check the state and move on (no context changes).
                                case ReferenceIdentifier(x) => Some(x)
                                case This() => Some("this")
                                case _ => None
                            }
                            ident match {
                                case Some(x) =>
                                    np.permission match {
                                        case Owned() =>
                                            val newType = StateType(np.contractName, Set(state._1), np.isRemote)
                                            t match {
                                                case StateType(_, specificStates, _) =>
                                                    if (specificStates.size == 1 && specificStates.contains(state._1)) {
                                                        logError(e, StateCheckRedundant())
                                                    }
                                                    val typeFalse = StateType(np.contractName, specificStates - state._1, np.isRemote)
                                                    (contextPrime.updated(x, newType).updatedMakingVariableVal(x), contextPrime.updated(x, typeFalse).updatedMakingVariableVal(x))
                                                case _ =>
                                                    if (allStates.size == 1 && allStates.contains(state._1)) {
                                                        logError(e, StateCheckRedundant())
                                                    }
                                                    val typeFalse = StateType(np.contractName, allStates - state._1, np.isRemote)
                                                    (contextPrime.updated(x, newType).updatedMakingVariableVal(x), contextPrime.updated(x, typeFalse).updatedMakingVariableVal(x))
                                            }
                                        case Unowned() => (contextPrime, contextPrime)
                                        case Shared() | Inferred() =>
                                            // If it's Inferred(), there's going to be another error later. For now, be optimistic.
                                            if (allStates.size == 1 && allStates.contains(state._1)) {
                                                logError(e, StateCheckRedundant())
                                            }
                                            val newType = StateType(np.contractName, Set(state._1), np.isRemote)
                                            val typeFalse = StateType(np.contractName, allStates - state._1, np.isRemote)
                                            resetOwnership = Some((x, np))
                                            (contextPrime.updated(x, newType).updatedMakingVariableVal(x), contextPrime.updated(x, typeFalse).updatedMakingVariableVal(x))
                                    }
                                case None => (contextPrime, contextPrime)
                            }
                        case BottomType() => (contextPrime, contextPrime)
                    }

                val (trueContext, checkedTrueStatements) = checkStatementSequence(decl, contextForCheckingTrueBranch, body1)
                val (falseContext, checkedFalseStatements) = checkStatementSequence(decl, contextForCheckingFalseBranch, body2)

                val (resetTrueContext, resetFalseContext) = resetOwnership match {
                    case None => (trueContext, falseContext)
                    case Some((x, oldType)) => (trueContext.updated(x, oldType), falseContext.updated(x, oldType))
                }

                val contextIfTrue = pruneContext(s,
                    resetTrueContext,
                    contextForCheckingTrueBranch)

                val contextIfFalse = pruneContext(s,
                    resetFalseContext,
                    contextPrime)

                val mergedContext = mergeContext(s, contextIfFalse, contextIfTrue)
                val newStatement = IfInState(ePrime, state, checkedTrueStatements, checkedFalseStatements).setLoc(s)

                (mergedContext, newStatement)
            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) =>
                val (tryContext, checkedTryStatements) = checkStatementSequence(decl, context, s1)
                val (catchContext, checkedCatchStatements) = checkStatementSequence(decl, context, s2)


                val contextIfTry = pruneContext(s,
                    tryContext,
                    context)
                val contextIfCatch = pruneContext(s,
                    catchContext,
                    context)
                (mergeContext(s, contextIfTry, contextIfCatch), TryCatch(checkedTryStatements, checkedCatchStatements).setLoc(s))

            case Switch(e: Expression, cases: Seq[SwitchCase]) =>
                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, NoOwnershipConsumption())

                val contractName = t match {
                    case np: NonPrimitiveType =>
                        np.contractName
                    case _ =>
                        if (!t.isBottom) {
                            logError(e, SwitchError(t))
                        }
                        // There was previously some kind of error. Don't propagate it.
                        return (contextPrime, Switch(ePrime, cases).setLoc(s))
                }

                val contractTable = context.contractTable.lookupContract(contractName) match {
                    case Some(table) => table
                    case None => logError(e, SwitchError(t))
                        return (contextPrime, Switch(ePrime, cases).setLoc(s))
                }

                def checkSwitchCase(sc: SwitchCase) : (Context, SwitchCase) = {
                    val newType: ObsidianType =
                        contractTable.state(sc.stateName) match {
                            case Some(stTable) =>
                                StateType(contractTable.name, stTable.name, false)
                            case None =>
                                logError(sc, StateUndefinedError(contractTable.name, sc.stateName))
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
                        case ReferenceIdentifier(x) =>
                            if (contextPrime.get(x).isDefined) {
                                // We're switching on a local variable or formal parameter.
                                contextPrime.updated(x, newType)
                            }
                            else {
                                // This must be a field.
                                contextPrime.updatedThisFieldType(x, newType)
                            }
                        case _ => contextPrime
                    }

                    val (contextFromBody, newBody) = checkStatementSequence(decl, startContext, sc.body)
                    val prunedContext = pruneContext(s, contextFromBody, startContext)

                    (prunedContext, sc.copy(body = newBody))
                }

                val (mergedContext, newCases) = cases.headOption match {
                    case None => (contextPrime, cases)
                    case Some(switchCase) =>
                        val (initialContext, newCase) = checkSwitchCase(switchCase)
                        val restCases = cases.tail
                        restCases.foldLeft((initialContext, Seq(newCase)))((prev: (Context, Seq[SwitchCase]), sc: SwitchCase) => {
                            val (newContext, newCase) = checkSwitchCase(sc)
                            (mergeContext(s, prev._1, newContext), newCase +: prev._2)
                        })
                }


                (mergedContext, Switch(ePrime, newCases).setLoc(s))

            // TODO maybe allow constructors as statements later, but it's not very important
            case d@Disown (e) =>
                val (typ, contextPrime, ePrime) = inferAndCheckExpr(decl, context, d, ConsumingOwnedGivesShared())
                (contextPrime, Disown(ePrime).setLoc(s))
            case e: Expression =>
                val (typ, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, ConsumingOwnedGivesShared())
                if (typ.isOwned) {
                    logError(s, UnusedExpressionOwnershipError(e))
                }
                if (!(s.isInstanceOf[LocalInvocation] || s.isInstanceOf[Invocation])) {
                    logError(s, NoEffectsError(s))
                }
                (contextPrime, ePrime)
            case StaticAssert(e, allowedStatesOrPermissions) =>
                // We claim to consume ownership here so that typ include all the ownership information.
                // But at the end, we're going to throw out contextPrime and just continue with context.
                val (typ, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, ConsumingOwnedGivesUnowned())

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
                }

                (context, StaticAssert(ePrime, allowedStatesOrPermissions).setLoc(s)) // Not contextPrime!
            case _ =>
                logError(s, NoEffectsError(s))
                (context, s)
        }
    }

    private def checkField(field: Field, lexicallyInsideOf: ContractTable, containingState: Option[State]): Field = {
        val contractIsAsset = lexicallyInsideOf.contract.modifiers.contains(IsAsset())

        val containerIsAsset = containingState match {
            case None =>
                // This field is lexically inside a contract, not in a state, but it might be available in a particular set of states.

                // If the field is in a contract that was declared as a asset, the settings on the individual states don't matter.
                if (contractIsAsset) {
                    true
                }
                else if (field.availableIn.isDefined) {
                    val availableStateNames = field.availableIn.get
                    val availableStates = availableStateNames.map((stateName: String) => lexicallyInsideOf.state(stateName))
                    availableStates.foldLeft(true)((allAssetsSoFar: Boolean, state: Option[StateTable]) => allAssetsSoFar && state.isDefined && state.get.ast.isAsset)
                }
                else {
                    contractIsAsset
                }
            case Some(state) => contractIsAsset || state.isAsset
        }

        field.typ match {
            case typ: NonPrimitiveType =>
                if (typ.isOwned) {
                    // Only resources can own other resources (since otherwise they might go out of scope improperly).
                    if (typ.isAssetReference(lexicallyInsideOf) != No() && !containerIsAsset) {
                        logError(field, NonAssetOwningAssetError(lexicallyInsideOf.name, field))
                    }
                }
                if (typ.permission == Inferred()) {
                    logError(field, FieldMissingPermissionError(field.name))
                }
            case _ => ()
        }

        field
    }

    private def checkTransactionInState(tx: Transaction,
                                        lexicallyInsideOf: DeclarationTable,
                                        initContext: Context): Transaction = {

        var context = initContext

        for (arg <- tx.args) {
            context = initContext.updated(arg.varName, arg.typIn)
        }

        // Check the body; ensure [this] is well-typed after, and check for leaked ownership
        val (outputContext, newStatements) =
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
        for (arg <- tx.args) {
            outputContext.get(arg.varName) match {
                case Some(actualTypOut) =>
                    val errorOpt = isSubtype(actualTypOut, arg.typOut, false)
                    if (errorOpt.isDefined) {
                        logError(tx, ArgumentSpecificationError(arg.varName, tx.name, arg.typOut, actualTypOut))
                    }
                case None => ()
            }
        }

        checkForUnusedStateInitializers(outputContext)

        if (tx.retType.isDefined & !hasReturnStatement(tx, tx.body)) {
            val ast =
                if (tx.body.length > 0) {
                    tx.body.last
                }
                else {
                    tx
                }
            logError(ast, MustReturnError(tx.name))
        }
        else if (!tx.retType.isDefined) {
            // We check for unused ownership errors at each return; if there isn't guaranteed to be one at the end, check separately.
            // Every arg whose output type is owned should be owned at the end.
            val ownedArgs = tx.args.filter((arg: VariableDeclWithSpec) => arg.typOut.isOwned)
            val ownedArgNames = ownedArgs.map((arg: VariableDeclWithSpec) => arg.varName)
            checkForUnusedOwnershipErrors(tx, outputContext, Set("this") ++ ownedArgNames)
        }

        // Check to make sure all the field types are consistent with their declarations.
        checkFieldTypeConsistency(outputContext, tx)

        // todo: check that every declared variable is initialized before use
        tx.copy(body = newStatements)
    }

    private def checkTransaction(tx: Transaction, lexicallyInsideOf: DeclarationTable): Transaction = {
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

                    lexicallyInsideOf.contractTable.state(stateName) match {
                        case Some (t) => t
                        case None => lexicallyInsideOf
                    }
                }
                else {
                    lexicallyInsideOf
                }
            case _ => lexicallyInsideOf
        }

        // Construct the context that the body should start with
        var initContext = Context(table,
                                  new TreeMap[String, ObsidianType](),
                                  isThrown = false,
                                  Set.empty,
                                  Set.empty,
                                  tx.initialFieldTypes,
                                  tx.args.map((v: VariableDeclWithSpec) => v.varName).toSet)
        initContext = initContext.updated("this", thisType)

        // Add all the args first (in an unsafe way) before checking anything
        for (arg <- tx.args) {
            initContext = initContext.updated(arg.varName, arg.typIn)
        }

        checkTransactionArgShadowing(startStates, tx)

        tx.retType match {
            case Some(typ) =>
                typ match {
                    case np: NonPrimitiveType =>
                         if (np.permission == Inferred()) {
                             logError(tx, ReturnTypeMissingPermissionError(np.contractName))
                         }
                    case _ => ()
                }
            case _ => ()
        }

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

    private def checkState(lexicallyInsideOf: ContractTable, state: State): State = {
        val table = lexicallyInsideOf.state(state.name).get
        for (field <- state.fields) {
            checkField(field, table.contractTable, Some(state))
            checkStateFieldShadowing(lexicallyInsideOf, field, state)
        }

        state
    }

    private def checkConstructor(constr: Constructor, table: ContractTable, hasStates: Boolean): Constructor = {
        // maybe this error should be handled in the parser
        if(constr.name != table.name) {
            logError(constr, ConstructorNameError(table.name))
        }

        if (table.contract.isAsset && !constr.resultType.isOwned) {
            logError(constr, AssetContractConstructorError(table.name))
        }


        val stateSet: Set[(String, StateTable)] = table.stateLookup.toSet
        var initContext = Context(table,
                                  new TreeMap[String, ObsidianType](),
                                  isThrown = false,
                                  Set.empty,
                                  Set.empty,
                                  Map.empty,
                                  constr.args.map((v: VariableDeclWithSpec) => v.varName).toSet)

        val thisType = ContractReferenceType(table.contractType, Owned(), false)

        initContext = initContext.updated("this", thisType)

        for (arg <- constr.args) {
            initContext = initContext.updated(arg.varName, arg.typIn)
        }
        // Check as if all the nonprimitive fields were of Inferred permission initially so that we can track ownership correctly.

        for (field <- table.allFields) {
            field.typ match {
                case ContractReferenceType(contractType, permission, isRemote) =>
                    val inferredType = ContractReferenceType(contractType, Inferred(), isRemote)
                    initContext = initContext.updatedThisFieldType(field.name, inferredType)
                case StateType(contractName, stateNames, isRemote) =>
                    val inferredType = ContractReferenceType(ContractType(contractName), Inferred(), isRemote)
                    initContext = initContext.updatedThisFieldType(field.name, inferredType)
                case _ => () // Nothing to do
            }
        }

        val (outputContext, newBody) = checkStatementSequence(constr, initContext, constr.body)

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

        // Check that the arguments meet the correct specification afterwards
        for (arg <- constr.args) {
            outputContext.get(arg.varName) match {
                case Some(actualTypOut) =>
                    val errorOpt = isSubtype(actualTypOut, arg.typOut, false)
                    if (errorOpt.isDefined) {
                        logError(constr, ArgumentSpecificationError(arg.varName, constr.name, arg.typOut, actualTypOut))
                    }
                case None => ()
            }
        }

        val expectedThisType: NonPrimitiveType = constr.resultType
        val thisAST = This().setLoc(constr.loc) // Make a fake "this" AST so we generate the right error message.
        checkIsSubtype(thisAST, outputContext("this"), expectedThisType)

        checkForUnusedOwnershipErrors(constr, outputContext, Set("this"))
        checkForUnusedStateInitializers(outputContext)

        // if the contract contains states, its constructor must contain a state transition
        if (hasStates && !hasTransition(constr.body)) {
            logError(constr, NoStartStateError(constr.name))
        }

        // If there are states, we'll check to make sure the transitions initialize all fields.
        // But if there are no states, we have to check separately.
        if (!hasStates && !outputContext.isThrown) {
            for (field <- table.allFields) {
                if (!outputContext.localFieldsInitialized.contains(field.name)) {
                    logError(constr, UninitializedFieldError(field.name))
                }
            }
        }

        constr.copy(body = newBody)
    }

    private def checkConstructors(constructors: Seq[Constructor], contract: ObsidianContractImpl, table: ContractTable): Unit = {
        if (constructors.isEmpty && table.stateLookup.nonEmpty) {
            logError(contract, NoConstructorError(contract.name))
        }

        // TODO: enable multiple constructors. by adding appropriate dispatch logic in init. For now, we only support one constructor per contract.
        if (constructors.length > 1) {
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

    private def checkDeclaration(table: ContractTable)(decl: Declaration): Declaration = {
        decl match {
            case t: Transaction => checkTransaction(t, table)
            case s: State => checkState(table, s)
            case f: Field => {
                checkContractFieldRepeats(f, table.contract)
                checkField(f, table, None)
            }
            case c: Constructor => checkConstructor(c, table, table.stateLookup.nonEmpty)
            case c: Contract => assert(false, "Contract nesting is no longer supported."); decl
            case d: TypeDecl => assert(false, "Type declarations are not supported yet."); decl
        }
    }

    private def checkContract(contract: Contract): Contract = {

        contract match {
            case obsContract : ObsidianContractImpl =>
                val table = globalTable.contractLookup(obsContract.name)
                val newDecls = obsContract.declarations.map(checkDeclaration(table))

                checkConstructors(table.constructors, obsContract, table)

                obsContract.copy(declarations = newDecls)
            case ffiContract : JavaFFIContractImpl => ffiContract
        }
    }

    // Returns a new program (in a symbol table) with additional type information, paired with errors from the program.
    def checkProgram(): (Seq[ErrorRecord], SymbolTable) = {
        checkForMainContract(globalTable.ast)

        var checkDiffContract = (x: Contract) => x match {
            case obsContract : ObsidianContractImpl => checkContract(obsContract)
            case ffiContract : JavaFFIContractImpl => ffiContract
        }

        val newContracts = globalTable.ast.contracts.map(checkDiffContract)

        (errors, new SymbolTable(new Program(globalTable.ast.imports, newContracts)))
    }
}

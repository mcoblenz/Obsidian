package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.parser._

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.collection.mutable.HashSet


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
                   transitionFieldsDefinitelyInitialized: Set[(String, String, AST)], // stateName, fieldName, AST
                   transitionFieldsMaybeInitialized: Set[(String, String, AST)], // stateName, fieldName, AST
                   localFieldsInitialized: Set[String],
                   thisFieldTypes: Map[String, ObsidianType],
                   valVariables : Set[String]) {
    def keys: Iterable[String] = underlyingVariableMap.keys

    def updated(s: String, t: ObsidianType): Context =
        Context(contractTable,
            underlyingVariableMap.updated(s, t),
            isThrown,
            transitionFieldsDefinitelyInitialized,
            transitionFieldsMaybeInitialized,
            localFieldsInitialized,
            thisFieldTypes,
            valVariables)
    def updatedWithTransitionInitialization(stateName: String, fieldName: String, ast: AST): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsDefinitelyInitialized + ((stateName, fieldName, ast)),
            transitionFieldsMaybeInitialized + ((stateName, fieldName, ast)),
            localFieldsInitialized,
            thisFieldTypes,
            valVariables)

    def updatedAfterTransition(newThisFieldTypes: Map[String, ObsidianType]): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            Set.empty,
            Set.empty,
            localFieldsInitialized,
            newThisFieldTypes,
            valVariables)

    def updatedWithFieldInitialization(fieldName: String): Context = {
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsDefinitelyInitialized,
            transitionFieldsMaybeInitialized,
            localFieldsInitialized + fieldName,
            thisFieldTypes,
            valVariables)
    }

    def updatedThisFieldType(fieldName: String, newType: ObsidianType): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsDefinitelyInitialized,
            transitionFieldsMaybeInitialized,
            localFieldsInitialized,
            thisFieldTypes.updated(fieldName, newType),
            valVariables)

    def removeThisFieldType(fieldName: String): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsDefinitelyInitialized,
            transitionFieldsMaybeInitialized,
            localFieldsInitialized,
            thisFieldTypes - fieldName,
            valVariables)

    def updatedMakingVariableVal(variableName: String): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsDefinitelyInitialized,
            transitionFieldsMaybeInitialized,
            localFieldsInitialized,
            thisFieldTypes,
            valVariables + variableName)

    def updatedClearingValVariable(variableName: String): Context =
        Context(contractTable,
            underlyingVariableMap,
            isThrown,
            transitionFieldsDefinitelyInitialized,
            transitionFieldsMaybeInitialized,
            localFieldsInitialized,
            thisFieldTypes,
            valVariables - variableName)

    def get(s: String): Option[ObsidianType] = underlyingVariableMap.get(s)

    def apply(s: String): ObsidianType = underlyingVariableMap(s)

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

        inType match {
            case np: NonPrimitiveType =>
                // Look up the type in the current scope, NOT with lookupFunction.
                val contractTableOpt = contractTable.lookupContract(np.contractName)
                if (contractTableOpt.isEmpty) {
                    return None
                }

                // Look inside the contract.
                val insideContractResult = contractTableOpt.flatMap(lookupFunction)

                val possibleCurrentStateNames: Iterable[String] = contractTable.possibleStatesFor(np)

                // It's weird that the way we find the available state names depends on the current state; this is an artifact
                // of the fact that lookup for things defined in exactly one state is different from lookup for things defined in more than one state
                // (or the whole contract).
                val availableInStateNames: Iterable[String] = {
                    if (insideContractResult.isDefined) {
                        insideContractResult.get.availableIn match {
                            case None => // This identifier is available in all states of the contract.
                                contractTableOpt.get.stateLookup.keys
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
        thisFieldTypes.get(fieldName).orElse(lookupDeclaredFieldTypeInType(typ)(fieldName))
    }

    def genericParams(typ: ObsidianType): Seq[GenericType] = {
        typ match {
            case np: NonPrimitiveType =>
                val conOpt = if (np.contractName == contractTable.contract.name) {
                    Some(contractTable.contract)
                } else {
                    contractTable.lookupContract(np.contractName).map(_.contract)
                }

                conOpt match {
                    case Some(con) =>
                        con match {
                            case obsCon: ObsidianContractImpl => obsCon.params
                            case javaCon: JavaFFIContractImpl => Nil
                        }
                    case None => Nil
                }

            case _ => Nil
        }
    }

    def actualParams: ObsidianType => Seq[ObsidianType] = {
        case np: NonPrimitiveType => np.contractType.typeArgs
        case _ => Nil
    }

    def lookupDeclaredFieldTypeInType(typ: ObsidianType)(fieldName: String): Option[ObsidianType] = {
        doLookup((declTable: DeclarationTable) => declTable.lookupField(fieldName), typ)
            .map(_.typ)
            .map(_.substitute(genericParams(typ), actualParams(typ)))
    }

    def lookupTransactionInType(typ: ObsidianType)(transactionName: String): Option[Transaction] = {
        typ match {
            case np: NonPrimitiveType =>
                // Look up the type in the current scope, NOT with lookupFunction.
                contractTable.lookupContract(np.contractType)
                    .flatMap(_.lookupTransaction(transactionName))
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
                } else if (np.isOwned) {
                    ConsumingOwnedGivesUnowned()
                } else {
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

    //-------------------------------------------------------------------------
    /* Subtyping definitions */

    def checkArgCompat(table: ContractTable): ((ObsidianType, ObsidianType)) => Boolean = {
        case (np1: NonPrimitiveType, np2: NonPrimitiveType) =>
            if (np1.permission == Inferred()) {
                np1.withTypeState(np2.permission) == np2
            } else if (np2.permission == Inferred()) {
                np2.withTypeState(np1.permission) == np1
            } else {
                isSubtype(table, np1, np2, false).isEmpty
            }
        case (c1Arg, c2Arg) => c1Arg == c2Arg
    }

    /* method to check if contract is actually implementing an interface */
    private def contractIsSubtype(table: ContractTable, c1: ContractType, c2: ContractType, isThis: Boolean): Either[Boolean, Error] = {
        // Everything is a subtype of Top
        if (c2.contractName == ContractType.topContractName) {
            return Left(true)
        }

        val c1Lookup = globalTable.contract(c1.contractName).map(_.contract)
        val c2Lookup = globalTable.contract(c2.contractName).map(_.contract)

        if (c1Lookup.isEmpty) {
            return Right(ContractUndefinedError(c1.contractName))
        }

        if (c2Lookup.isEmpty) {
            return Right(ContractUndefinedError(c2.contractName))
        }

        Left((c1Lookup.get, c2Lookup.get) match {
            case (obs1: ObsidianContractImpl, obs2: ObsidianContractImpl) =>
                if (!obs1.isInterface && obs2.isInterface) {
                    val interfaceMatches = obs1.bound.contractName == obs2.name

                    val subsParams = obs1.bound.substitute(obs1.params, c1.typeArgs).typeArgs
                    val argChecks = subsParams.zip(c2.typeArgs).forall(checkArgCompat(table))

                    interfaceMatches && argChecks
                } else {
                   obs1 == obs2 && c1.typeArgs.zip(c2.typeArgs).forall(checkArgCompat(table))
                }
            case (jvcon1: JavaFFIContractImpl, jvcon2: JavaFFIContractImpl) => jvcon1 == jvcon2
            case (obs: ObsidianContractImpl, jvcon: JavaFFIContractImpl) => false
            case (jvcon: JavaFFIContractImpl, obs:ObsidianContractImpl) => obs.name == jvcon.interface
        })
    }

    private def typeBound(table: ContractTable): ObsidianType => ObsidianType = {
        case np: NonPrimitiveType => np match {
            case GenericType(gVar, bound) => bound.referenceType
            case t => t
        }

        case t => t
    }

    /* true iff [t1 <: t2] */
    private def isSubtype(table: ContractTable, t1: ObsidianType, t2: ObsidianType, isThis: Boolean): Option[Error] = {
        val isSubtypeRes = (typeBound(table)(t1), typeBound(table)(t2)) match {
            case (BottomType(), _) => true
            case (_, BottomType()) => true
            case (IntType(), IntType()) => true
            case (BoolType(), BoolType()) => true
            case (StringType(), StringType()) => true
            case (np1: NonPrimitiveType, np2: NonPrimitiveType) =>
                (np1, np2) match {
                    case (ContractReferenceType(c1, c1p, _), ContractReferenceType(c2, c2p, _)) =>
                        contractIsSubtype(table, c1, c2, isThis) match {
                            case Left(isSubtypeRes) => isSubtypeRes && isSubpermission(c1p, c2p)
                            case Right(err) => return Some(err)
                        }
                    case (StateType(c1, ss1, _), StateType(c2, ss2, _)) =>
                        contractIsSubtype(table, c1, c2, isThis) match {
                            case Left(isSubtypeRes) => isSubtypeRes && ss1.subsetOf(ss2)
                            case Right(err) => return Some(err)
                        }
                    case (StateType(c1, ss1, _), ContractReferenceType(c2, c2p, _)) =>
                        contractIsSubtype(table, c1, c2, isThis) match {
                            case Left(isSubtypeRes) => isSubtypeRes
                            case Right(err) => return Some(err)
                        }
                    case _ => false
                }

            case (p: PrimitiveType, np: NonPrimitiveType) => np.contractName == ContractType.topContractName

            case _ => false
        }

        if (isSubtypeRes) {
            None
        } else {
            Some(SubtypingError(t1, t2, isThis))
        }
    }

    /* returns [t1] if [t1 <: t2], logs an error and returns [BottomType] otherwise */
    private def checkIsSubtype(table: ContractTable, ast: AST, t1: ObsidianType, t2: ObsidianType, thisAtEndOfTransaction: Boolean = false): ObsidianType = {
        val errorOpt = isSubtype(table, t1, t2, thisAtEndOfTransaction)
        if (errorOpt.isDefined) {
            logError(ast, errorOpt.get)
            BottomType()
        } else {
            t1
        }
    }


    // returns true iff [p1 <: p2]
    private def isSubpermission(p1: Permission, p2: Permission): Boolean = {
        p1 match {
            case Owned() => true
            case Unowned() => (p2 == Unowned()) || (p2 == Inferred())
            case Shared() => (p2 == Shared()) || (p2 == Unowned()) || (p2 == Inferred())
            case Inferred() => true
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
                        case Unowned() => if (p2 == Shared()) Some(t1) else typeForMismatchedPermissions
                        case Shared() => if (p2 == Unowned()) Some(t2) else typeForMismatchedPermissions
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
            case (StateType(ct, ss1, _), StateType(_, ss2, _)) =>
                val unionStates = ss1.union(ss2)
                Some(StateType(ct, unionStates, NotRemoteReferenceType()))

            case (g1@GenericType(gVar1, gBound1), g2@GenericType(gVar2, gBound2)) =>
                if (g1 == g2) {
                    Some(g1)
                }
                else if (gVar1.permissionVar.isDefined && gVar2.permissionVar.isDefined) {
                    Some(t1)
                } else {
                    val typeForMismatchedPermissions =
                        if (t1.isAssetReference(contractTable) != No()) {
                            None
                        }
                        else {
                            Some(t1.withTypeState(Unowned()))
                        }

                    g1.permission match {
                        case Owned() => typeForMismatchedPermissions
                        case Unowned() => if (g2.permission == Shared()) Some(t1) else typeForMismatchedPermissions
                        case Shared() => if (g2.permission == Unowned()) Some(t2) else typeForMismatchedPermissions
                        case Inferred() => assert(assertion = false, "Inferred types should be removed"); None
                    }
                }

            case (g1: GenericType, np2: NonPrimitiveType) =>
                if (g1.bound.interfaceType.contractName == np2.contractName) {
                    nonPrimitiveMergeTypes(g1.bound.referenceType, np2, contractTable)
                } else {
                    None
                }
            case (np1: NonPrimitiveType, g2: GenericType) =>
                nonPrimitiveMergeTypes(g2, np1, contractTable)

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

    def checkGenericArgLength(table: ContractTable, ast: AST, actualArg: ObsidianType): Unit = {
        actualArg match {
            case np: NonPrimitiveType =>
                table.lookupContract(np.contractName) match {
                    case Some(t) =>
                        t.contract match {
                            case obsCon: ObsidianContractImpl =>
                                if (obsCon.params.length != np.genericParams.length) {
                                    logError(ast, GenericParameterListError(obsCon.params.length, np.genericParams.length))
                                }
                            case javaCon: JavaFFIContractImpl => ()
                        }
                    // This will be an error somewhere else
                    case None => ()
                }
            case _ => ()
        }
    }

    def substituteOk(table: ContractTable, ast: AST, params: Seq[GenericType], typeArgs: Seq[ObsidianType]): Unit = {
        if (params.length != typeArgs.length) {
            logError(ast, GenericParameterListError(params.length, typeArgs.length))
        }

        for (((param, idx), actualArg) <- params.zipWithIndex.zip(typeArgs)) {
            checkGenericArgLength(table, ast, actualArg)

            // Here we want to do the substitution before we check, for example, in the following case:
            // Foo[T implements Consumer[U], U]
            val otherParams = params.take(idx) ++ params.drop(idx + 1)
            val otherArgs = typeArgs.take(idx) ++ typeArgs.drop(idx + 1)
            val substitutedParam = param.substitute(otherParams, otherArgs)

            if (actualArg.isAssetReference(table) != No() && substitutedParam.isAssetReference(table) == No()) {
                logError(ast, GenericParameterAssetError(param.gVar.varName, actualArg.baseTypeName))
            }

            if (isSubtype(table, actualArg, substitutedParam, isThis = false).isDefined) {
                logError(ast, GenericParameterError(param, actualArg))
            }

            actualArg match {
                case nonPrimitiveActualArg: NonPrimitiveType =>
                    if (nonPrimitiveActualArg.permission == Inferred() && param.gVar.permissionVar.isDefined) {
                        logError(ast, GenericParameterPermissionMissingError(param, param.gVar.permissionVar.get, nonPrimitiveActualArg))
                    }
                case _ =>
                    // Nothing to do here.
            }
        }
    }

    private def inferAndCheckExpr(decl: InvokableDeclaration,
                                  context: Context,
                                  e: Expression,
                                  ownershipConsumptionMode: OwnershipConsumptionMode): (ObsidianType, Context, Expression) = {

        /* returns [t] if [e : t], otherwise returns BottomType */
        def assertTypeEquality(e: Expression, t: ObsidianType, c: Context): (ObsidianType, Context, Expression) = {
            val (tPrime, contextPrime, ePrime) = inferAndCheckExpr(decl, c, e, NoOwnershipConsumption())
            (checkIsSubtype(contextPrime.contractTable, e, tPrime, t), contextPrime, ePrime)
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
                params: Seq[ObsidianType],
                args: Seq[Expression]): (ObsidianType, Context, Boolean, Expression, Seq[GenericType], Seq[Expression]) = {
            val (receiverType, contextAfterReceiver, receiverPrime) = inferAndCheckExpr(decl, context, receiver, NoOwnershipConsumption())

            // Terrible special case just for now. TODO: remove this.
            if (name == "sqrt" && args.length == 1) {
                // Int isn't really right either, but it will have to do for now.
                return (IntType(), context, false, receiverPrime, Nil, args)
            }

            // Eliminate things we can't invoke methods on first.
            val nonPrimitiveReceiverType = receiverType match {
                case BottomType() => return (BottomType(), contextAfterReceiver, false, receiverPrime, Nil, args)
                case UnitType() | IntType() | BoolType() | StringType() =>
                    logError(e, NonInvokeableError(receiverType))
                    return (BottomType(), contextAfterReceiver, false, receiverPrime, Nil, args)
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


            val foundTransaction =
                contextAfterReceiver.lookupTransactionInType(receiverType)(name)
                .map(t => t.substitute(t.params, params))

            val invokable: InvokableDeclaration = foundTransaction match {
                case None =>
                    val err = MethodUndefinedError(nonPrimitiveReceiverType, name)
                    logError(e, err)
                    return (BottomType(), contextAfterReceiver, isFFIInvocation, receiverPrime, Nil, args)
                case Some(t) => t
            }

            if (receiverType.isInstanceOf[InterfaceContractType] && !foundTransaction.get.isStatic) {
                val err = NonStaticAccessError(foundTransaction.get.name, receiver.toString)
                logError(e, err)
                return (BottomType(), contextAfterReceiver, isFFIInvocation, receiverPrime, foundTransaction.get.params, args)
            }

            if (!invokable.isStatic && isSubtype(context.contractTable, receiverType, invokable.thisType, receiver.isInstanceOf[This]).isDefined) {
                logError(e, ReceiverTypeIncompatibleError(name, receiverType, invokable.thisType))
            }

            // Check field type preconditions for overrides in private invocations
            for ((fieldName, requiredInitialFieldType) <- foundTransaction.get.initialFieldTypes) {
                val currentFieldType = context.lookupCurrentFieldTypeInThis(fieldName)
                currentFieldType match {
                   case None => ()
                   case Some(cft) => if(isSubtype(context.contractTable, cft, requiredInitialFieldType, receiver.isInstanceOf[This]).isDefined ||
                                        (cft.isOwned != requiredInitialFieldType.isOwned && !cft.isBottom)) {
                       logError(e, FieldSubtypingError(fieldName, cft, requiredInitialFieldType))
                   }
                }
            }

            // check arguments
            val spec = invokable.args
            val specList = (spec, invokable)::Nil

            val (exprSequence, contextAfterArgs, correctInvokable) =
                checkArgs(e, contextAfterReceiver, specList, args) match {
                    case None => return (BottomType(), contextAfterReceiver, isFFIInvocation, receiverPrime, foundTransaction.get.params, args)
                    case Some(x) => x
                }

            val resultType = correctInvokable.retType match {
                case None => UnitType()
                case Some(typ) => typ
            }

            val contextPrime =
                correctInvokable match {
                    case t: Transaction =>
                        updateArgTypeInContext(receiver, receiverType, t.thisType, t.thisFinalType, contextAfterArgs)
                    case _ => contextAfterArgs
                }

            // Update field types if we invoked a private method.
            val contextAfterPrivateInvocation = updateFieldsForPrivateInvocation(contextPrime, foundTransaction.get)

            (resultType, contextAfterPrivateInvocation, isFFIInvocation, receiverPrime, foundTransaction.get.params, exprSequence)
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
                         if (tableLookup.isDefined) {
                             val contractTable = tableLookup.get
                             val nonPrimitiveType = ContractReferenceType(contractTable.contractType, Shared(), NotRemoteReferenceType())
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
                 val (typ, con, ePrime) = assertTypeEquality(e, BoolType(), context)
                 (typ, con, LogicalNegation(ePrime).setLoc(e))
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
             case Mod(e1: Expression, e2: Expression) =>
                 val (typ, con, e1Prime, e2Prime) = assertOperationType(e1, e2, IntType())
                 (typ, con, Mod(e1Prime, e2Prime).setLoc(e))
             case Negate(e: Expression) =>
                 assertTypeEquality(e, IntType(), context)
             case Equals(e1: Expression, e2: Expression) =>
                 val (t1, c1, e1Prime) = inferAndCheckExpr(decl, context, e1, NoOwnershipConsumption())
                 val (t2, c2, e2Prime) = inferAndCheckExpr(decl, c1, e2, NoOwnershipConsumption())
                 if (t1 == t2) (BoolType(), c2, Equals(e1Prime, e2Prime).setLoc(e)) else {
                     if (!t1.isBottom && !t2.isBottom) {
                         logError(e, DifferentTypeError(e1, t1, e2, t2))
                     }
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
                     if (!t1.isBottom && !t2.isBottom) {
                         logError(e, DifferentTypeError(e1, t1, e2, t2))
                     }
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
                                     }
                                     (ePrime, c)
                             }
                         (BottomType(), newContext, newExpr)
                 }

             case LocalInvocation(name, _, params, args: Seq[Expression]) =>
                 val (typ, con, _, _, newGenericParams, newArgs) = handleInvocation(context, name, This().setLoc(e), params, args)
                 //This may need correction.
                 (typ, con, LocalInvocation(name, newGenericParams, params, newArgs).setLoc(e))

             case Invocation(receiver: Expression, _, params, name, args: Seq[Expression], isFFIInvocation) =>
                 val (typ, con, isFFIInv, newReceiver, newGenericParams, newArgs) = handleInvocation(context, name, receiver, params, args)
                 (typ, con, Invocation(newReceiver, newGenericParams, params, name, newArgs, isFFIInv).setLoc(e))

             case c@Construction(contractType, args: Seq[Expression], isFFIInvocation) =>
                 val tableLookup = context.contractTable.lookupContract(contractType.contractName)
                 val isFFIInv = tableLookup match {
                     case None => false
                     case Some(x) => x.contract match {
                         case obsContract: ObsidianContractImpl => false
                         case javaContract: JavaFFIContractImpl => true
                     }
                 }

                 if (tableLookup.isEmpty) {
                     logError(e, ContractUndefinedError(contractType.contractName))
                     return (BottomType(), context, e)
                 }

                 if (tableLookup.get.contract.isInterface) {
                    logError(e, InterfaceInstantiationError(contractType.contractName))
                 }

                 tableLookup.get.contract match {
                     case obsCon: ObsidianContractImpl =>
                         substituteOk(tableLookup.get, c, obsCon.params, contractType.typeArgs)
                     case javaCon: JavaFFIContractImpl => ()
                 }

                 val ctTableOfConstructed = tableLookup.get.substitute(contractType.typeArgs)

                 val constrSpecs = ctTableOfConstructed
                                    .constructors
                                    .map(constr => (constr.args, constr))

                 val result = checkArgs(e, context, constrSpecs, args)

                 val (exprList, simpleType, contextPrime) = result match {
                     // Even if the args didn't check, we can still output a type
                     case None => (Nil, ContractReferenceType(contractType, Owned(), NotRemoteReferenceType()), context)
                     case Some((newExprSequence, cntxt, constr)) =>
                         val outTyp = constr.asInstanceOf[Constructor].resultType match {
                             case ContractReferenceType(_, permission, isRemote) =>
                                 ContractReferenceType(contractType, permission, isRemote)
                             case StateType(_, stateNames, isRemote) =>
                                 StateType(contractType, stateNames, isRemote)
                             case t => t
                         }
                         (newExprSequence, outTyp, cntxt)
                 }

                 (simpleType, contextPrime, Construction(contractType, exprList, isFFIInv))

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
                     case ReferenceIdentifier(x) =>
                         if (contextPrime.lookupCurrentFieldTypeInThis(x).isDefined) {
                             // This is a field.
                             contextPrime.updatedThisFieldType(x, newTyp)
                         }
                         else {
                             contextPrime.updated(x, newTyp)
                         }
                     case _ => contextPrime
                 }
                 (newTyp, finalContext, ePrime)
             case StateInitializer(stateName, fieldName) =>
                 // A state initializer expression has its field's type.
                 if (!context.transitionFieldsDefinitelyInitialized.exists(
                     {
                         case (stateNameInitialized, fieldNameInitialized, _) =>
                             (stateName._1 == stateNameInitialized && fieldName._1 == fieldNameInitialized)
                     }))
                     {
                         logError(e, StateInitializerUninitialized(stateName._1, fieldName._1))
                     }

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
    private def hasReturnStatement(statements: Seq[Statement]) : Boolean = {
        var hasRet = false

        for (statement <- statements) {
            if (hasRet) {
                logError(statement, UnreachableCodeError())
                return hasRet
            }

            statement match {
                case Return() | ReturnExpr(_) | Revert(_) => hasRet = true
                case IfThenElse(_, s1, s2) =>
                    hasRet = hasReturnStatement(s1) && hasReturnStatement(s2)
                case IfInState(e, ePerm, state, s1, s2) =>
                    hasRet = hasReturnStatement(s1) && hasReturnStatement(s2)
                case Switch(e, cases) =>
                    hasRet = cases.forall(aCase => hasReturnStatement(aCase.body))
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
        for (remainingInitialization <- context.transitionFieldsMaybeInitialized) {
            logError(remainingInitialization._3, InvalidStateFieldInitialization(remainingInitialization._1, remainingInitialization._2))
        }
    }

    // Removes any field types from thisFieldTypes that are already consistent with their declarations.
    private def packFieldTypes(context: Context): Context = {
        var resultContext = context
        for ((field, typ) <- context.thisFieldTypes) {
            val requiredFieldType = context.lookupDeclaredFieldTypeInThis(field)

            requiredFieldType match {
                case None =>
                    // The field doesn't exist, so there would have been another error generated previously.
                    // Nothing to do here.
                case Some(declaredFieldType) =>
                    if (isSubtype(context.contractTable, typ, declaredFieldType, false).isEmpty &&
                        (typ.isOwned == declaredFieldType.isOwned || declaredFieldType.isBottom)) {
                        resultContext = resultContext.removeThisFieldType(field)
                    }
            }
        }

        resultContext
    }

    private def checkFieldTypeConsistencyAfterTransaction(context: Context, tx: InvokableDeclaration, exitLocation: AST): Unit = {
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
                    // Nothing to do; there was an assignment to a field type that may not be in scope, so there will be a separate error message.
                case Some(declaredFieldType) =>
                    if (isSubtype(context.contractTable, typ, declaredFieldType, false).isDefined || (typ.isOwned != declaredFieldType.isOwned && !declaredFieldType.isBottom)) {
                        logError(exitLocation, InvalidInconsistentFieldType(field, typ, declaredFieldType))
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
                        if (isSubtype(context.contractTable, currentTyp, requiredTyp, false).isDefined) {
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

        // Anything we threw out (relative to the branch context) needs to be checked for non-disposable references.
        val discardedVariables = branchContext.keys.toSet -- newContext.keys.toSet
        for (discardedVariable <- discardedVariables) {
            errorIfNotDisposable(discardedVariable, branchContext.get(discardedVariable).get, branchContext, ast)
        }

        Context(oldContext.contractTable,
            newContext.underlyingVariableMap,
            isThrown = branchContext.isThrown,
            branchContext.transitionFieldsDefinitelyInitialized,
            branchContext.transitionFieldsMaybeInitialized,
            branchContext.localFieldsInitialized,
            branchContext.thisFieldTypes,
            branchContext.valVariables)
    }

    private def errorIfNotDisposable(variable: String, typ: ObsidianType, context: Context, ast: AST): Unit = {
        typ match {
            case t: NonPrimitiveType =>
                if (t.isOwned && t.isAssetReference(context.contractTable) != No() && t.remoteReferenceType != TopLevelRemoteReferenceType()) {
                    logError(ast, UnusedOwnershipError(variable))
                }
            case _ => ()
        }
    }

    // If a context exits, then allow discrepancies between variables, and take the version from the OTHER context.
    // If both contexts exit, and there is a discrepancy, just use the version from context 1, since the code below will be unreachable anyway.
    private def mergeContext(
            ast: AST,
            context1: Context,
            context2: Context,
            context1Exits: Boolean,
            context2Exits: Boolean): Context = {
        /* If we're merging with a context from a "throw", just take the other context
        * emit no errors */
        assert(context1.contractTable == context2.contractTable)

        def mergeMaps(map1: Map[String, ObsidianType],
                      map2: Map[String, ObsidianType],
                      dropDisposableReferences: Boolean): Map[String, ObsidianType] = {
            var mergedMap = new TreeMap[String, ObsidianType]()

            val inBoth = map1.keys.toSet.intersect(map2.keys.toSet)

            for (x <- inBoth) {
                val t1 = map1(x)
                val t2 = map2(x)
                if (context2Exits) {
                    mergedMap = mergedMap.updated(x, t1)
                }
                else if (context1Exits) {
                    mergedMap = mergedMap.updated(x, t2)
                }
                else if (context1Exits && context2Exits) {
                    mergedMap = mergedMap.updated(x, t1)
                }
                else mergeTypes(t1, t2, context1.contractTable) match {
                    case Some(u) => mergedMap = mergedMap.updated(x, u)
                    case None =>
                        if (!t1.isBottom && !t2.isBottom) {
                            logError(ast, MergeIncompatibleError(x, t1, t2))
                        }
                        // Otherwise, one is BottomType, so we can ignore the failure to merge for now.
                }
            }

            // Make sure anything that is not in both is disposable (if disposing of them is allowed; otherwise error).
            if (!context1Exits) {
                val inOnlyContext1 = map1.keys.toSet -- inBoth

                if (dropDisposableReferences) {
                    inOnlyContext1.foreach((x: String) => errorIfNotDisposable(x, map1(x), context1, ast))
                }
                else {
                    inOnlyContext1.foreach((x: String) => logError(ast, FieldTypesIncompatibleAcrossBranches(x, map1(x), "true")))
                }
            }
            if (!context2Exits) {
                val inOnlyContext2 = map2.keys.toSet -- inBoth

                if (dropDisposableReferences) {
                    inOnlyContext2.foreach((x: String) => errorIfNotDisposable(x, map2(x), context2, ast))
                }
                else {
                    inOnlyContext2.foreach((x: String) => logError(ast, FieldTypesIncompatibleAcrossBranches(x, map2(x), "false")))
                }
            }

            mergedMap
        }

        val packedContext1 = packFieldTypes(context1)
        val packedContext2 = packFieldTypes(context2)

        val mergedVariableMap = mergeMaps(packedContext1.underlyingVariableMap, packedContext2.underlyingVariableMap, true)
        val mergedThisFieldMap = mergeMaps(packedContext1.thisFieldTypes, packedContext2.thisFieldTypes, false)

        // This is an intersect operation that only depends on the state and field names, not on the ASTs.
        def mergeTransitionFieldsInitialized(fields1: Set[(String, String, AST)], fields2: Set[(String, String, AST)]) : Set[(String, String, AST)] = {
            var resultingFields = Set[(String, String, AST)]()
            for (fieldInitialized <- fields1) {
                if (fields2.exists( (otherFieldInitialized: (String, String, AST)) =>
                    otherFieldInitialized._1 == fieldInitialized._1 &&
                    otherFieldInitialized._2 == fieldInitialized._2))
                {
                    resultingFields = resultingFields + fieldInitialized
                }
            }
            resultingFields
        }

        val mergedLocalFieldsInitialized = (context1Exits, context2Exits) match {
            case (false, false) => packedContext1.localFieldsInitialized.intersect(packedContext2.localFieldsInitialized)
            case (true, false) => packedContext2.localFieldsInitialized
            case (false, true) => packedContext1.localFieldsInitialized
            case (true, true) => packedContext1.localFieldsInitialized.intersect(packedContext2.localFieldsInitialized) // not sure what's right here
        }

        Context(context1.contractTable,
            mergedVariableMap,
            packedContext1.isThrown,
            mergeTransitionFieldsInitialized(packedContext1.transitionFieldsDefinitelyInitialized, packedContext2.transitionFieldsDefinitelyInitialized),
            packedContext1.transitionFieldsMaybeInitialized.union(packedContext2.transitionFieldsMaybeInitialized),
            mergedLocalFieldsInitialized,
            mergedThisFieldMap,
            packedContext1.valVariables.intersect(packedContext2.valVariables))
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

                val nameToUpdate = arg match {
                    case ReferenceIdentifier(x) => Some(x)
                    case This() => Some("this")
                    case _ => None
                }

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

                val (argType, contextAfterArg, e) = inferAndCheckExpr(decl, contextAfterArgs, arg, NoOwnershipConsumption())

                val contextAfterOwnershipTransfer = specInputType match {
                    case npSpecInputType: NonPrimitiveType =>
                        specOutputType match {
                            case npSpecOutputType: NonPrimitiveType => updateArgTypeInContext(e, argType, npSpecInputType, npSpecOutputType, contextAfterArg)
                            case _ => // There should have been a prior error about a bogus type declaration.
                                contextAfterArg
                        }

                    case _ => contextAfterArg
                }
                if (isSubtype(context.contractTable, argType, specInputType, false).isDefined) {
                    val err = ArgumentSubtypingError(decl.name, spec(i).varName, argType, specInputType)
                    errList = (ast, err)::errList
                }

                contextAfterArgs = contextAfterOwnershipTransfer
                expressionList = expressionList :+ e
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

    private def updateArgumentTypeInInvocation(arg: Expression,
                                               passedType: NonPrimitiveType,
                                               initialType: NonPrimitiveType,
                                               finalType: NonPrimitiveType,
                                               referenceIdentifier: String,
                                               context: Context): Context = {
        if ((passedType.isOwned || passedType.permission == Shared()) && initialType.permission == Unowned()) {
            // Special exception: passing owned to Unowned or to Shared does not lose ownership.
            context
        }
        else {
            if (passedType.isOwned && initialType.permission == Shared() && !finalType.isOwned &&
                passedType.isAssetReference(context.contractTable) != No() && passedType.remoteReferenceType != TopLevelRemoteReferenceType()) {
                // Special case: passing an owned reference to a Shared >> Unowned arg will make the arg Unowned but also lose ownership!
                logError(arg, LostOwnershipErrorDueToSharing(arg))
            }

            if (context.get(referenceIdentifier).isDefined) {
                // This was a local variable.
                context.updated(referenceIdentifier, passedType.withTypeState(finalType.typeState))
            }
            else {
                // This was a field or a static invocation. If it was a field, update the field's type.
                val currentFieldType = context.lookupCurrentFieldTypeInThis(referenceIdentifier)

                if (currentFieldType.isDefined && currentFieldType.get != finalType) {
                    context.updatedThisFieldType(referenceIdentifier, passedType.withTypeState(finalType.typeState))
                }
                else {
                    // No need to update anything for static invocations.
                    context
                }
            }
        }

    }

    // updates the type of an identifier in the context based on the transaction invoked on it
    private def updateArgTypeInContext(arg: Expression,
                                       argType: ObsidianType,
                                       declaredInitialType: NonPrimitiveType,
                                       declaredFinalType: NonPrimitiveType,
                                       context: Context): Context = {
        val nameToUpdate = arg match {
            case ReferenceIdentifier(x) => Some(x)
            case This() => Some("this")
            case _ =>
                // If the argument isn't bound to a variable but owns an asset, and this call is not going to consume ownership, then error.
                if (argType.isOwned && argType.isAssetReference(context.contractTable) != No() &&
                    (declaredFinalType.isOwned || !declaredInitialType.isOwned) &&
                    argType.remoteReferenceType != TopLevelRemoteReferenceType())
                {
                    if (declaredInitialType.permission == Shared()) {
                        logError(arg, LostOwnershipErrorDueToSharing(arg))
                    }
                    else {
                        logError(arg, UnusedExpressionArgumentOwnershipError(arg))
                    }
                }
                None
        }

        nameToUpdate match {
            case Some(name) =>
                argType match {
                    case npArgType: NonPrimitiveType => updateArgumentTypeInInvocation(arg, npArgType, declaredInitialType, declaredFinalType, name, context)
                    case _ => context
                }
            case None => context
        }
    }

    def contractTypeCompat(table: ContractTable, ct1: ContractType, ct2: ContractType): Boolean =
        ct1.contractName == ct2.contractName && ct1.typeArgs.zip(ct2.typeArgs).forall(checkArgCompat(table))

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

                    // This should be just on the contract name/params, since assignment is allowed to change permission
                    if (!contractTypeCompat(context.contractTable, exprNPType.contractType, variableNPType.contractType)) {
                        logError(s, InconsistentContractTypeError(variableNPType.contractType, exprNPType.contractType))
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
                    checkIsSubtype(context.contractTable, s, exprType, variableType)
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
                if (context.get(name).isDefined) {
                    logError(s, VariableShadowingDisallowed(name))
                    (context, s)
                }
                else {
                    val (exprType, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, ConsumingOwnedGivesUnowned())
                    val declaredType = typ match {
                        case np: NonPrimitiveType =>
                            val declaredContractName = np.contractName
                            val tableLookup = context.contractTable.lookupContract(declaredContractName)

                            val resolvedType = tableLookup match {
                                case None =>
                                    logError(s, ContractUndefinedError(declaredContractName))
                                    BottomType()
                                case Some(table) =>
                                    checkGenericArgLength(table, s, np)

                                    exprType match {
                                        case exprNonPrimitiveType: NonPrimitiveType =>
                                            if (isSubtype(table, exprNonPrimitiveType, np, isThis = false).isEmpty) {
                                                // We only want the permission/states from the expr, not the whole thing
                                                val tempTyp = np.withTypeState(exprNonPrimitiveType.permission)
                                                exprNonPrimitiveType match {
                                                    case stateType: StateType => tempTyp.withTypeState(States(stateType.stateNames))
                                                    case GenericType(gVar, bound) => bound match {
                                                        case perm: GenericBoundPerm =>
                                                            tempTyp
                                                        case states: GenericBoundStates =>
                                                            tempTyp.withTypeState(States(states.states))
                                                    }
                                                    case _ => tempTyp
                                                }
                                            } else {
                                                logError(s, InconsistentContractTypeError(np.contractType, exprNonPrimitiveType.contractType))
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
                                    logError(s, InvalidLocalVariablePermissionDeclarationError())
                                    BottomType()
                            }
                        case BottomType() => BottomType()
                        case _ =>
                            checkIsSubtype(context.contractTable, s, exprType, typ)
                            typ
                    }

                    (contextPrime.updated(name, declaredType), VariableDeclWithInit(typ, name, ePrime))
                }
            case Return() =>
                checkFieldTypeConsistencyAfterTransaction(context, decl, s)

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
                checkFieldTypeConsistencyAfterTransaction(contextPrime, decl, s)


                if (retTypeOpt.isDefined && !retTypeOpt.get.isBottom) {
                    checkIsSubtype(contextPrime.contractTable, s, typ, retTypeOpt.get)
                }
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

                val possibleCurrentStates = thisTable.possibleStatesFor(oldType)

                def fieldsAvailableInState(stateName: String): Set[Field] = {
                    val allFields = thisTable.allFields
                    allFields.filter((f: Field) =>
                        f.availableIn.isEmpty || f.availableIn.get.contains(stateName))
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

                val updatedViaAssignment: Set[String] = context.transitionFieldsDefinitelyInitialized.filter(testStateMatch).map(_._2)


                val updatedViaAssignmentToWrongState = context.transitionFieldsMaybeInitialized.filterNot(testStateMatch)
                for (invalidAssignment <- updatedViaAssignmentToWrongState) {
                    logError(invalidAssignment._3, InvalidStateFieldInitialization(invalidAssignment._1, invalidAssignment._2))
                }
                for (updateInAssignment <- updatedViaAssignment) {
                    // Make sure these don't overwrite owned references.
                    val currentFieldType = context.lookupCurrentFieldTypeInThis(updateInAssignment).getOrElse(BottomType())
                    if (currentFieldType.isAssetReference(thisTable) != No() && currentFieldType.isOwned) {
                        logError(s, StateInitializerOverwritesOwnership(updateInAssignment))
                    }
                }

                // Give an error for fields that are initialized BOTH in this transition AND in an assignment statement.
                for (redundantlyInitializedField <- updatedInTransition.intersect(updatedViaAssignment)) {
                    logError(s, RedundantFieldInitializationError(redundantlyInitializedField))
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
                var newUpdatesOption: Option[Seq[(ReferenceIdentifier, Expression)]] = None

                // First transform the context according to all the update expressions. Afterward, update field types.

                if (updates.isDefined) {
                    var newUpdates = Seq.empty[(ReferenceIdentifier, Expression)]

                    for ((ReferenceIdentifier(f), e) <- updates.get) {
                        // Check to make sure we're not going to lose an owned asset by overwriting it
                        val currentFieldType = contextPrime.lookupCurrentFieldTypeInThis(f).getOrElse(BottomType())
                        if (currentFieldType.isAssetReference(thisTable) != No() && currentFieldType.isOwned) {
                            logError(s, OverwrittenOwnershipError(f))
                        }

                        // Check to make sure the new value is type-appropriate for the field's declaration
                        val fieldAST = newStateTable.lookupField(f)
                        if (fieldAST.isDefined) {
                            val (t, contextPrime2, ePrime) = inferAndCheckExpr(decl, contextPrime, e, consumptionModeForType(fieldAST.get.typ))
                            newUpdates = newUpdates :+ (ReferenceIdentifier(f), ePrime)
                            contextPrime = contextPrime2
                            checkIsSubtype(contextPrime.contractTable, s, t, fieldAST.get.typ)
                        }
                    }
                    newUpdatesOption = Some(newUpdates)
                }

                // Check for potentially-dropped resources.
                // For each state that we might be in, compute the set of fields that could be available.
                val possibleCurrentFields: Set[Field] =
                if (possibleCurrentStates.isEmpty) {
                    Set.empty[Field]
                }
                else {
                    // All the fields from all the possible states, BUT if this is a constructor, some of those
                    // fields will not have been initialized yet, so that's fine.
                    val fieldsFromPossibleStates = possibleCurrentStates.map(fieldsAvailableInState).reduce(
                        (s1: Set[Field], s2: Set[Field]) => s1 union s2)
                    val uninitializedConstructorFieldNames = context.thisFieldTypes.filter( (field: (String, ObsidianType)) =>
                        field._2 match {
                            case np: NonPrimitiveType => np.permission == Inferred()
                            case _ => false
                        }).keys
                    fieldsFromPossibleStates.filter( (f: Field) => !uninitializedConstructorFieldNames.exists(f.name.equals))
                }
                val toCheckForDroppedAssets = possibleCurrentFields -- newFields // fields that we might currently have minus fields we're initializing now
                for (oldField <- toCheckForDroppedAssets) {
                    val fieldType = contextPrime.thisFieldTypes.getOrElse(oldField.name, oldField.typ)
                    if (fieldType.isAssetReference(thisTable) != No() && fieldType.isOwned) {
                        logError(s, PotentiallyUnusedOwnershipError(oldField.name))
                    }
                }

                // Compute new this-field types.
                // Discard records for any this-field types that are not in the new state.
                val newFieldNames = newFields.map((f: Field) => f.name)
                var newThisFieldTypes = contextPrime.thisFieldTypes.filter((fieldType: (String, ObsidianType)) => newFieldNames.contains(fieldType._1))

                // Discard records for any this-field types that we're about to assign to.
                for ((ReferenceIdentifier(f), e) <- updates.getOrElse(Seq())) {
                    newThisFieldTypes = newThisFieldTypes - f
                }
                // Discard records for any this-field types for which there were initializers (S::x = ...)
                newThisFieldTypes = newThisFieldTypes -- updatedViaAssignment


                val newTypeTable = thisTable.contractTable.state(newStateName).get
                val newSimpleType =
                    if (oldType.isOwned) {
                        StateType(thisTable.contractType, newStateName, NotRemoteReferenceType())
                    }
                    else {
                        // If the old "this" was unowned, we'd better not steal ownership for ourselves here.
                        oldType
                    }

                // Any initialization checking that will happen later can assume all the state fields have been initialized.
                var contextAfterTransition = contextPrime.updated("this", newSimpleType).updatedAfterTransition(newThisFieldTypes)
                if (updates.isDefined) {
                    for (update <- updates.get) {
                        contextAfterTransition = contextAfterTransition.updatedWithFieldInitialization(update._1.name)
                    }
                }
                for (assignmentUpdate <- updatedViaAssignment) {
                    contextAfterTransition = contextAfterTransition.updatedWithFieldInitialization(assignmentUpdate)
                }

                (contextAfterTransition, Transition(newStateName, newUpdatesOption, context.thisType.permission).setLoc(s))

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
                val declaredFieldType = stateOption match {
                    case None => logError(s, StateUndefinedError(context.contractTable.name, stateName._1)); BottomType()
                    case Some(stateTable) =>
                        stateTable.lookupField(fieldIdentifier._1) match {
                            case None => logError(s, FieldUndefinedError(stateTable.nonPrimitiveType, fieldIdentifier._1)); BottomType()
                            case Some(field) => field.typ
                        }
                }

                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, consumptionModeForType(declaredFieldType))

                // If there might have been a prior initialization, make sure this initialization doesn't discard a
                // nondisposable reference.
                if (declaredFieldType.isOwned && declaredFieldType.isAssetReference(context.contractTable) != No() &&
                    context.transitionFieldsMaybeInitialized.exists({ case (sn, fn, _) => sn == stateName._1 && fn == fieldIdentifier._1})) {
                    logError(s, OverwrittenOwnershipError(fieldIdentifier._1))
                }

                checkIsSubtype(contextPrime.contractTable, s, t, declaredFieldType)
                if (declaredFieldType == BottomType()) {
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
                            checkIsSubtype(exprContext.contractTable, expr, eTyp, StringType())
                            (exprContext, Some(newE))
                    }

                // If exceptions are ever catchable, we will need to make sure the fields of this have types consistent with their declarations.
                // For now, we treat this like a permanent abort.
                (contextPrime.makeThrown, Revert(ePrime).setLoc(s))

            case If(eCond: Expression, body: Seq[Statement]) =>
                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, eCond, NoOwnershipConsumption())
                checkIsSubtype(contextPrime.contractTable, s, t, BoolType())
                val (newContext, checkedStatements) = checkStatementSequence(decl, contextPrime, body)
                val newIf = If(ePrime, checkedStatements).setLoc(s)
                val contextIfTrue = pruneContext(s, newContext, contextPrime)
                (mergeContext(s, contextPrime, contextIfTrue, false, hasReturnStatementDontLog(body) || contextIfTrue.isThrown), newIf)

            case IfThenElse(eCond: Expression, body1: Seq[Statement], body2: Seq[Statement]) =>
                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, eCond, NoOwnershipConsumption())
                checkIsSubtype(contextPrime.contractTable, s, t, BoolType())
                val (trueContext, checkedTrueStatements) = checkStatementSequence(decl, contextPrime, body1)
                val contextIfTrue = pruneContext(s,
                    trueContext,
                    contextPrime)

                val (falseContext, checkedFalseStatements) = checkStatementSequence(decl, contextPrime, body2)

                val contextIfFalse = pruneContext(s,
                    falseContext,
                    contextPrime)

                val newIf = IfThenElse(ePrime, checkedTrueStatements, checkedFalseStatements).setLoc(s)

                val trueBranchExits = hasReturnStatementDontLog(body1) || contextIfTrue.isThrown
                val falseBranchExits = hasReturnStatementDontLog(body2) || contextIfFalse.isThrown

                (mergeContext(s, contextIfTrue, contextIfFalse, trueBranchExits, falseBranchExits), newIf)

            case IfInState(e, ePerm, state, body1, body2) =>
                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, NoOwnershipConsumption())

                val contractName = t match {
                    case np: NonPrimitiveType =>
                        np.contractName
                    case p: PrimitiveType =>
                        if (!t.isBottom) {
                          logError(e, StateCheckOnPrimitiveError())
                        }
                        // There was previously some kind of error. Don't propagate it.
                        return (contextPrime, IfInState(ePrime, ePerm, state, body1, body2).setLoc(s))
                    case _ =>
                        if (!t.isBottom) {
                            logError(e, SwitchError(t))
                        }
                        // There was previously some kind of error. Don't propagate it.
                        return (contextPrime, IfInState(ePrime, ePerm, state, body1, body2).setLoc(s))
                }

                val contractTable = context.contractTable.lookupContract(contractName) match {
                    case Some(table) => table
                    case None =>
                        logError(e, SwitchError(t))
                        return (contextPrime, IfInState(ePrime, ePerm, state, body1, body2).setLoc(s))
                }

                val allStates = contractTable.possibleStates

                var resetOwnership: Option[(String, NonPrimitiveType)] = None
                var valVariableToClear: Option[String] = None
                var exprIsField = false

                val (contextForCheckingTrueBranch, ePermPrime, contextForCheckingFalseBranch) =
                    t match {
                        case p: PrimitiveType =>
                            logError(s, StateCheckOnPrimitiveError())
                            (contextPrime, Inferred(), contextPrime)
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
                                    exprIsField = contextPrime.get(x).isEmpty

                                    if (!contextPrime.valVariables.contains(x)) {
                                        valVariableToClear = Some(x)
                                    }
                                    val newType = np.withTypeState(state)


                                    val trueBranchContext =
                                        if (exprIsField) {
                                            contextPrime.updatedThisFieldType(x, newType).updatedMakingVariableVal(x)
                                        }
                                        else {
                                            // This is a local variable, so just update it.
                                            contextPrime.updated(x, newType).updatedMakingVariableVal(x)
                                        }
                                    def falseBranchContext(typeFalse: ObsidianType) =
                                        if (exprIsField) {
                                            contextPrime.updatedThisFieldType(x, typeFalse).updatedMakingVariableVal(x)
                                        }
                                        else {
                                            contextPrime.updated(x, typeFalse).updatedMakingVariableVal(x)
                                        }

                                    if (np.permission == Unowned()) {
                                        (contextPrime, np.permission, contextPrime)
                                    }
                                    else {
                                        state match {
                                            case States(states) =>
                                                if (np.permission == Shared() || np.permission == Inferred()) {
                                                    resetOwnership = Some((x, np))
                                                }

                                                val typeForFalseBranch = np match {
                                                    case StateType(_, specificStates, _) =>
                                                        if (specificStates == states) {
                                                            logError(e, StateCheckRedundant())
                                                        }
                                                        StateType(np.contractType, specificStates -- states, np.remoteReferenceType)

                                                    case _ =>
                                                        if (allStates == states) {
                                                            logError(e, StateCheckRedundant())
                                                        }
                                                        StateType(np.contractType, allStates -- states, np.remoteReferenceType)
                                                }
                                                (trueBranchContext, np.permission, falseBranchContext(typeForFalseBranch))
                                            case permission: Permission =>
                                                logError(e, PermissionCheckRedundant(np.permission, permission, isSubpermission(np.permission, permission)))
                                                return (contextPrime, IfInState(ePrime, ePerm, state, body1, body2).setLoc(s))
                                            case permVar: PermVar =>
                                                val newType = np.withTypeState(permVar)
                                                (trueBranchContext,
                                                    np.permission,
                                                    contextPrime)
                                        }
                                    }
                                case None => (contextPrime, np.permission, contextPrime)
                            }
                        case BottomType() => (contextPrime, Inferred(), contextPrime)
                    }

                val (trueContext, checkedTrueStatements) = checkStatementSequence(decl, contextForCheckingTrueBranch, body1)
                val (falseContext, checkedFalseStatements) = checkStatementSequence(decl, contextForCheckingFalseBranch, body2)

                // Ensure that the variable that was checked retains ownership (was not given away) in the true branch.
                resetOwnership match {
                    case None => () // Nothing to do
                    case Some ((x, oldType)) =>
                        val newType = if (exprIsField) trueContext.thisFieldTypes.get(x) else trueContext.get(x)
                        newType match {
                        case None => () // OK; fields that are consistent with their declarations may not be in here anymore.
                        case Some(newType) => newType match {
                            case np: NonPrimitiveType => if (np.permission == Unowned()) {
                                logError(s, InvalidOwnershipLossInDynamicCheck(x))
                            }
                            case _ => assert(false, "Nonprimitive type should not become primitive in branch.");
                        }
                    }
                }

                val packedTrueContext = packFieldTypes(trueContext)
                val packedFalseContext = packFieldTypes(falseContext)

                val (resetTrueContext, resetFalseContext) = resetOwnership match {
                    case None => (packedTrueContext, packedFalseContext)
                    case Some((x, oldType)) =>
                        if (exprIsField) {
                            // Only reset the tested field in the context if it is still in scope (since the type of this may have changed)
                            val revisedTrueContext = if (packedTrueContext.lookupDeclaredFieldTypeInThis(x).isDefined) {
                                packedTrueContext.updatedThisFieldType(x, oldType)
                            } else {
                                packedTrueContext
                            }
                            val revisedFalseContext = if (packedFalseContext.lookupDeclaredFieldTypeInThis(x).isDefined) {
                                packedFalseContext.updatedThisFieldType(x, oldType)
                            } else {
                                packedFalseContext
                            }
                            (revisedTrueContext, revisedFalseContext)
                        }
                        else {
                            (packedTrueContext.updated(x, oldType), packedFalseContext.updated(x, oldType))
                        }
                }

                val newStatement = IfInState(ePrime, ePermPrime, state, checkedTrueStatements, checkedFalseStatements).setLoc(s)

                val trueBranchReturns = hasReturnStatementDontLog(body1)
                val falseBranchReturns = hasReturnStatementDontLog(body2)

                // Throw out (prune) any variables that were local to the branches.
                val contextIfTrue = pruneContext(s, resetTrueContext, contextForCheckingTrueBranch)
                val contextIfFalse = pruneContext(s, resetFalseContext, contextPrime)

                val mergedContext = mergeContext(s,
                    contextIfTrue,
                    contextIfFalse,
                    trueBranchReturns || trueContext.isThrown,
                    falseBranchReturns || falseContext.isThrown)
                val finalContext = valVariableToClear match {
                    case None => mergedContext
                    case Some(x) => mergedContext.updatedClearingValVariable(x)
                }

                (finalContext, newStatement)

            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) =>
                val (tryContext, checkedTryStatements) = checkStatementSequence(decl, context, s1)
                val (catchContext, checkedCatchStatements) = checkStatementSequence(decl, context, s2)


                val contextIfTry = pruneContext(s,
                    tryContext,
                    context)
                val contextIfCatch = pruneContext(s,
                    catchContext,
                    context)
                (mergeContext(s, contextIfTry, contextIfCatch,
                    hasReturnStatementDontLog(s1) || tryContext.isThrown,
                    hasReturnStatementDontLog(s2) || catchContext.isThrown),
                    TryCatch(checkedTryStatements, checkedCatchStatements).setLoc(s))

            case Switch(e: Expression, cases: Seq[SwitchCase]) =>
                val (t, contextPrime, ePrime) = inferAndCheckExpr(decl, context, e, NoOwnershipConsumption())

                val contractType = t match {
                    case np: NonPrimitiveType => np.contractType
                    case _ =>
                        if (!t.isBottom) {
                            logError(e, SwitchError(t))
                        }
                        // There was previously some kind of error. Don't propagate it.
                        return (contextPrime, Switch(ePrime, cases).setLoc(s))
                }

                val contractTable = context.contractTable.lookupContract(contractType) match {
                    case Some(table) => table
                    case None => logError(e, SwitchError(t))
                        return (contextPrime, Switch(ePrime, cases).setLoc(s))
                }

                def checkSwitchCase(sc: SwitchCase) : (Context, SwitchCase) = {
                    val newType: ObsidianType = t.withTypeState(States(Set(sc.stateName)))

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
                        // If a given switch case is guaranteed to return, then if it DID run, then the code BELOW this will NEVER run, so
                        // we need not merge the case's output context in.
                        val (firstCaseOutputContext, newCase) = checkSwitchCase(switchCase)

                        val initialContextToMerge = if (hasReturnStatementDontLog(switchCase.body)) contextPrime else firstCaseOutputContext
                        val restCases = cases.tail
                        restCases.foldLeft((initialContextToMerge, Seq(newCase)))((prev: (Context, Seq[SwitchCase]), sc: SwitchCase) => {
                            val (newContext, newCase) = checkSwitchCase(sc)
                            val caseExits = hasReturnStatementDontLog(sc.body) || newContext.isThrown
                            (mergeContext(s, prev._1, newContext, false, caseExits), newCase +: prev._2)
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

                def checkStateOrPermissionValid(contractName: String, stateName: String): Unit = {
                    // Assume stateOrPermission is a state name.
                    // Make sure it's a valid name, although it wouldn't harm too much it if it were.
                    val contractType = context.contractTable.lookupContract(contractName).get
                    val referencedState = contractType.state(stateName)
                    if (referencedState.isEmpty) {
                        logError(s, StaticAssertInvalidState(contractName, stateName))
                    }
                }

                def allValidStates(contractName: String) : Set[String] = {
                    val contractType = context.contractTable.lookupContract(contractName).get
                    contractType.possibleStates
                }

                val currentType = typ match {
                    case InterfaceContractType(name, realTyp) => realTyp
                    case _ => typ
                }

                currentType match {
                    case b: BottomType => ()
                    case p: PrimitiveType => logError(s, StaticAssertOnPrimitiveError(e))
                    case ContractReferenceType(contractType, permission, _) =>
                        allowedStatesOrPermissions match {
                            case States(states) =>
                                states.foreach(stateName => checkStateOrPermissionValid(contractType.contractName, stateName))
                                if (!allValidStates(contractType.contractName).subsetOf(states)) {
                                    logError(s, StaticAssertFailed(e, allowedStatesOrPermissions, typ))
                                }
                            case checkPerm: Permission =>
                                if (checkPerm != permission) {
                                    logError(s, StaticAssertFailed(e, allowedStatesOrPermissions, typ))
                                }
                            case PermVar(varName) =>
                                // These always fail, because the permission var has to exactly match for it to work
                                logError(s, StaticAssertFailed(e, allowedStatesOrPermissions, typ))
                        }

                    case stateType@StateType(contractType, stateNames, _) =>
                        allowedStatesOrPermissions match {
                            case States(states) =>
                                if (!stateNames.subsetOf(states)) {
                                    logError(s, StaticAssertFailed(e, allowedStatesOrPermissions, typ))
                                }
                                states.foreach(stateName => checkStateOrPermissionValid(stateType.contractName, stateName))
                            case _ =>
                                logError(s, StaticAssertFailed(e, allowedStatesOrPermissions, typ))
                        }

                    case InterfaceContractType(name, _) => assert(false, "Should have already eliminated this case")

                    case genericType: GenericType =>
                        val error = allowedStatesOrPermissions match {
                            case States(states) =>
                                if (genericType.gVar.permissionVar.isDefined) {
                                    true
                                } else {
                                    genericType.bound.typeState match {
                                        case States(actualStates) => actualStates.subsetOf(states)
                                        case _ => true
                                    }
                                }
                            case PermVar(varName) =>
                                genericType.gVar.permissionVar match {
                                    case Some(pVar) => pVar != varName
                                    case None => true
                                }
                            case permission: Permission =>
                                if (genericType.gVar.permissionVar.isDefined) {
                                    true
                                } else {
                                    genericType.bound.permission != permission
                                }
                        }

                        if (error) {
                            logError(s, StaticAssertFailed(e, allowedStatesOrPermissions, typ))
                        }
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

    private def checkDuplicateArgNames(tx: Transaction): Unit = {
        val argNames = mutable.HashSet[String]()
        for (arg <- tx.args) {
            if (argNames.exists(arg.varName.equals)) {
                logError(tx, DuplicateArgName(arg.varName))
            }
            argNames.add(arg.varName)
        }
    }

    private def checkTransactionInState(tx: Transaction,
                                        lexicallyInsideOf: DeclarationTable,
                                        initContext: Context): Transaction = {

        lexicallyInsideOf.contract match {
            case obsCon: ObsidianContractImpl =>
                for (param <- tx.params) {
                    obsCon.params.find(param.shadows)
                        .foreach(contractParam => logError(tx, GenericParamShadowError(tx.name, param, obsCon.name, contractParam)))
                }
            case _: JavaFFIContractImpl => ()
        }

        for (i <- tx.params.indices) {
            for (j <- i + 1 until tx.params.size) {
                if (tx.params(i).shadows(tx.params(j))) {
                    logError(tx, GenericParamShadowError(tx.name, tx.params(i), tx.name, tx.params(j)))
                }
            }
        }

        checkDuplicateArgNames(tx);
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

        // Check that the arguments meet the correct specification afterwards
        for (arg <- tx.args) {
            outputContext.get(arg.varName) match {
                case Some(actualTypOut) =>
                    val errorOpt = isSubtype(context.contractTable, actualTypOut, arg.typOut, false)
                    if (errorOpt.isDefined) {
                        logError(tx, ArgumentSpecificationError(arg.varName, tx.name, arg.typOut, actualTypOut))
                    }
                case None => ()
            }
        }

        checkForUnusedStateInitializers(outputContext)

        lexicallyInsideOf.contract match {
            case impl: ObsidianContractImpl =>
                // Only need to check the output context type on this if we're actually implementing the interface
                // (otherwise, with no body, it will always default to not typechecking correctly)
                if (!impl.isInterface) {
                    checkIsSubtype(context.contractTable, tx, outputContext("this"), expectedType, thisAtEndOfTransaction = true)
                }

                // Don't need to check interface methods to make sure they return
                if (!hasReturnStatement(tx.body) && !impl.isInterface && tx.retType.isDefined) {
                    logError(tx.bodyEnd, MustReturnError(tx.name))
                } else if (tx.retType.isEmpty) {
                    // We check for unused ownership errors at each return; if there isn't guaranteed to be one at the end, check separately.
                    // Every arg whose output type is owned should be owned at the end.
                    val ownedArgs = tx.args.filter((arg: VariableDeclWithSpec) => arg.typOut.isOwned)
                    val ownedArgNames = ownedArgs.map((arg: VariableDeclWithSpec) => arg.varName)
                    val ownedIdentifiers =
                        if (tx.thisFinalType.isOwned) {
                            ownedArgNames ++ Set("this")
                        }
                        else {
                            ownedArgNames
                        }
                    checkForUnusedOwnershipErrors(tx, outputContext, ownedIdentifiers.toSet)
                }

            case JavaFFIContractImpl(name, interface, javaPath, sp, declarations) => ()
        }

        // Check to make sure all the field types are consistent with their declarations.
        // But don't bother checking if the body is guaranteed to return, in which case this check was already done.
        if (!hasReturnStatementDontLog(tx.body)) {
            checkFieldTypeConsistencyAfterTransaction(outputContext, tx, tx.bodyEnd)
        }

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
                                  Set.empty,
                                  Map.empty,
                                  constr.args.map((v: VariableDeclWithSpec) => v.varName).toSet)

        val thisType = ContractReferenceType(table.contractType, Owned(), NotRemoteReferenceType())

        initContext = initContext.updated("this", thisType)

        for (arg <- constr.args) {
            initContext = initContext.updated(arg.varName, arg.typIn)
        }
        // Check as if all the nonprimitive fields were of Inferred permission initially so that we can track ownership correctly.

        for (field <- table.allFields) {
            val inferredType = field.typ.withTypeState(Inferred())
            initContext = initContext.updatedThisFieldType(field.name, inferredType)
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

            case ContractReferenceType(_, Inferred(), _) =>
                logError(constr, ConstructorAnnotationMissingError(table.name))

            case _ => ()
        }

        // Check that the arguments meet the correct specification afterwards
        for (arg <- constr.args) {
            outputContext.get(arg.varName) match {
                case Some(actualTypOut) =>
                    val errorOpt = isSubtype(outputContext.contractTable, actualTypOut, arg.typOut, isThis = false)
                    if (errorOpt.isDefined) {
                        logError(constr, ArgumentSpecificationError(arg.varName, constr.name, arg.typOut, actualTypOut))
                    }
                case None => ()
            }
        }

        val expectedThisType: NonPrimitiveType = constr.resultType
        val thisAST = This().setLoc(constr.loc) // Make a fake "this" AST so we generate the right error message.
        checkIsSubtype(table, thisAST, outputContext("this"), expectedThisType, true)

        checkForUnusedOwnershipErrors(constr, outputContext, Set("this"))
        checkForUnusedStateInitializers(outputContext)

        checkFieldTypeConsistencyAfterTransaction(outputContext, constr, constr.bodyEnd)

        // if the contract contains states, its constructor must contain a state transition
        if (hasStates && !hasTransition(constr.body)) {
            logError(constr, NoStartStateError(constr.name))
        }

        // If there are states, we'll check to make sure the transitions initialize all fields.
        // But we need to check contract fields separately.
        if (!outputContext.isThrown) {
            for (field <- table.allFields) {
                if (field.availableIn.isEmpty && !outputContext.localFieldsInitialized.contains(field.name)) {
                    logError(constr, UninitializedFieldError(field.name))
                }
            }
        }

        constr.copy(body = newBody)
    }

    private def checkConstructorDistinguishibility(constructors: Seq[Constructor], contract: ObsidianContractImpl, table: ContractTable): Unit = {
        if (constructors.isEmpty && table.stateLookup.nonEmpty && !contract.isInterface) {
            logError(contract, NoConstructorError(contract.name))
        }

        if (contract.isMain && constructors.length > 1) {
            logError(contract, MultipleConstructorsError(contract.name))
        }

        // We have two group by's because the groupings are not exactly the same: this one disregards states
        // to ensure that the generated Java code (which will not have states) will work
        val constructorGroups = constructors.groupBy(constr => constr.args.map(_.typIn.baseTypeName))

        // If the types are distinguishable, this function returns None
        // If they are NOT distinguishable, this function returns an example that is a subtype of both types
        // Two types are distinguishable iff they are both owned and have non-overlapping state lists
        def tryDistinguish(arg1: VariableDeclWithSpec, arg2: VariableDeclWithSpec): Option[AmbiguousConstructorExample] =
            ((arg1.typIn, arg2.typIn) match {
                case (t1: NonPrimitiveType, t2: NonPrimitiveType) =>
                    (t1.permission, t2.permission) match {
                        case (Unowned(), p)     => Some(s"${t2.contractName}@${p.toString}")
                        case (p, Unowned())     => Some(s"${t2.contractName}@${p.toString}")
                        case (Shared(), p)      => Some(s"${t2.contractName}@${p.toString}")
                        case (p, Shared())      => Some(s"${t2.contractName}@${p.toString}")
                        case (Owned(), Owned()) =>
                            table.possibleStatesFor(t1).intersect(table.possibleStatesFor(t2)).headOption.map(state => s"${t1.contractName}@$state")
                        case _                  => None
                    }

                // Should always (?) fail, since we grouped on type name (e.g., int is never distinguishable from another int)
                case (t1: PrimitiveType, t2: PrimitiveType) =>
                    if (t1.toString != t2.toString) { None } else { Some(t1.toString) }
                case (BottomType(), _) => None
                case (_, BottomType()) => None
                // This case should never happen, since we already grouped the constructor arguments on type
                case _ => assert(false, s"Unexpected distinguishability test: $arg1, $arg2"); None
            }).map(example => AmbiguousConstructorExample(example, arg1, arg2))

        // https://stackoverflow.com/a/6751877/1498618
        def sequence[A](opts: Seq[Option[A]]): Option[Seq[A]] =
            if (opts.contains(None)) { None } else { Some(opts.flatten) }

        // Returns the first argument pair that is ambiguous, if EVERY argument pair is ambiguous
        // If any pair is distinguishable, that means the constructors are themselves distinguishable
        def distinguish(constructor: Constructor, other: Constructor): Option[Seq[AmbiguousConstructorExample]] = {
            val pairedArgs = constructor.args.zip(other.args)
            sequence(pairedArgs.map { case (arg1, arg2) => tryDistinguish(arg1, arg2) })
        }

        // Check each group of constructors whose parameters have the same base type for ambiguity
        constructorGroups.values.foreach(group => {
            for (constructor <- group) {
                // So we don't do double or self-comparisons
                for (other <- group.drop(group.indexOf(constructor) + 1)) {
                    distinguish(constructor, other).foreach(examples =>
                        logError(other, AmbiguousConstructorError(contract.name, examples)))
                }
            }
        })
    }

    private def checkForMainContract(ast: Program) = {
        val c: Option[Contract] = ast.contracts.find((c: Contract) =>
            c.modifiers.contains(IsMain()))
        if (c.isEmpty) logError(ast, NoMainContractError())
    }

    private def checkDeclaration(table: ContractTable)(decl: Declaration): Declaration = {
        decl match {
            case t: Transaction =>
                // When checking the body, just make sure that the generic parameters are properly
                // recognized as being parameters (e.g., for subtype checking)
                checkTransaction(t.substitute(t.params, t.params), table)
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

    def checkArgSubtype(table: ContractTable, ast: AST, arg1: VariableDeclWithSpec, arg2: VariableDeclWithSpec): Unit = {
        checkIsSubtype(table, ast, arg2.typIn, arg1.typIn)
        checkIsSubtype(table, ast, arg1.typOut, arg2.typOut)
    }

    def implementOk(table: ContractTable, tx: Transaction, interfaceTx: Transaction): Unit = {
        if (tx.args.length != interfaceTx.args.length) {
            logError(tx, MethodImplArgsError(tx.name, tx.args, interfaceTx.args))
        }

        for ((txArg, implArg) <- tx.args.zip(interfaceTx.args)) {
            checkArgSubtype(table, tx, implArg, txArg)
        }

        (tx.retType, interfaceTx.retType) match {
            case (Some(txRet), Some(interfaceRet)) => checkIsSubtype(table, tx, txRet, interfaceRet)
            case (None, None) => ()
            case _ =>
                logError(tx, MethodImplReturnTypeError(tx.name, tx.retType, interfaceTx.retType))
        }
    }

    // Checks to make sure that a contract actually implements all the declarations in the interface it claims to implement.
    def implementOk(table: ContractTable, contract: Contract, interfaceName: String,
                    declarations: Seq[Declaration], boundDecls: Seq[Declaration]): Unit = {
        var toImplStates = boundDecls.flatMap {
            case state: State => state :: Nil
            case _ => Nil
        }.toSet

        var toImplTransactions = boundDecls.flatMap {
            case declaration: InvokableDeclaration =>
                declaration match {
                    case Constructor(name, args, resultType, body) =>
                        logError(declaration, InterfaceConstructorError())
                        Nil
                    case transaction: Transaction => transaction :: Nil
                }

            case _ => Nil
        }.toSet

        for (decl <- declarations) {
            decl match {
                case s@State(name, fields, isAsset) =>
                    toImplStates = toImplStates.filter(state => {
                        if (state.name == name) {
                            if (isAsset && !state.isAsset) {
                                logError(s, AssetStateImplError(s, state))
                                true
                            } else {
                                false
                            }
                        } else {
                            true
                        }
                    })

                case declaration: InvokableDeclaration => declaration match {
                    case transaction: Transaction =>
                        toImplTransactions.find(_.name == transaction.name).foreach(implementOk(table, transaction, _))
                        // Remove the transaction from the list of things we need to implement
                        toImplTransactions = toImplTransactions.filter(_.name != transaction.name)
                    case constructor: Constructor => ()
                }

                case _ => ()
            }
        }

        for (stateName <- toImplStates) {
            logError(contract, MissingStateImplError(contract.name, interfaceName, stateName.name))
        }

        for (transaction <- toImplTransactions) {
            logError(contract, MissingTransactionImplError(contract.name, interfaceName, transaction.name))
        }
    }

    private def checkInterfaceDeclaration(decl: Declaration): Unit = {
        decl match {
            case t: Transaction =>
                checkDuplicateArgNames(t)
            case s: State => ()
            case f: Field => logError(f, FieldsNotAllowedInInterfaces(f.name, decl.name))
            case c: Constructor => logError(c, InterfaceConstructorError())
            case c: Contract => assert(false, "Contract nesting is no longer supported.")
            case d: TypeDecl => assert(false, "Type declarations are not supported yet.")
        }
    }

    private def checkContract(contract: Contract): Contract = {
        currentContractSourcePath = contract.sourcePath

        contract match {
            case obsContract: ObsidianContractImpl =>
                val table = globalTable.contractLookup(obsContract.name)

                for (i <- obsContract.params.indices) {
                    for (j <- i + 1 until obsContract.params.size) {
                        if (obsContract.params(i).shadows(obsContract.params(j))) {
                            logError(obsContract, GenericParamShadowError(obsContract.name, obsContract.params(i), obsContract.name, obsContract.params(j)))
                        }
                    }
                }

                var newDecls = obsContract.declarations

                if (obsContract.isInterface) {
                    obsContract.declarations.map(checkInterfaceDeclaration)
                }
                else {
                    val boundDecls = globalTable.contract(obsContract.bound.contractName) match {
                        case Some(interface) => interface.contract match {
                            case impl: ObsidianContractImpl => impl.substitute(impl.params, obsContract.bound.typeArgs).declarations
                            case JavaFFIContractImpl(name, interface, javaPath, sp, declarations) =>
                                logError(obsContract, BadFFIInterfaceBoundError(name))
                                Nil
                        }
                        case None =>
                            logError(obsContract, InterfaceNotFoundError(obsContract.name, obsContract.bound.contractName))
                            Nil
                    }

                    implementOk(table, obsContract, obsContract.bound.contractName, obsContract.declarations, boundDecls)
                    newDecls = obsContract.declarations.map(checkDeclaration(table))
                    checkConstructorDistinguishibility(table.constructors, obsContract, table)
                }

                obsContract.copy(declarations = newDecls)
            case ffiContract: JavaFFIContractImpl => ffiContract
        }
    }

    def checkForDuplicateTopDecl(contracts: Seq[Contract]): Unit = {
        var foundTop = false
        for (contract <- contracts) {
            if (contract.name == ContractType.topContractName) {
                if (foundTop) {
                    logError(contract, DuplicateContractError(contract.name))
                } else {
                    foundTop = true
                }
            }
        }
    }

    // Returns a new program (in a symbol table) with additional type information, paired with errors from the program.
    def checkProgram(): (Seq[ErrorRecord], SymbolTable) = {
        checkForMainContract(globalTable.ast)

        var checkDiffContract = (x: Contract) => x match {
            case obsContract : ObsidianContractImpl => checkContract(obsContract)
            case ffiContract : JavaFFIContractImpl => ffiContract
        }

        checkForDuplicateTopDecl(globalTable.ast.contracts)

        val newContracts = globalTable.ast.contracts.map(checkDiffContract)

        (errors, new SymbolTable(Program(globalTable.ast.imports, newContracts)))
    }
}

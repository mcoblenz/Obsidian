package edu.cmu.cs.obsidian.typecheck
import edu.cmu.cs.obsidian.parser._

sealed trait TypeState

case class States(states: Set[String]) extends TypeState {
    override def toString: String = states.mkString(" | ")
}
case class PermVar(varName: String) extends TypeState {
    override def toString: String = varName
}

sealed trait Permission extends TypeState {
    def residual: Permission
}

case class Shared() extends Permission {
    override def toString: String = "Shared"

    override def residual: Permission = Shared()
}
case class Owned() extends Permission {
    override def toString: String = "Owned"

    override def residual: Permission = Unowned()
}

case class Unowned() extends Permission {
    override def toString: String = "Unowned"

    override def residual: Permission = Unowned()
}

case class Inferred() extends Permission {
    override def toString: String = "Inferred"

    override def residual: Permission = Inferred()
} // For local variables

trait OwnershipConsumptionMode
case class ConsumingOwnedGivesShared() extends OwnershipConsumptionMode
case class ConsumingOwnedGivesUnowned() extends OwnershipConsumptionMode
case class NoOwnershipConsumption() extends OwnershipConsumptionMode

sealed trait RemoteReferenceType
case class NotRemoteReferenceType() extends RemoteReferenceType
case class TopLevelRemoteReferenceType() extends RemoteReferenceType
case class NonTopLevelRemoteReferenceType() extends RemoteReferenceType

// Type of references to contracts.
case class ContractReferenceType(override val contractType: ContractType,
                                 permission: Permission,
                                 override val remoteReferenceType: RemoteReferenceType) extends NonPrimitiveType {
    override val isRemote = remoteReferenceType != NotRemoteReferenceType()

    override def toString: String =
        if (permission == Inferred()) {
            s"$contractType"
        } else {
            s"$contractType@$permission"
        }

    val contractName: String = contractType.contractName

    override def isOwned: Boolean = permission == Owned()

    override def residualType(mode: OwnershipConsumptionMode): NonPrimitiveType = {
        if (permission == Owned()) {
            val newPermission =
                mode match {
                    case ConsumingOwnedGivesShared() => Shared()
                    case ConsumingOwnedGivesUnowned() => Unowned()
                    case NoOwnershipConsumption() => Owned()
                }

            this.copy(permission = newPermission).setLoc(this)
        }
        else {
            this
        }
    }

    override def topPermissionType: NonPrimitiveType = this.copy(permission = Unowned()).setLoc(this)

    override def remoteType(remoteReferenceType: RemoteReferenceType): NonPrimitiveType =
        ContractReferenceType(contractType, permission, remoteReferenceType).setLoc(this)

    override def genericParams: Seq[ObsidianType] = contractType.typeArgs

    override  def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType =
        ObsidianType.substituteVarName(contractName, genericParams, actualParams).getOrElse(
            ContractReferenceType(contractType.substitute(genericParams, actualParams), permission, remoteReferenceType))
            .withTypeState(permission)

    override def typeState: TypeState = permission

    override def withTypeState(ts: TypeState): NonPrimitiveType = ts match {
        case States(states) => StateType(contractType, states, remoteReferenceType)
        case PermVar(varName) => assert(false, "Trying to use a permission variable on a concrete type"); null
        case permission: Permission => this.copy(permission = permission)
    }

    override def withParams(contractParams: Seq[ObsidianType]): NonPrimitiveType =
        copy(contractType = contractType.copy(typeArgs = contractParams))

    override def typeByMatchingPermission(otherType: NonPrimitiveType): NonPrimitiveType =
        this.copy(permission = otherType.permission).setLoc(this)
}



// Type of actual contracts. This is ALMOST NEVER the right class; it is specially for actual contracts.
// Almost everywhere will use ContractReferenceType. This intentionally does not extend ObsidianType
// because it is not available in the language itself (for now).
case class ContractType(contractName: String, typeArgs: Seq[ObsidianType]) {
    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ContractType =
        ContractType(contractName, typeArgs.map(_.substitute(genericParams, actualParams)))

    override def toString: String =
        if (typeArgs.isEmpty) {
            contractName
        } else {
            s"$contractName[${typeArgs.mkString(", ")}]"
        }
}

object ContractType {
    def topContractName: String = "Contract"
    def topContractParams: Seq[GenericType] = Nil
    def topContractType: ContractType = ContractType(topContractName, topContractParams)
    def unownedTop: ObsidianType =
        ContractReferenceType(topContractType, Unowned(), NotRemoteReferenceType())
    def topContractImpl =
        ObsidianContractImpl(
            Set(),
            topContractName,
            topContractParams,
            topContractType,
            Nil,
            None,
            isInterface = true,
            "")
}

object Possibility {
    def fromBoolean(b: Boolean): Possibility =
        if (b) {
            Yes()
        } else {
            No()
        }
}

sealed trait Possibility
case class Yes() extends Possibility
case class Maybe() extends Possibility
case class No() extends Possibility

/* Invariant: [stateNames] is missing at least one of the states of the
 * contract (i.e. it is more specific than [ContractReferenceType(contractName)],
 * but has at least 2 distinct states
 *
 * StateType is always owned.
 * */
case class StateType(override val contractType: ContractType, stateNames: Set[String], override val remoteReferenceType: RemoteReferenceType) extends NonPrimitiveType {
    def this(contractType: ContractType, stateName: String, remoteReferenceType: RemoteReferenceType) = {
        this(contractType, Set(stateName), remoteReferenceType)
    }

    override val isRemote = remoteReferenceType != NotRemoteReferenceType()

    private def orOfStates: String = stateNames.mkString(" | ")

    override def toString: String =
        s"$contractType@${if (stateNames.size > 1) s"($orOfStates)" else orOfStates}"

    override val permission = Owned()

    override def isOwned = true

    override def residualType(mode: OwnershipConsumptionMode): NonPrimitiveType = {
        if (mode == NoOwnershipConsumption()) {
            this
        }
        else {
            val newPermission =
                mode match {
                    case ConsumingOwnedGivesShared() => Shared()
                    case ConsumingOwnedGivesUnowned() => Unowned()
                    case NoOwnershipConsumption() => Owned()
                }

            ContractReferenceType(contractType, newPermission, remoteReferenceType).setLoc(this)
        }
    }
    override val topPermissionType: NonPrimitiveType = this

    override def isAssetReference(contextContractTable: ContractTable): Possibility = {
        val contract = contextContractTable.lookupContract(contractName)

        if (contract.isDefined && contract.get.contract.isAsset) {
            return Yes()
        }

        contract match {
            case None => No() // This will result in an error elsewhere.
            case Some(contractTable) =>
                val statesAreAssets = stateNames.map((sn: String) =>
                    contractTable.state(sn) match
                {
                    case None => No()
                    case Some(stateTable) => if(stateTable.ast.isAsset) Yes() else No()
                })


                // If there's no possible states (i.e., statesAreAssets is empty), then there should be error elsewhere
                if (statesAreAssets.isEmpty) {
                    No()
                } else {
                    statesAreAssets.reduce((p1: Possibility, p2: Possibility) => if (p1 == p2) p1 else Maybe())
                }
        }
    }

    override def remoteType(remoteReferenceType: RemoteReferenceType): NonPrimitiveType =
        StateType(contractType, stateNames, remoteReferenceType).setLoc(this)

    override val contractName: String = contractType.contractName

    override def genericParams: Seq[ObsidianType] = contractType.typeArgs

    def substituteStateNames(stateNames: Set[String], genericParams: Seq[GenericType],
                             actualParams: Seq[ObsidianType]): TypeState = {
        val stateNameMap = ObsidianType.stateSubstitutions(genericParams, actualParams)
        val results = stateNames.map(name => stateNameMap.getOrElse(name, States(Set(name))))

        results.find(!_.isInstanceOf[States]) match {
            case Some(ts) => ts
            case None => States(results.flatMap(_.asInstanceOf[States].states))
        }
    }

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType =
        ObsidianType.substituteVarName(contractName, genericParams, actualParams).getOrElse(
            StateType(contractType.substitute(genericParams, actualParams), stateNames, remoteReferenceType))
            .withTypeState(substituteStateNames(stateNames, genericParams, actualParams))

    override def typeState: TypeState = States(stateNames)

    override def withTypeState(ts: TypeState): NonPrimitiveType = ts match {
        case States(states) => this.copy(stateNames = states)
        case permission: Permission => ContractReferenceType(contractType, permission, remoteReferenceType)
        case PermVar(varName) =>
            // This should never happen
            assert(false, "Trying to use a state variable on a concrete type"); null
    }

    override def withParams(contractParams: Seq[ObsidianType]): StateType =
        this.copy(contractType = ContractType(contractName, contractParams))

    override def typeByMatchingPermission(otherType: NonPrimitiveType): NonPrimitiveType = otherType
}

object StateType {
    def apply(contractType: ContractType, stateName: String, remoteReferenceType: RemoteReferenceType): StateType =
        new StateType(contractType, Set(stateName), remoteReferenceType)
}


/* a path starts with either a local variable or "this", but "this" can sometimes be omitted */
//case class PathType(path: Seq[String], ts: NonPrimitiveType) extends NonPrimitiveType {
//    private def pathAsString = path.foldLeft("")(
//        (prev: String, pathNode: String) => prev + pathNode + "."
//    )
//    override def toString: String = pathAsString + ts.toString
//    override val extractSimpleType: NonPrimitiveType = ts
//}

/* Invariant for permissioned types: any path that occurs in the type makes "this" explicit */
sealed trait ObsidianType extends HasLocation {
    def typeParams: Seq[ObsidianType]
    def withParams(contractParams: Seq[ObsidianType]): ObsidianType

    def typeState: TypeState

    def withTypeState(ts: TypeState): ObsidianType

    // for tests
    val isBottom: Boolean

    /* the permission system doesn't allow arbitrary aliasing of a reference
     * typed as [t]: aliasing forces one of the resulting types to be
     * [residualType(t)] instead */
    def residualType(mode: OwnershipConsumptionMode): ObsidianType

    def topPermissionType: ObsidianType

    def isOwned = false
    val remoteReferenceType: RemoteReferenceType = NotRemoteReferenceType()

    def isAssetReference(contextContractTable: ContractTable): Possibility = No()

    def baseTypeName: String = toString

    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType
}

object ObsidianType {
    def lookupState(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType])(permVar: PermVar): TypeState =
        stateSubstitutions(genericParams, actualParams).getOrElse(permVar.varName, permVar)

    def stateSubstitutions(genericParams: Seq[GenericType],
                           actualParams: Seq[ObsidianType]): Map[String, TypeState] = {
        val t = for ((genParam, actualParam) <- genericParams.zip(actualParams)) yield {
            genParam.gVar.permissionVar match {
                case Some(varName) => (varName, actualParam.typeState) :: Nil
                case None => Nil
            }
        }

        t.flatten.toMap
    }

    def substituteVarName(contractName: String,
                          genericParams: Seq[GenericType],
                          actualParams: Seq[ObsidianType]): Option[ObsidianType] = {
        val idx = genericParams.indexWhere(p => p.gVar.varName == contractName)

        // If idx is too large, then we will get an error elsewhere
        if (idx >= 0 && idx < actualParams.length) {
            Some(actualParams(idx))
        } else {
            None
        }
    }
}

/* int, int256, unit, bool, or string */
sealed trait PrimitiveType extends ObsidianType {
    val isBottom: Boolean = false
    override def residualType(mode: OwnershipConsumptionMode): ObsidianType = this
    override val topPermissionType: ObsidianType = this

    // Substituting for primitives does nothing
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType = this

    override def withTypeState(ts: TypeState): ObsidianType = this
    // All primitive types are treated as owned
    override def typeState: TypeState = Owned()

    override def typeParams: Seq[ObsidianType] = Nil
    override def withParams(contractParams: Seq[ObsidianType]): ObsidianType = this
}

/* all permissioned types are associated with their corresponding symbol table
 * These types were generated by resolution; they are not generated by the parser.
 */
sealed trait NonPrimitiveType extends ObsidianType {
    val isBottom: Boolean = false

    val isRemote: Boolean = false

    val permission: Permission

    val contractName: String

    def codeGenName: String = contractName

    override def typeParams: Seq[ObsidianType] = contractType.typeArgs
    def withParams(contractParams: Seq[ObsidianType]): NonPrimitiveType

    override def isOwned: Boolean = permission == Owned()

    def contractType: ContractType = ContractType(contractName, genericParams)
    def genericParams: Seq[ObsidianType]

    override def withTypeState(ts: TypeState): NonPrimitiveType

    override def baseTypeName: String = contractName

    def topPermissionType = this

    override def isAssetReference(contextContractTable: ContractTable): Possibility = {
        contextContractTable.lookupContract(contractName) match {
            case Some(contract) => {
                if (contract.contract.isAsset) {
                    Yes()
                } else {
                    StateType(contract.contractType, contract.possibleStates, remoteReferenceType).isAssetReference(contextContractTable)
                }
            }

            case None => No()
        }
    }

    def remoteType(remoteReferenceType: RemoteReferenceType): NonPrimitiveType

    def typeByMatchingPermission(otherType: NonPrimitiveType): NonPrimitiveType
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

case class Int256Type() extends PrimitiveType {
    override def toString: String = "Int256"
}

case class UnitType() extends PrimitiveType {
    override def toString: String = "unit"
}

/* Used to indicate an error in the type checker when a reasonable type cannot
 * otherwise be inferred */
case class BottomType() extends ObsidianType {
    val isBottom: Boolean = true
    override def residualType(mode: OwnershipConsumptionMode): ObsidianType = this
    override def topPermissionType: ObsidianType = this

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType = this

    override def withParams(contractParams: Seq[ObsidianType]): ObsidianType = this
    override def typeState: TypeState = Unowned() // Doesn't really matter for bottom
    override def withTypeState(ts: TypeState): ObsidianType = BottomType()
    override def typeParams: Seq[ObsidianType] = Nil
}

// TODO GENERIC: Does this need to be extended for generics?
case class InterfaceContractType(name: String, simpleType: NonPrimitiveType) extends NonPrimitiveType {
    override def toString: String = name
    override val isBottom: Boolean = false
    override def residualType(mode: OwnershipConsumptionMode): NonPrimitiveType = this
    override def topPermissionType: NonPrimitiveType = this
    override val contractName: String = name
    override val permission: Permission = simpleType.permission
    override def remoteType(remoteReferenceType: RemoteReferenceType): NonPrimitiveType = this

    override def genericParams: Seq[ObsidianType] = simpleType.genericParams

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType = {
        simpleType.substitute(genericParams, actualParams) match {
            case np: NonPrimitiveType => InterfaceContractType(name, np)

            case other => ObsidianType.substituteVarName(contractName, genericParams, actualParams).getOrElse(this)
        }
    }

    override def withParams(contractParams: Seq[ObsidianType]): NonPrimitiveType =
        this.copy(simpleType = simpleType.withParams(contractParams))

    override def typeState: TypeState = simpleType.typeState
    override def withTypeState(ts: TypeState): NonPrimitiveType = this.copy(simpleType = simpleType.withTypeState(ts))

    override def typeByMatchingPermission(otherType: NonPrimitiveType): NonPrimitiveType =
        InterfaceContractType(name, simpleType.typeByMatchingPermission(otherType))
}

sealed trait GenericBound {
    def genericParams: Seq[ObsidianType]

    def withParams(contractParams: Seq[ObsidianType]): GenericBound

    def typeState: TypeState

    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GenericBound

    def withStates(stateNames: Set[String]): GenericBound
    def withPermission(permission: Permission): GenericBound

    def interfaceType: ContractType
    def permission: Permission
    def residualType(mode: OwnershipConsumptionMode): GenericBound

    def referenceType: NonPrimitiveType

    def show(genericVar: GenericVar): String

    def interfaceSpecified: Boolean

    def declBound(genericVar: GenericVar): String =
        if (interfaceSpecified) {
            s" where ${genericVar.varName} implements $interfaceType"
        } else {
            ""
        }
}

case class GenericBoundPerm(interfaceSpecified: Boolean, permSpecified: Boolean,
                            interfaceType: ContractType, permission: Permission) extends GenericBound {
    override def residualType(mode: OwnershipConsumptionMode): GenericBound = {
        if (permission == Owned()) {
            val newPermission =
                mode match {
                    case ConsumingOwnedGivesShared() => Shared()
                    case ConsumingOwnedGivesUnowned() => Unowned()
                    case NoOwnershipConsumption() => Owned()
                }

            this.copy(permission = newPermission)
        } else {
            this
        }
    }

    override def withPermission(permission: Permission): GenericBound =
        copy(permission = permission)

    override def withStates(stateNames: Set[String]): GenericBound =
        GenericBoundStates(interfaceSpecified, permSpecified, interfaceType, stateNames)

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GenericBound =
        this.copy(interfaceType = interfaceType.substitute(genericParams, actualParams))

    override def typeState: TypeState = permission

    override def referenceType: NonPrimitiveType = ContractReferenceType(interfaceType, permission, NotRemoteReferenceType())

    override def genericParams: Seq[ObsidianType] = interfaceType.typeArgs
    override def withParams(contractParams: Seq[ObsidianType]): GenericBound =
        copy(interfaceType = interfaceType.copy(typeArgs = contractParams))

    private def permBound(genericVar: GenericVar): String =
        if (genericVar.permissionVar.isDefined && permSpecified) {
            s" where ${genericVar.permissionVar.get} is $permission"
        } else {
            ""
        }

    override def show(genericVar: GenericVar): String = declBound(genericVar) + permBound(genericVar)
}

case class GenericBoundStates(interfaceSpecified: Boolean, permSpecified: Boolean,
                              interfaceType: ContractType, states: Set[String]) extends GenericBound {
    override def permission: Permission = Owned()

    override def residualType(mode: OwnershipConsumptionMode): GenericBound =
        if (mode == NoOwnershipConsumption()) {
            this
        } else {
            val newPermission =
                mode match {
                    case ConsumingOwnedGivesShared() => Shared()
                    case ConsumingOwnedGivesUnowned() => Unowned()

                    // This shouldn't happen, since we check for it above
                    case NoOwnershipConsumption() => Owned()
                }

            GenericBoundPerm(interfaceSpecified, permSpecified, interfaceType, newPermission)
        }

    override def withPermission(permission: Permission): GenericBound =
        GenericBoundPerm(interfaceSpecified, permSpecified, interfaceType, permission)

    override def withStates(stateNames: Set[String]): GenericBound = this.copy(states = stateNames)

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GenericBound =
        this.copy(interfaceType = interfaceType.substitute(genericParams, actualParams))

    override def typeState: TypeState = States(states)

    override def referenceType: NonPrimitiveType = StateType(interfaceType, states, NotRemoteReferenceType())

    override def genericParams: Seq[ObsidianType] = interfaceType.typeArgs
    override def withParams(contractParams: Seq[ObsidianType]): GenericBound =
        copy(interfaceType = interfaceType.copy(typeArgs = contractParams))

    private def permBound(genericVar: GenericVar): String =
        if (genericVar.permissionVar.isDefined) {
            s" where ${genericVar.permissionVar.get} is (${states.mkString(" | ")})"
        } else {
            ""
        }

    override def show(genericVar: GenericVar): String = declBound(genericVar) + permBound(genericVar)
}

case class GenericVar(isAsset: Boolean, varName: String, permissionVar: Option[String]) {
    def hasPermissionVar(stateName: String): Boolean = permissionVar.contains(stateName)

    override def toString: String =
        s"${if (isAsset) { "asset " } else { "" }}$varName${if (permissionVar.isDefined) { "@" + permissionVar.get } else { "" }}"
}

case class GenericType(gVar: GenericVar, bound: GenericBound) extends NonPrimitiveType {
    override def toString: String =
        s"${gVar}@${permission}${bound.show(gVar)}"

    def hasPermissionVar(stateName: String): Boolean = gVar.hasPermissionVar(stateName)

    // We need to treat all permission variables as though they may be owned
    override def isOwned: Boolean =
        gVar.permissionVar.isDefined || super.isOwned

    override val permission: Permission = bound.permission
    override val contractName: String = bound.interfaceType.contractName

    override def remoteType(remoteReferenceType: RemoteReferenceType): NonPrimitiveType = this

    override def residualType(mode: OwnershipConsumptionMode): ObsidianType =
        if (mode == NoOwnershipConsumption()) {
            this
        } else if (isOwned) {
            val newPermission =
                mode match {
                    case ConsumingOwnedGivesShared() => Shared()
                    case ConsumingOwnedGivesUnowned() => Unowned()

                    // This shouldn't happen, since we check for it above
                    case NoOwnershipConsumption() => Owned()
                }

            withTypeState(newPermission)
        } else {
            this
        }

    def lookupInterface(contractTable: ContractTable): Option[ContractTable] =
        contractTable.lookupContract(bound.interfaceType)

    override def isAssetReference(contextContractTable: ContractTable): Possibility =
        Possibility.fromBoolean(gVar.isAsset)

    override def contractType: ContractType = bound.interfaceType
    override def genericParams: Seq[ObsidianType] = bound.genericParams

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType = {
        val idx = genericParams.indexWhere(_.gVar.varName == this.gVar.varName)

        // if idx is greater, we'll get an error elsewhere
        if (idx >= 0 && idx < actualParams.length) {
            // If we have a permission var, we want to lookup the correct thing to substitute
            // Otherwise, we will just keep what we had
            val defaultPermOrState = bound.typeState match {
                case p: PermVar => ObsidianType.lookupState(genericParams, actualParams)(p)
                case ts => ts
            }

            val typeState = gVar.permissionVar match {
                case Some(permissionVarName) =>
                    ObsidianType.stateSubstitutions(genericParams, actualParams)
                        .getOrElse(permissionVarName, defaultPermOrState)
                case None => defaultPermOrState
            }

            actualParams(idx).withTypeState(typeState)
        } else {
            this.copy(bound = bound.substitute(genericParams, actualParams))
        }
    }

    override def withParams(contractParams: Seq[ObsidianType]): NonPrimitiveType =
        copy(bound = bound.withParams(contractParams))

    override def codeGenName: String = gVar.varName

    override def withTypeState(ts: TypeState): NonPrimitiveType = ts match {
        case States(states) => GenericType(gVar.copy(permissionVar = None), bound.withStates(states))
        case permission: Permission => GenericType(gVar.copy(permissionVar = None), bound.withPermission(permission))
        case PermVar(varName) => this.copy(gVar = gVar.copy(permissionVar = Some(varName)))
    }

    override def typeState: TypeState = gVar.permissionVar match {
        case Some(name) => PermVar(name)
        case None => bound.typeState
    }

    override def typeByMatchingPermission(otherType: NonPrimitiveType): NonPrimitiveType =
        withTypeState(otherType.permission)

    def shadows(other: GenericType): Boolean = {
        val permVarsMatch = (gVar.permissionVar, other.gVar.permissionVar) match {
            case (Some(permVar1), Some(permVar2)) => permVar1 == permVar2
            case _ => false
        }
        gVar.varName == other.gVar.varName || permVarsMatch
    }
}

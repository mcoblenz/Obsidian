package edu.cmu.cs.obsidian.typecheck
import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.parser._

import scala.util.parsing.input.Position


sealed trait Permission {
    def residual: Permission
}

// TODO GENERIC: We need a new permission: Unspecified() or Variable() or something

// This should ONLY be used for passing generic type parameters to functions
//case class Unspecified() extends Permission {
//    override def toString: String = "Unspecified"
//
//    override def residual: Permission = Unspecified()
//}

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

// Type of references to contracts.
case class ContractReferenceType(override val contractType: ContractType,
                                 permission: Permission,
                                 override val isRemote: Boolean) extends NonPrimitiveType {
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

    override def remoteType: NonPrimitiveType = ContractReferenceType(contractType, permission, isRemote = true)

    override def genericParams: Seq[ObsidianType] = contractType.typeArgs

    // TODO GENERIC: handle state variables here
    override  def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType =
        ObsidianType.substituteVarName(contractName, genericParams, actualParams).getOrElse(
            ContractReferenceType(contractType.substitute(genericParams, actualParams), permission, isRemote))
            .withPermission(permission)

    override def withStates(stateNames: Set[String]): NonPrimitiveType =
        StateType(contractType, stateNames, isRemote)

    override def withPermission(permission: Permission): NonPrimitiveType = this.copy(permission = permission)

    override def permissionOrState: Either[Permission, Set[String]] = Left(permission)

    override def withParams(contractParams: List[GenericType]): NonPrimitiveType =
        this.copy(contractType = ContractType(contractName, contractParams))
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
case class StateType(override val contractType: ContractType, stateNames: Set[String], override val isRemote: Boolean) extends NonPrimitiveType {
    // TODO GENERIC: Do we need this
    def this(contractType: ContractType, stateName: String, isRemote: Boolean) = {
        this(contractType, Set(stateName), isRemote)
    }

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

            ContractReferenceType(contractType, newPermission, isRemote).setLoc(this)
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

    override def remoteType: NonPrimitiveType = StateType(contractType, stateNames, isRemote = true)

    override val contractName: String = contractType.contractName

    override def genericParams: Seq[ObsidianType] = contractType.typeArgs

    def substituteStateNames(stateNames: Set[String], genericParams: Seq[GenericType],
                             actualParams: Seq[ObsidianType]): Either[Permission, Set[String]] = {
        val stateNameMap = ObsidianType.stateSubstitutions(genericParams, actualParams)
        val results = stateNames.map(name => stateNameMap.get(name) match {
            case Some(value) => value
            case None => Right(Set(name))
        })

        results.find(_.isLeft) match {
            case Some(perm) => perm
            case None => Right(results.flatMap(_.right.get))
        }
    }

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType =
        ObsidianType.substituteVarName(contractName, genericParams, actualParams).getOrElse(
            StateType(contractType.substitute(genericParams, actualParams), stateNames, isRemote))
            .withPermOrState(substituteStateNames(stateNames, genericParams, actualParams))

    override def withStates(stateNames: Set[String]): NonPrimitiveType = this.copy(stateNames = stateNames)

    override def withPermission(permission: Permission): NonPrimitiveType =
        ContractReferenceType(contractType, permission, isRemote)

    override def permissionOrState: Either[Permission, Set[String]] = Right(stateNames)

    override def withParams(contractParams: List[GenericType]): NonPrimitiveType =
        this.copy(contractType = ContractType(contractName, contractParams))
}

object StateType {
    def apply(contractType: ContractType, stateName: String, isRemote: Boolean): StateType =
        new StateType(contractType, Set(stateName), isRemote)
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
    // TODO GENERIC: Should this stuff be here?
    def withStates(stateNames: Set[String]): ObsidianType
    def withPermission(permission: Permission): ObsidianType
    def withParams(contractParams: List[GenericType]): ObsidianType

    def permissionOrState: Either[Permission, Set[String]]

    def withPermOrState: Either[Permission, Set[String]] => ObsidianType = {
        case Left(perm) => this.withPermission(perm)
        case Right(states) => this.withStates(states)
    }

    // for tests
    val isBottom: Boolean

    /* the permission system doesn't allow arbitrary aliasing of a reference
     * typed as [t]: aliasing forces one of the resulting types to be
     * [residualType(t)] instead */
    def residualType(mode: OwnershipConsumptionMode): ObsidianType

    def topPermissionType: ObsidianType

    def isOwned = false

    def isAssetReference(contextContractTable: ContractTable): Possibility = No()

    def baseTypeName: String = toString

    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType
}

object ObsidianType {
    def lookupState(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType])(state: String): Set[String] = {
        stateSubstitutions(genericParams, actualParams).get(state) match {
            case Some(permOrState) => permOrState match {
                case Left(perm) => Set()
                case Right(value) => value
            }
            case None => Set(state)
        }
    }

    def stateSubstitutions(genericParams: Seq[GenericType],
                           actualParams: Seq[ObsidianType]): Map[String, Either[Permission, Set[String]]] = {
        val t = for ((genParam, actualParam) <- genericParams.zip(actualParams)) yield {
            genParam.gVar.permissionVar match {
                case Some(varName) => (varName, actualParam.permissionOrState) :: Nil
                case None => Nil
            }
        }

        t.flatten.toMap
    }

    def substituteVarName(contractName: String,
                          genericParams: Seq[GenericType],
                          actualParams: Seq[ObsidianType]): Option[ObsidianType] = {
        // TODO GENERIC: Is this actually the right approach. What if the name conflicts? Should we just substitute anyway or warn?
        var idx = genericParams.indexWhere(p => p.gVar.varName == contractName)

        // If idx is too large, then we will get an error elsewhere
        if (idx >= 0 && idx < actualParams.length) {
            Some(actualParams(idx))
        } else {
            None
        }
    }

    def requireNonPrimitive(typ: ObsidianType): NonPrimitiveType = {
        typ match {
            case np: NonPrimitiveType => np

            // TODO GENERIC: Improve this message, rpobably should log a real error
            case t => assert(false, s"A nonprimitive type is required, but found: $t"); null
        }
    }
}

/* int, bool, or string */
sealed trait PrimitiveType extends ObsidianType {
    val isBottom: Boolean = false
    override def residualType(mode: OwnershipConsumptionMode): ObsidianType = this
    override val topPermissionType: ObsidianType = this

    // Substituting for primitives does nothing
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType = this

    override def withStates(stateNames: Set[String]): PrimitiveType = this
    override def withPermission(permission: Permission): PrimitiveType = this

    // All primitive types are treated as owned
    override def permissionOrState: Either[Permission, Set[String]] = Left(Owned())

    override def withParams(contractParams: List[GenericType]): ObsidianType = this
}

/* all permissioned types are associated with their corresponding symbol table
 * These types were generated by resolution; they are not generated by the parser.
 */
sealed trait NonPrimitiveType extends ObsidianType {
    val isBottom: Boolean = false

    val isRemote: Boolean = false
    val permission: Permission

    val contractName: String

    def withParams(contractParams: List[GenericType]): NonPrimitiveType

    override def isOwned: Boolean = permission == Owned()

    def contractType: ContractType = ContractType(contractName, genericParams)
    def genericParams: Seq[ObsidianType]

    override def withStates(stateNames: Set[String]): NonPrimitiveType
    override def withPermission(permission: Permission): NonPrimitiveType

    override def baseTypeName: String = contractName

    //    override def toString: String = {
    //        val modifiersString = modifiers.map(m => m.toString).mkString(" ")
    //
    //        if (modifiers.size > 0) {
    //            modifiersString + " " + t.toString
    //        }
    //        else {
    //            t.toString
    //        }
    //    }

    //    override def equals(other: Any): Boolean = {
    //        other match {
    //            case NonPrimitiveType(typ, mod) => typ == t && mod == modifiers
    //            case _ => false
    //        }
    //    }
    //    override def hashCode(): Int = t.hashCode()
    //    val residualType: ObsidianType = if (modifiers.contains(IsOwned()))
    //        NonPrimitiveType(t, modifiers - IsOwned() + IsReadOnlyState())
    //    else this

    def topPermissionType = this

    override def isAssetReference(contextContractTable: ContractTable): Possibility = {
        contextContractTable.lookupContract(contractName) match {
            case Some(contract) => {
                if (contract.contract.isAsset) {
                    Yes()
                } else {
                    StateType(contract.contractType, contract.possibleStates, isRemote).isAssetReference(contextContractTable)
                }
            }

            case None => No()
        }
    }

    def remoteType: NonPrimitiveType
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

    override def withStates(stateNames: Set[String]): ObsidianType = BottomType()
    override def withPermission(permission: Permission): ObsidianType = BottomType()

    override def permissionOrState: Either[Permission, Set[String]] = Left(Owned())

    override def withParams(contractParams: List[GenericType]): ObsidianType = this
}

// TODO GENERIC: Does this need to be extended for generics?
case class FFIInterfaceContractType(name: String, simpleType: NonPrimitiveType) extends NonPrimitiveType {
    override def toString: String = name
    override val isBottom: Boolean = false
    override def residualType(mode: OwnershipConsumptionMode): NonPrimitiveType = this
    override def topPermissionType: NonPrimitiveType = this
    override val contractName: String = name
    override val permission: Permission = simpleType.permission
    override def remoteType: NonPrimitiveType = this

    override def genericParams: Seq[ObsidianType] = simpleType.genericParams

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType = {
        simpleType.substitute(genericParams, actualParams) match {
            case np: NonPrimitiveType => FFIInterfaceContractType(name, np)

            case other => ObsidianType.substituteVarName(contractName, genericParams, actualParams).getOrElse(this)
        }
    }

    override def withStates(stateNames: Set[String]): NonPrimitiveType =
        copy(simpleType = simpleType.withStates(stateNames))

    override def withPermission(permission: Permission): NonPrimitiveType =
        copy(simpleType = simpleType.withPermission(permission))

    override def permissionOrState: Either[Permission, Set[String]] = simpleType.permissionOrState

    override def withParams(contractParams: List[GenericType]): NonPrimitiveType =
        this.copy(simpleType = simpleType.withParams(contractParams))
}

sealed trait GenericBound {
    def permissionOrState: Either[Permission, Set[String]]

    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GenericBound

    def withStates(stateNames: Set[String]): GenericBound
    def withPermission(permission: Permission): GenericBound

    def interfaceName: String
    def interfaceParams: Seq[ObsidianType]
    def permission: Permission
    def residualType(mode: OwnershipConsumptionMode): GenericBound

    def contractType: ContractType = ContractType(interfaceName, interfaceParams)

    // TODO GENERIC: Should isRemote be false or should it be part of the bound?
    def referenceType: ObsidianType = ContractReferenceType(contractType, permission, isRemote = false)
}

case class GenericBoundPerm(interfaceName: String, interfaceParams: Seq[ObsidianType], permission: Permission) extends GenericBound {
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
        GenericBoundStates(interfaceName, interfaceParams, stateNames)

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GenericBound =
        this.copy(interfaceParams = interfaceParams.map(_.substitute(genericParams, actualParams)))

    override def permissionOrState: Either[Permission, Set[String]] = Left(permission)
}

// TODO GENERIC: Should these params be strings or actual types?
case class GenericBoundStates(interfaceName: String, interfaceParams: Seq[ObsidianType], states: Set[String]) extends GenericBound {
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

            GenericBoundPerm(interfaceName, interfaceParams, newPermission)
        }

    override def withPermission(permission: Permission): GenericBound =
        if (permission == Owned()) {
            this
        } else {
            GenericBoundPerm(interfaceName, interfaceParams, permission)
        }

    override def withStates(stateNames: Set[String]): GenericBound = copy(states = stateNames)

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GenericBound =
        this.copy(interfaceParams = interfaceParams.map(_.substitute(genericParams, actualParams)))

    override def permissionOrState: Either[Permission, Set[String]] = Right(states)
}

case class GenericVar(isAsset: Boolean, varName: String, permissionVar: Option[String]) {
    def hasPermissionVar(stateName: String): Boolean = permissionVar.contains(stateName)
}

case class GenericType(gVar: GenericVar, bound: GenericBound) extends NonPrimitiveType {
    def hasPermissionVar(stateName: String): Boolean = gVar.hasPermissionVar(stateName)

    // TODO GENERIC: How should these be implemented

    override val permission: Permission = bound.permission
    override val contractName: String = bound.interfaceName

    override def remoteType: NonPrimitiveType = this

    // TODO GENERIC: Uh what should this be
    override def residualType(mode: OwnershipConsumptionMode): ObsidianType =
        GenericType(gVar, bound.residualType(mode))

    def lookupInterface(contractTable: ContractTable): Option[ContractTable] =
        // TODO GENERIC: Does this work?
        // TODO GENERIC: Allow the default of not including an interface at all
        contractTable.lookupContract(bound.interfaceName)

    override def isAssetReference(contextContractTable: ContractTable): Possibility =
        Possibility.fromBoolean(gVar.isAsset)

    override def contractType: ContractType = bound.contractType

    // TODO GENERIC: what to do here??
    override def genericParams: Seq[ObsidianType] = Nil

    // TODO GENERIC: Is there more substitution that needs to be done here or is this sufficient
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianType = {
        val idx = genericParams.indexWhere(_.gVar.varName == this.gVar.varName)

        // if idx is greater, we'll get an error elsewhere
        if (idx >= 0 && idx < actualParams.length) {
            val permOrStates = bound.permissionOrState.map(_.flatMap(ObsidianType.lookupState(genericParams, actualParams)))
            actualParams(idx).withPermOrState(permOrStates)
        } else {
            this.copy(bound = bound.substitute(genericParams, actualParams))
        }
    }

    override def withStates(stateNames: Set[String]): GenericType =
        copy(bound = bound.withStates(stateNames))

    override def withPermission(permission: Permission): GenericType =
        GenericType(gVar, bound.withPermission(permission))

    override def permissionOrState: Either[Permission, Set[String]] =
        bound.permissionOrState

    // TODO GENERIC: Is there something more that should happen here?
    override def withParams(contractParams: List[GenericType]): NonPrimitiveType = this
}

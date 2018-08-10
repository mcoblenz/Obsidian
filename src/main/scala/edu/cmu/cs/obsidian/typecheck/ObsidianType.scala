package edu.cmu.cs.obsidian.typecheck
import edu.cmu.cs.obsidian.parser._


trait Permission
case class Shared() extends Permission {
    override def toString: String = "Shared"
}
case class Owned() extends Permission {
    override def toString: String = "Owned"
}

case class Unowned() extends Permission {
    override def toString: String = "Unowned"
}

case class Inferred() extends Permission {
    override def toString: String = "Inferred"
} // For local variables

trait OwnershipConsumptionMode
case class ConsumingOwnedGivesShared() extends OwnershipConsumptionMode
case class ConsumingOwnedGivesUnowned() extends OwnershipConsumptionMode
case class NoOwnershipConsumption() extends OwnershipConsumptionMode

// Type of references to contracts.
case class ContractReferenceType(contractType: ContractType, permission: Permission, override val isRemote: Boolean) extends NonPrimitiveType {
    override def toString: String = contractName + "@" + permission
    val contractName: String = contractType.contractName

    override def isOwned = permission == Owned()

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
}



// Type of actual contracts. This is ALMOST NEVER the right class; it is specially for actual contracts.
// Almost everywhere will use ContractReferenceType. This intentionally does not extend ObsidianType
// because it is not available in the language itself (for now).
case class ContractType(contractName: String) {
    override def toString: String = contractName
}

trait Possibility
case class Yes() extends Possibility
case class Maybe() extends Possibility
case class No() extends Possibility

/* Invariant: [stateNames] is missing at least one of the states of the
 * contract (i.e. it is more specific than [ContractReferenceType(contractName)],
 * but has at least 2 distinct states
 *
 * StateType is always owned.
 * */
case class StateType(contractName: String, stateNames: Set[String], override val isRemote: Boolean) extends NonPrimitiveType {
    def this(contractName: String, stateName: String, isRemote: Boolean) = {
        this(contractName, Set(stateName), isRemote)
    }

    private def orOfStates: String = stateNames.mkString(" | ")

    override def toString: String = contractName + "@" +
        (if (stateNames.size > 1) "(" + orOfStates + ")" else orOfStates)

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

            ContractReferenceType(ContractType(contractName), newPermission, isRemote).setLoc(this)
        }
    }
    override val topPermissionType: NonPrimitiveType = this

    override def isResourceReference(contextContractTable: ContractTable): Possibility = {
        val contract = contextContractTable.lookupContract(contractName)

        if (contract.isDefined && contract.get.contract.isResource) {
            return Yes()
        }

        val stateIsResource = contract match {
            case None => No() // This will result in an error elsewhere.
            case Some(contractTable) =>
                val statesAreResources = stateNames.map((sn: String) =>
                    contractTable.state(sn) match
                {
                    case None => No()
                    case Some(stateTable) => if(stateTable.ast.isResource) Yes() else No()
                })
                statesAreResources.reduce((p1: Possibility, p2: Possibility) => if (p1 == p2) p1 else Maybe())
        }

        stateIsResource
    }

}

object StateType {
    def apply(contractName: String, stateName: String, isRemote: Boolean): StateType = new StateType(contractName, Set(stateName), isRemote)
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
    // for tests
    val isBottom: Boolean

    /* the permission system doesn't allow arbitrary aliasing of a reference
     * typed as [t]: aliasing forces one of the resulting types to be
     * [residualType(t)] instead */
    def residualType(mode: OwnershipConsumptionMode): ObsidianType

    def topPermissionType: ObsidianType

    def isOwned = false

    def isResourceReference(contextContractTable: ContractTable): Possibility = No()

}

/* int, bool, or string */
sealed trait PrimitiveType extends ObsidianType {
    val isBottom: Boolean = false
    override def residualType(mode: OwnershipConsumptionMode): ObsidianType = this
    override val topPermissionType: ObsidianType = this
}

/* all permissioned types are associated with their corresponding symbol table
 * These types were generated by resolution; they are not generated by the parser.
 */
sealed trait NonPrimitiveType extends ObsidianType {
    val isBottom: Boolean = false

    val isRemote: Boolean = false
    val permission: Permission

    val contractName: String


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

    override def isResourceReference(contextContractTable: ContractTable): Possibility = {
        val contract = contextContractTable.lookupContract(contractName)
        if (contract.isDefined && contract.get.contract.isResource) {
            Yes()
        }
        else {
            No()
        }
    }
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
}

// Only appears before running resolution, which happens right after parsing.
case class UnresolvedNonprimitiveType(identifiers: Seq[String], permission: Permission) extends ObsidianType {
    val isBottom: Boolean = false

    override def toString: String  = identifiers.mkString(".")


    override def residualType(mode: OwnershipConsumptionMode): ObsidianType = this // Should never be invoked
    override def topPermissionType: ObsidianType = this
}

case class InterfaceContractType(name: String, simpleType: NonPrimitiveType) extends NonPrimitiveType {
    override def toString: String = name
    override val isBottom: Boolean = false
    override def residualType(mode: OwnershipConsumptionMode): NonPrimitiveType = this
    override def topPermissionType: NonPrimitiveType = this
    override val contractName: String = name
    override val permission: Permission = simpleType.permission
}

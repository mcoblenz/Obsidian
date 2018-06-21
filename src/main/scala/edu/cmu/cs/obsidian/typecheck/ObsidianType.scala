package edu.cmu.cs.obsidian.typecheck
import edu.cmu.cs.obsidian.parser._

sealed abstract class TypeModifier() extends HasLocation
case class IsReadOnlyState() extends TypeModifier {
    override def toString: String = "readonlyState"
}

case class IsRemote() extends TypeModifier {
    override def toString: String = "remote"
}

case class IsOwned() extends TypeModifier {
    override def toString: String = "owned"
}

/* [SimpleType] simply indicates a contract, and possibly a state or set of states: there
 * is neither a permission nor a path associated with the type */
sealed trait SimpleType { val contractName: String }

case class JustContractType(contractName: String) extends SimpleType {
    override def toString: String = contractName
}
/* Invariant: [stateNames] is missing at least one of the states of the
 * contract (i.e. it is more specific than [JustContractType(contractName)],
 * but has at least 2 distinct states */
case class StateType(contractName: String, stateNames: Set[String]) extends SimpleType {
    def this(contractName: String, stateName: String) = {
        this(contractName, Set(stateName))
    }

    private def orOfStates: String = stateNames.toSeq.tail.foldLeft(stateNames.head)(
        (prev: String, sName: String) => prev + " | " + sName
    )
    override def toString: String = contractName + "." + "(" + orOfStates + ")"
}

object StateType {
    def apply(contractName: String, stateName: String): StateType = new StateType(contractName, Set(stateName))
}


/* a path starts with either a local variable or "this", but "this" can sometimes be omitted */
//case class PathType(path: Seq[String], ts: SimpleType) extends SimpleType {
//    private def pathAsString = path.foldLeft("")(
//        (prev: String, pathNode: String) => prev + pathNode + "."
//    )
//    override def toString: String = pathAsString + ts.toString
//    override val extractSimpleType: SimpleType = ts
//}

/* Invariant for permissioned types: any path that occurs in the type makes "this" explicit */
sealed trait ObsidianType extends HasLocation {
    // for tests
    val isBottom: Boolean

    /* the permission system doesn't allow arbitrary aliasing of a reference
     * typed as [t]: aliasing forces one of the resulting types to be
     * [residualType(t)] instead */
    val residualType: ObsidianType

    val contractNameOpt: Option[String] = None
    val extractSimpleType: Option[SimpleType]
    def extractModifiers: Set[TypeModifier] = Set.empty

    def isOwned = false
    def isShared = false
    def isReadOnlyState = false
    def isRemote = false

    def isResourceReference(contextContractTable: ContractTable) = false

}

sealed trait PotentiallyUnresolvedType extends ObsidianType
sealed trait ResolvedType extends ObsidianType

/* int, bool, or string */
sealed trait PrimitiveType extends PotentiallyUnresolvedType with ResolvedType {
    val isBottom: Boolean = false
    override val residualType: ObsidianType = this
    override val extractSimpleType: Option[SimpleType] = None
}

/* all permissioned types are associated with their corresponding symbol table
 * These types were generated by resolution; they are not generated by the parser.
 */
case class NonPrimitiveType(t: SimpleType, modifiers: Set[TypeModifier]) extends ResolvedType {
    val isBottom: Boolean = false

    override val contractNameOpt: Option[String] = Some(t.contractName)

    override def toString: String = {
        val modifiersString = modifiers.map(m => m.toString).mkString(" ")

        if (modifiers.size > 0) {
            modifiersString + " " + t.toString
        }
        else {
            t.toString
        }
    }

    override def equals(other: Any): Boolean = {
        other match {
            case NonPrimitiveType(typ, mod) => typ == t && mod == modifiers
            case _ => false
        }
    }
    override def hashCode(): Int = t.hashCode()
    val residualType: ObsidianType = if (modifiers.contains(IsOwned()))
        NonPrimitiveType(t, modifiers - IsOwned() + IsReadOnlyState())
    else this

    val extractSimpleType: Option[SimpleType] = Some(t)
    override val extractModifiers = modifiers

    override def isOwned = modifiers.contains(IsOwned())
    override def isReadOnlyState = modifiers.contains(IsReadOnlyState())
    override def isRemote = modifiers.contains(IsRemote())

    override def isResourceReference(contextContractTable: ContractTable): Boolean = {
        val contract = contextContractTable.lookupContract(t.contractName)
        contract.isDefined && contract.get.contract.isResource
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
/* Used to indicate an error in the type checker when a reasonable type cannot
 * otherwise be inferred */
case class BottomType() extends ResolvedType {
    val isBottom: Boolean = true
    override val residualType: ObsidianType = this
    override val extractSimpleType: Option[SimpleType] = None
}

// Only appears before running resolution, which happens right after parsing.
case class UnresolvedNonprimitiveType(identifiers: Seq[String], mods: Set[TypeModifier]) extends PotentiallyUnresolvedType {
    val isBottom: Boolean = false

    override def toString: String = mods.map(m => m.toString).mkString(" ") + " " + identifiers.mkString(".")


    override val residualType: ObsidianType = this // Should never be invoked
    override val extractSimpleType: Option[SimpleType] = None
}

case class InterfaceContractType(name: String, simpleType: SimpleType) extends ObsidianType {
    override def toString: String = name
    val isBottom: Boolean = false
    override val residualType: ObsidianType = this
    override val extractSimpleType: Option[SimpleType] = Some(simpleType)
}

package CodeGen

import edu.cmu.cs.obsidian.parser._
import com.sun.codemodel.internal._

import scala.collection._

/* aggregates information about a particular state */
case class StateContext(
    /* the AST node representing the state */
    astState: State,
    /* the enum value that represents this state */
    enumVal: JEnumConstant,
    /* the inner class has the fields and methods for the state */
    innerClass: JDefinedClass,
    /* this the the field in the parent class (of type [innerClass]) that is
     * defined when the contract is in this particular state */
    innerClassField: JFieldVar
)

/* a declaration is either defined globally in a contract, or in one or more
 * states of the contract */
sealed trait DeclarationContext[T]
/* a declaration made in the whole contract */
case class GlobalDeclaration[T](decl: T) extends DeclarationContext[T]
/* a declaration made in one or more states */
case class StateSpecificDeclaration[T](declSeq: Seq[(State, T)]) extends DeclarationContext[T]

/* aggregates information about the current state in the translation */
case class TranslationContext(
    /* name of the state we're currently translating */
    currentState: Option[String],
    /* when translating an inner class, this gives us the fully-qualified name */
    contractNameResolutionMap: Map[Contract, String], // maps from Obsidian contracts to fully-qualified Java class names
    protobufOuterClassNames: Map[String, String], // maps from fully-qualified Java class names to protobuf outer class names
                                 

    /* the states of the contract we're currently translating */
    states: Map[String, StateContext],
    /* the state enum class */
    stateEnumClass: Option[JDefinedClass],
    /* a field of the state enum type representing the current state */
    stateEnumField: Option[JFieldVar],
    /* aggregates information about [transaction/function/field] declarations:
     * maps the [tx/fun/field] name to [DeclarationContext]  */
    txLookup: Map[String, DeclarationContext[Transaction]],
    funLookup: Map[String, DeclarationContext[Func]],
    fieldLookup: Map[String, DeclarationContext[Field]]
) {
    /* gets the enum if it exists, fails otherwise */
    def getEnum(stName: String): JEnumConstant = {
        states.get(stName).get.enumVal
    }

    def getProtobufClassName(contract: Contract) : String = {
        val fullyQualifiedClassName = contractNameResolutionMap(contract)
        protobufOuterClassNames(fullyQualifiedClassName) + "." + fullyQualifiedClassName
    }

    def getContainingContract(contract: Contract) : Option[Contract] = {
        // TODO: store a mapping instead of doing this, because this is kind of awful.
        if (contractNameResolutionMap.contains(contract)) {
            val fullyQualifiedName = contractNameResolutionMap(contract)
            val lastPeriod = fullyQualifiedName.lastIndexOf(".")
            if (lastPeriod == -1) {
                None
            }
            else {
                assert(lastPeriod > 0, "contract names should not start with '.'")
                val containingContractFullyQualifiedName = fullyQualifiedName.substring(lastPeriod - 1)
                // Ugh.
                val foundPair = contractNameResolutionMap.find((pair: (Contract, String)) => pair._2.equals(containingContractFullyQualifiedName))
                if (foundPair.isDefined) Some(foundPair.get._1) else None
            }
        }
        else {
            assert(false, "Failed to look up contract " + contract.name)
            None
        }
    }
}

object TranslationContext {
    // For constructing the translation context.
    // TODO: invoke this from a constructor of TranslationContext.
    def contractNameResolutionMapForProgram(program: Program): Map[Contract, String] = {
        val map = mutable.HashMap.empty[Contract, String]

        def addClassesToMap(map: mutable.HashMap[Contract, String], contract: Contract, outerClassPath: String): Unit = {
            val name = contract.name

            val newClassPath = if (outerClassPath.length == 0) name else outerClassPath + "." + name
            map += (contract -> newClassPath)

            for (c <- contract.declarations if c.isInstanceOf[Contract]) {
                val innerContract = c.asInstanceOf[Contract]

                addClassesToMap(map, innerContract, newClassPath)
            }
        }

        for (c <- program.contracts) {
            addClassesToMap(map, c, "")
        }

        map
    }
}
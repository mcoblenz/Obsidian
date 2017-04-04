package CodeGen

import edu.cmu.cs.obsidian.parser._
import com.helger.jcodemodel._

import scala.collection._

/* aggregates information about a particular state.
 * INVARIANT: the  */
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

sealed trait FieldInfo
/* a field made in the whole contract. */
case class GlobalFieldInfo(decl: Field) extends FieldInfo
/* a field made in one or more states.
 * This has getter and setter methods that do dynamic checks on state  */
case class StateSpecificFieldInfo(declSeq: Seq[(State, Field)],
                                     getFunc: JMethod, setFunc: JMethod) extends FieldInfo

sealed trait FuncInfo
case class GlobalFuncInfo(decl: Func) extends FuncInfo
case class StateSpecificFuncInfo(declSeq: Seq[(State, Func)], dynamicCheckMethod: JMethod) extends FuncInfo

sealed trait TransactionInfo
case class GlobalTransactionInfo(decl: Transaction) extends TransactionInfo
case class StateSpecificTransactionInfo(declSeq: Seq[(State, Transaction)],
                                           dynamicCheckMethod: JMethod) extends TransactionInfo



/* aggregates information about the current state in the translation */
case class TranslationContext(
    /* the contract currently being translated */
    contract: Contract,
    /* the class of the _contract_ that is currently being translated (i.e. doesn't include states) */
    contractClass: JDefinedClass,
    /* when translating an inner class, this gives us the fully-qualified name */
    contractNameResolutionMap: Map[Contract, String], // maps from Obsidian contracts to fully-qualified Java class names
    protobufOuterClassNames: Map[String, String], // maps from fully-qualified Java class names to protobuf outer class names
    /* the states of the contract we're currently translating */
    states: Map[String, StateContext],
    /* the state currently being translated, if there is such a state */
    currentStateName: Option[String],
    /* the state enum class */
    stateEnumClass: Option[JDefinedClass],
    /* a field of the state enum type representing the current state */
    stateEnumField: Option[JFieldVar],
    /* aggregates information about [transaction/function/field] declarations:
     * maps the [tx/fun/field] name to useful data  */
    txLookup: Map[String, TransactionInfo],
    funLookup: Map[String, FuncInfo],
    fieldLookup: Map[String, FieldInfo]
) {
    /* gets the enum if it exists, fails otherwise */
    def getEnum(stName: String): JEnumConstant = {
        states.get(stName).get.enumVal
    }

    /* assigns a variable (can be either a field or a local variable/argument).
     * Because of JCodeModel's design, [assignField] appends the code onto [body]. */
    def assignVariable(name: String, assignExpr: IJExpression, body: JBlock): Unit = {
        (currentStateName, fieldLookup(name)) match {
            /* if "f" is defined in the outer class, "this.f" isn't a valid reference of the field
             * from inside the inner class. Note that it's not sufficient to just use "f", because
             * this makes shadowing of this variable impossible */
            case (None, GlobalFieldInfo(_)) => body.assign(JExpr._this().ref(name), assignExpr)
            case (Some(_), GlobalFieldInfo(_)) => body.assign(contractClass.staticRef("this").ref(name), assignExpr)
            case (_, StateSpecificFieldInfo(_, _, setFunc)) => body.invoke(setFunc).arg(assignExpr)
        }
    }

    /* just like [assignVariable], but this dereferences a variable */
    def dereferenceVariable(f: String): IJExpression = {
        (currentStateName, fieldLookup(f)) match {
            /* same note in [assignVariable] above applies here as well */
            case (None, GlobalFieldInfo(_)) => JExpr._this().ref(f)
            case (Some(_), GlobalFieldInfo(_)) => contractClass.staticRef("this").ref(f)
            case (_, StateSpecificFieldInfo(_, getFunc, _)) => JExpr.invoke(getFunc)
        }
    }

    /* does one of the below; checks first to see which one is possible */
    def invokeTransactionOrFunction(name: String): JInvocation = {
        if (txLookup.get(name).isDefined) {
            invokeTransaction(name)
        } else {
            invokeFunction(name)
        }
    }

    def invokeTransaction(name: String): JInvocation = {
        txLookup(name) match {
            case GlobalTransactionInfo(_) => JExpr.invoke(name)
            case StateSpecificTransactionInfo(_, meth) => JExpr.invoke(meth)
        }
    }

    def invokeFunction(name: String): JInvocation = {
        funLookup(name) match {
            case GlobalFuncInfo(_) => JExpr.invoke(name)
            case StateSpecificFuncInfo(_, meth) => JExpr.invoke(meth)
        }
    }

    /* the set of fields that defined in both states. Note: this does not include fields that are defined
     * in the contract as a whole */
    def conservedFields(stNameFrom: String, stNameTo: String) : Set[Field] = {
        val getFields = (stName: String) =>
            states(stName).astState.declarations.filter(_.isInstanceOf[Field]).map(_.asInstanceOf[Field])
        val (fromFields, toFields) = (getFields(stNameFrom), getFields(stNameTo))

        def conservedFilter(fieldPrime: Field): Boolean = { toFields.exists(_.fieldName == fieldPrime.fieldName) }

        /* take only the fields that are conserved */
        fromFields.filter(conservedFilter).toSet
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
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
    outerClassNames: Option[List[String]],
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
}
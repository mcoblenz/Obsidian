package CodeGen

import edu.cmu.cs.obsidian.parser._
import com.sun.codemodel.internal._

import scala.collection._

/* aggregates information about a particular state */
case class StateContext(
    /* the AST node representing the state */
    astState: State,
    /* the enum value that represents this state */
    enumVal: JExpr,
    /* the inner */
    innerClass: JDefinedClass,
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
    stateEnumClass: JDefinedClass,
    /* a field of the state enum type representing the current state */
    stateEnumField: JFieldVar,
    /* aggregates information about [transaction/function/field] declarations:
     * maps the [tx/fun/field] name to [DeclarationContext]  */
    txLookup: Map[String, DeclarationContext[Transaction]],
    funLookup: Map[String, DeclarationContext[Func]],
    fieldLookup: Map[String, DeclarationContext[Field]]
)
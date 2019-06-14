package edu.cmu.cs.obsidian.parser

import scala.util.parsing.input.{NoPosition, Position}
import edu.cmu.cs.obsidian.lexer.Token
import edu.cmu.cs.obsidian.parser.Parser.{EndsInState, Identifier}
import edu.cmu.cs.obsidian.typecheck._

trait HasLocation {
    var loc: Position = NoPosition
    def setLoc(t: Token): this.type = { loc = t.pos; this }
    def setLoc(other: HasLocation): this.type = { loc = other.loc; this }
    def setLoc(id: (String, Position)): this.type = { loc = id._2; this }
    def setLoc(pos: Position): this.type = { loc = pos; this }
}

sealed abstract class AST() extends HasLocation

sealed abstract class Statement() extends AST

/* All expressions are statements. We relegate the pruning of expressions
 * that don't have effects to a later analysis */
sealed abstract class Expression() extends Statement

/* this is to circumnavigate type erasure: it makes it possible to match on the exact
 * type of a Declarations at runtime */
sealed trait DeclarationTag
object TypeDeclTag extends DeclarationTag
object FieldDeclTag extends DeclarationTag
object ContractDeclTag extends DeclarationTag
object StateDeclTag extends DeclarationTag
object ConstructorDeclTag extends DeclarationTag
object TransactionDeclTag extends DeclarationTag

sealed abstract class Declaration() extends AST {
    val name: String
    val tag: DeclarationTag
}

sealed abstract class InvokableDeclaration() extends Declaration {
    val args: Seq[VariableDeclWithSpec]
    val retType: Option[ObsidianType]
    val body: Seq[Statement]
    val thisType: ObsidianType
    val thisFinalType: ObsidianType
    val isStatic: Boolean
}

/* Expressions */
case class ReferenceIdentifier(name: String) extends Expression {
    override val toString = name
}

case class NumLiteral(value: Int) extends Expression
case class StringLiteral(value: String) extends Expression
case class TrueLiteral() extends Expression
case class FalseLiteral() extends Expression
case class This() extends Expression {
    override def toString: String = "this"
}
case class Parent() extends Expression
case class Conjunction(e1: Expression, e2: Expression) extends Expression
case class Disjunction(e1: Expression, e2: Expression) extends Expression
case class LogicalNegation(e: Expression) extends Expression
case class Add(e1: Expression, e2: Expression) extends Expression
case class Subtract(e1: Expression, e2: Expression) extends Expression
case class Divide(e1: Expression, e2: Expression) extends Expression
case class Multiply(e1: Expression, e2: Expression) extends Expression
case class Mod(e1: Expression, e2: Expression) extends Expression
case class Negate(e: Expression) extends Expression
case class Equals(e1: Expression, e2: Expression) extends Expression
case class GreaterThan(e1: Expression, e2: Expression) extends Expression
case class GreaterThanOrEquals(e1: Expression, e2: Expression) extends Expression
case class LessThan(e1: Expression, e2: Expression) extends Expression
case class LessThanOrEquals(e1: Expression, e2: Expression) extends Expression
case class NotEquals(e1: Expression, e2: Expression) extends Expression
case class Dereference(e: Expression, f: String) extends Expression {
    override def toString: String = {
        s"$e.$f"
    }
}

case class LocalInvocation(name: String, args: Seq[Expression]) extends Expression
case class Invocation(recipient: Expression, name: String, args: Seq[Expression], isFFIInvocation: Boolean) extends Expression {
    override def toString: String = {
        val argString = args.mkString(",")
        s"$recipient.$name($argString)"
    }
}
case class Construction(name: String, args: Seq[Expression], isFFIInvocation: Boolean) extends Expression
case class Disown(e: Expression) extends Expression
case class StateInitializer(stateName: Identifier, fieldName: Identifier) extends Expression

/* statements and control flow constructs */
case class VariableDecl(typ: ObsidianType, varName: String) extends Statement
case class VariableDeclWithInit(typ: ObsidianType, varName: String, e: Expression) extends Statement

case class VariableDeclWithSpec(typIn: ObsidianType, typOut: ObsidianType, varName: String) extends Statement

case class Return() extends Statement
case class ReturnExpr(e: Expression) extends Statement

// We distinguish between no update clause given and an empty update clause for a clean separation between syntax and semantics.
case class Transition(newStateName: String, updates: Option[Seq[(ReferenceIdentifier, Expression)]], thisPermission: Permission) extends Statement
case class Assignment(assignTo: Expression, e: Expression) extends Statement
case class Revert(maybeExpr: Option[Expression]) extends Statement
case class If(eCond: Expression, s: Seq[Statement]) extends Statement
case class IfThenElse(eCond: Expression, s1: Seq[Statement], s2: Seq[Statement]) extends Statement
case class IfInState(e: Expression, state: Identifier, s1: Seq[Statement], s2: Seq[Statement]) extends Statement
case class TryCatch(s1: Seq[Statement], s2: Seq[Statement]) extends Statement
case class Switch(e: Expression, cases: Seq[SwitchCase]) extends Statement
case class SwitchCase(stateName: String, body: Seq[Statement]) extends AST
case class StaticAssert(expr: Expression, statesOrPermissions: Seq[Identifier]) extends Statement

/* Declarations */
case class TypeDecl(name: String, typ: ObsidianType) extends Declaration {
    val tag: DeclarationTag = TypeDeclTag
}

sealed trait IsAvailableInStates {
    def availableIn: Option[Set[String]]
}

case class Field(isConst: Boolean,
                 typ: ObsidianType,
                 name: String,
                 availableIn: Option[Set[String]]) extends Declaration with IsAvailableInStates {
    val tag: DeclarationTag = FieldDeclTag
}

case class Constructor(name: String,
                       args: Seq[VariableDeclWithSpec],
                       resultType: NonPrimitiveType,
                       body: Seq[Statement]) extends InvokableDeclaration {
    val retType: Option[ObsidianType] = None
    val tag: DeclarationTag = ConstructorDeclTag
    val thisType: ObsidianType = resultType
    val thisFinalType: ObsidianType = resultType
    val isStatic: Boolean = false
}

case class Transaction(name: String,
                       args: Seq[VariableDeclWithSpec],
                       retType: Option[ObsidianType],
                       ensures: Seq[Ensures],
                       body: Seq[Statement],
                       isStatic: Boolean,
                       isPrivate: Boolean,
                       thisType: NonPrimitiveType,
                       thisFinalType: NonPrimitiveType,
                       initialFieldTypes: Map[String, ObsidianType] = Map.empty, // populated after parsing
                       finalFieldTypes: Map[String, ObsidianType] = Map.empty // populated after parsing
                      ) extends InvokableDeclaration with IsAvailableInStates {
    val tag: DeclarationTag = TransactionDeclTag

    def availableIn: Option[Set[String]] = thisType match {
        case StateType(_, stateNames, _) => Some(stateNames)
        case _ => None
    }
}

case class FSMEdge (fromState: Identifier, toState: Identifier) extends AST;
case class Transitions(edges: Seq[FSMEdge]) extends AST;


case class State(name: String, fields: Seq[Field], isAsset: Boolean) extends Declaration {
    val tag: DeclarationTag = StateDeclTag
}

case class Ensures(expr: Expression) extends AST

sealed abstract trait ContractModifier extends HasLocation
case class IsAsset() extends ContractModifier
case class IsMain() extends ContractModifier
case class IsImport() extends ContractModifier

case class Import(name: String) extends AST

/* Layer */
sealed abstract class Contract(name: String, val sourcePath: String) extends Declaration {
    def declarations: Seq[Declaration]
    def modifiers: Set[ContractModifier] = Set.empty
    val isAsset = modifiers.contains(IsAsset())
    val isMain = modifiers.contains(IsMain())
    val isImport = modifiers.contains(IsImport())
}

case class ObsidianContractImpl(override val modifiers: Set[ContractModifier],
                    name: String, override val declarations: Seq[Declaration],
                    transitions: Option[Transitions],
                    isInterface: Boolean,
                    sp: String) extends Contract (name, sp) {
    val tag: DeclarationTag = ContractDeclTag
}

/* FFI contract for Java */
case class JavaFFIContractImpl(name: String,
                               interface: String,
                               javaPath: Seq[Identifier],
                               sp: String,
                               override val declarations: Seq[Declaration] = Seq.empty) extends Contract(name, sp){
    val tag: DeclarationTag = ContractDeclTag
}


/* Program */
case class Program(imports: Seq[Import], contracts: Seq[Contract]) extends AST

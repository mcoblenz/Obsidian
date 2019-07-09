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


// TODO GENERIC: Should we split substitute off into a trait? Maybe not, since the ObsidianType substitute has a different return type (e.g., generic Obsidian type)
sealed abstract class Statement() extends AST {
    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Statement
}

/* All expressions are statements. We relegate the pruning of expressions
 * that don't have effects to a later analysis */
sealed abstract class Expression() extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Expression
}

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

    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Declaration
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

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ReferenceIdentifier = this
}

case class NumLiteral(value: Int) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): NumLiteral = this
}
case class StringLiteral(value: String) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): StringLiteral = this
}
case class TrueLiteral() extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): TrueLiteral = this
}
case class FalseLiteral() extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): FalseLiteral = this
}
case class This() extends Expression {
    override def toString: String = "this"

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): This = this
}
case class Parent() extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Parent = this
}
case class Conjunction(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Conjunction =
        Conjunction(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Disjunction(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Disjunction =
        Disjunction(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class LogicalNegation(e: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): LogicalNegation =
        LogicalNegation(e.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Add(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Add =
        Add(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Subtract(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Subtract =
        Subtract(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Divide(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Divide =
        Divide(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Multiply(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Multiply =
        Multiply(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Mod(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Mod =
    // TODO GENERIC: Will also need to handle the state substitution at some point
        Mod(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Negate(e: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Negate =
        Negate(e.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Equals(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Equals =
        Equals(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class GreaterThan(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GreaterThan =
        GreaterThan(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class GreaterThanOrEquals(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): GreaterThanOrEquals =
        GreaterThanOrEquals(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class LessThan(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): LessThan =
        LessThan(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class LessThanOrEquals(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): LessThanOrEquals =
        LessThanOrEquals(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class NotEquals(e1: Expression, e2: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): NotEquals =
        NotEquals(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Dereference(e: Expression, f: String) extends Expression {
    override def toString: String = {
        s"$e.$f"
    }

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Dereference =
        Dereference(e.substitute(genericParams, actualParams), f).setLoc(this)
}

case class LocalInvocation(name: String, args: Seq[Expression]) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): LocalInvocation =
        LocalInvocation(name, args.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}
case class Invocation(recipient: Expression, name: String, args: Seq[Expression], isFFIInvocation: Boolean) extends Expression {
    override def toString: String = {
        val argString = args.mkString(",")
        s"$recipient.$name($argString)"
    }

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Invocation =
        Invocation(recipient.substitute(genericParams, actualParams), name,
            args.map(_.substitute(genericParams, actualParams)), isFFIInvocation)
            .setLoc(this)
}
case class Construction(contractType: ContractType, args: Seq[Expression], isFFIInvocation: Boolean) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Construction =
        Construction(contractType.substitute(genericParams, actualParams),
            args.map(_.substitute(genericParams, actualParams)), isFFIInvocation)
            .setLoc(this)
}
case class Disown(e: Expression) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Disown =
        Disown(e.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class StateInitializer(stateName: Identifier, fieldName: Identifier) extends Expression {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): StateInitializer = this
}

/* statements and control flow constructs */
case class VariableDecl(typ: ObsidianType, varName: String) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): VariableDecl =
        VariableDecl(typ.substitute(genericParams, actualParams), varName)
            .setLoc(this)
}
case class VariableDeclWithInit(typ: ObsidianType, varName: String, e: Expression) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): VariableDeclWithInit =
        VariableDeclWithInit(typ.substitute(genericParams, actualParams), varName,
            e.substitute(genericParams, actualParams))
            .setLoc(this)
}

case class VariableDeclWithSpec(typIn: ObsidianType, typOut: ObsidianType, varName: String) extends Statement {
    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): VariableDeclWithSpec =
        VariableDeclWithSpec(typIn.substitute(genericParams, actualParams),
            typOut.substitute(genericParams, actualParams), varName)
            .setLoc(this)
}

case class Return() extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Return = this
}
case class ReturnExpr(e: Expression) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ReturnExpr =
        ReturnExpr(e.substitute(genericParams, actualParams))
            .setLoc(this)
}

// We distinguish between no update clause given and an empty update clause for a clean separation between syntax and semantics.
case class Transition(newStateName: String, updates: Option[Seq[(ReferenceIdentifier, Expression)]], thisPermission: Permission) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Transition = {
        // TODO GENERIC: We may need to do some substitute/lookup here for the state variables
        def doSubstitute: ((ReferenceIdentifier, Expression)) => (ReferenceIdentifier, Expression) = {
            case (id, expr) => (id, expr.substitute(genericParams, actualParams))
        }

        // TODO GENERIC: May have to do something here for the permission substitution
        Transition(newStateName, updates.map(_.map(doSubstitute)), thisPermission)
            .setLoc(this)
    }
}
case class Assignment(assignTo: Expression, e: Expression) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Assignment =
        Assignment(assignTo.substitute(genericParams, actualParams), e.substitute(genericParams, actualParams))
            .setLoc(this)
}
case class Revert(maybeExpr: Option[Expression]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Revert =
        Revert(maybeExpr.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}
case class If(eCond: Expression, s: Seq[Statement]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): If =
        If(eCond.substitute(genericParams, actualParams), s.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}
case class IfThenElse(eCond: Expression, s1: Seq[Statement], s2: Seq[Statement]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): IfThenElse =
        IfThenElse(eCond.substitute(genericParams, actualParams),
            s1.map(_.substitute(genericParams, actualParams)),
            s2.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}
// TODO GENERIC: will need to handle state variables here
case class IfInState(e: Expression, state: Identifier, s1: Seq[Statement], s2: Seq[Statement]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): IfInState =
        IfInState(e.substitute(genericParams, actualParams),
            state,
            s1.map(_.substitute(genericParams, actualParams)),
            s2.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}
case class TryCatch(s1: Seq[Statement], s2: Seq[Statement]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): TryCatch =
        TryCatch(s1.map(_.substitute(genericParams, actualParams)), s2.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}
case class Switch(e: Expression, cases: Seq[SwitchCase]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Switch =
        Switch(e.substitute(genericParams, actualParams),
            cases.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}
case class SwitchCase(stateName: String, body: Seq[Statement]) extends AST {
    // TODO GENERIC: will need to handle state variables here
    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): SwitchCase =
        SwitchCase(stateName, body.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}

// TODO GENERIC: Will need to handle state variables here
case class StaticAssert(expr: Expression, statesOrPermissions: Seq[Identifier]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): StaticAssert =
        StaticAssert(expr.substitute(genericParams, actualParams), statesOrPermissions)
            .setLoc(this)
}

/* Declarations */
case class TypeDecl(name: String, typ: ObsidianType) extends Declaration {
    val tag: DeclarationTag = TypeDeclTag

    // TODO GENERIC: Does this still get used/matter?
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): TypeDecl = ???
}

sealed trait IsAvailableInStates {
    def availableIn: Option[Set[String]]
}

case class Field(isConst: Boolean,
                 typ: ObsidianType,
                 name: String,
                 availableIn: Option[Set[String]]) extends Declaration with IsAvailableInStates {
    val tag: DeclarationTag = FieldDeclTag

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Field = {
        val idx = genericParams.indexWhere(_ == typ)

        if (idx >= 0) {
            Field(isConst, actualParams(idx), name, availableIn)
                .setLoc(this)
        } else {
            Field(isConst, typ.substitute(genericParams, actualParams), name, availableIn)
                .setLoc(this)
        }
    }
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

    // TODO GENERIC: Should probably give an error when we need to (e.g., when substitution is not appropriate)
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Constructor = {
        Constructor(name, args.map(_.substitute(genericParams, actualParams)),
            ObsidianType.requireNonPrimitive(resultType.substitute(genericParams, actualParams)),
            body.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
    }
}

// TODO GENERICS: Add generic parameters on transactions
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

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Transaction =
        Transaction(name,
            args.map(_.substitute(genericParams, actualParams)),
            retType.map(_.substitute(genericParams, actualParams)),
            // TODO GENERIC: Seems like this doesn't do anything, but just in case
            ensures.map(_.substitute(genericParams, actualParams)),
            body.map(_.substitute(genericParams, actualParams)),
            isStatic,
            isPrivate,
            ObsidianType.requireNonPrimitive(thisType.substitute(genericParams, actualParams)),
            ObsidianType.requireNonPrimitive(thisFinalType.substitute(genericParams, actualParams)),
            initialFieldTypes.mapValues(_.substitute(genericParams, actualParams)),
            finalFieldTypes.mapValues(_.substitute(genericParams, actualParams)))
            .setLoc(this)

    def declarationStr: String =
        retType match {
            case Some(typ) =>
                s"transaction $name(${args.mkString(", ")}) returns $typ;"
            case None =>
                s"transaction $name(${args.mkString(", ")});"
        }
}

case class FSMEdge (fromState: Identifier, toState: Identifier) extends AST;
case class Transitions(edges: Seq[FSMEdge]) extends AST;


case class State(name: String, fields: Seq[Field], isAsset: Boolean) extends Declaration {
    val tag: DeclarationTag = StateDeclTag

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): State =
        State(name, fields.map(_.substitute(genericParams, actualParams)), isAsset)
            .setLoc(this)
}

case class Ensures(expr: Expression) extends AST {
    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Ensures =
        Ensures(expr.substitute(genericParams, actualParams))
            .setLoc(this)
}

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

    def isInterface: Boolean
    def bound: GenericBound
}

case class ObsidianContractImpl(override val modifiers: Set[ContractModifier],
                    name: String, params: Seq[GenericType], implementBound: GenericBound,
                    override val declarations: Seq[Declaration],
                    transitions: Option[Transitions],
                    isInterface: Boolean,
                    sp: String) extends Contract (name, sp) {
    val tag: DeclarationTag = ContractDeclTag

    // TODO GENERIC: Should we remove the generic parameters, now that we've substituted for them?
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianContractImpl =
        ObsidianContractImpl(modifiers, name, params, implementBound,
            declarations.map(_.substitute(genericParams, actualParams)),
            transitions,
            isInterface, // TODO GENERIC: Should this ever be called for an interface?
            sp).setLoc(this)

    override def bound: GenericBound = implementBound
}

/* FFI contract for Java */
// TODO GENERIC: How do our generics interact with Java's? Probably just rely on the interface to get the types right?
case class JavaFFIContractImpl(name: String,
                               interface: String,
                               javaPath: Seq[Identifier],
                               sp: String,
                               override val declarations: Seq[Declaration] = Seq.empty) extends Contract(name, sp){
    val tag: DeclarationTag = ContractDeclTag

    // TODO GENERIC: Maybe this should actually do something
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): JavaFFIContractImpl = this
    override def bound: GenericBound = GenericBoundPerm("Top", Nil, Unowned())
    override def isInterface: Boolean = false
}


/* Program */
case class Program(imports: Seq[Import], contracts: Seq[Contract]) extends AST

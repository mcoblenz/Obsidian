package edu.cmu.cs.obsidian.parser

import edu.cmu.cs.obsidian.lexer.Token
import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.typecheck._

import scala.util.parsing.input.{NoPosition, Position}

trait HasLocation {
    var loc: Position = NoPosition

    def setLoc(t: Token): this.type = {
        loc = t.pos
        this
    }

    def setLoc(other: HasLocation): this.type = {
        loc = other.loc
        this
    }

    def setLoc(id: (String, Position)): this.type = {
        loc = id._2
        this
    }

    def setLoc(pos: Position): this.type = {
        loc = pos
        this
    }
}

sealed abstract class AST() extends HasLocation


sealed abstract class Statement() extends AST {
    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Statement
}

/* All expressions are statements. We relegate the pruning of expressions
 * that don't have effects to a later analysis */
sealed abstract class Expression() extends Statement {
    val obstype: Option[ObsidianType]

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
    val initialFieldTypes: Map[String, ObsidianType] = Map.empty
    val finalFieldTypes: Map[String, ObsidianType] = Map.empty

    def bodyEnd: AST =
        if (body.nonEmpty) {
            body.last
        } else {
            this
        }
}

// Expressions not containing other expressions
sealed abstract class AtomicExpression extends Expression {
    override val obstype: Option[ObsidianType] = None

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): AtomicExpression = this
}

sealed abstract class UnaryExpression(make: Expression => UnaryExpression,
                                      e: Expression) extends Expression {
    override val obstype: Option[ObsidianType] = None

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): UnaryExpression =
        make(e.substitute(genericParams, actualParams)).setLoc(this)
}

sealed abstract class BinaryExpression(make: (Expression, Expression) => BinaryExpression,
                                       e1: Expression,
                                       e2: Expression) extends Expression {
    override val obstype: Option[ObsidianType] = None

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): BinaryExpression =
        make(e1.substitute(genericParams, actualParams), e2.substitute(genericParams, actualParams))
            .setLoc(this)
}

/* Expressions */
case class ReferenceIdentifier(name: String, override val obstype: Option[ObsidianType]) extends AtomicExpression {
    override val toString: String = name
}

case class NumLiteral(value: Int) extends AtomicExpression {
    override val obstype: Option[ObsidianType] = Some(IntType())

    override def toString: String = value.toString
}

case class StringLiteral(value: String) extends AtomicExpression {
    override val obstype: Option[ObsidianType] = Some(StringType())
}

case class TrueLiteral() extends AtomicExpression {
    override val obstype: Option[ObsidianType] = Some(BoolType())

    override def toString: String = "true"
}

case class FalseLiteral() extends AtomicExpression {
    override val obstype: Option[ObsidianType] = Some(BoolType())

    override def toString: String = "false"
}

case class This(override val obstype: Option[ObsidianType]) extends AtomicExpression {

    override def toString: String = "this"
}

case class Parent() extends AtomicExpression //todo type of parent?

case class Conjunction(e1: Expression, e2: Expression) extends BinaryExpression(Conjunction, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class Disjunction(e1: Expression, e2: Expression) extends BinaryExpression(Disjunction, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class LogicalNegation(e: Expression) extends UnaryExpression(LogicalNegation, e) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}


case class Add(e1: Expression, e2: Expression) extends BinaryExpression(Add, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(IntType())
}

case class StringConcat(e1: Expression, e2: Expression) extends BinaryExpression(StringConcat, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(StringType())
}

case class Subtract(e1: Expression, e2: Expression) extends BinaryExpression(Subtract, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(IntType())
}

case class Divide(e1: Expression, e2: Expression) extends BinaryExpression(Divide, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(IntType())
}

case class Multiply(e1: Expression, e2: Expression) extends BinaryExpression(Multiply, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(IntType())
}

case class Mod(e1: Expression, e2: Expression) extends BinaryExpression(Mod, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(IntType())
}

case class Negate(e: Expression) extends UnaryExpression(Negate, e) {
    override val obstype: Option[ObsidianType] = Some(IntType())
}

case class Equals(e1: Expression, e2: Expression) extends BinaryExpression(Equals, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class GreaterThan(e1: Expression, e2: Expression) extends BinaryExpression(GreaterThan, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class GreaterThanOrEquals(e1: Expression, e2: Expression) extends BinaryExpression(GreaterThanOrEquals, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class LessThan(e1: Expression, e2: Expression) extends BinaryExpression(LessThan, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class LessThanOrEquals(e1: Expression, e2: Expression) extends BinaryExpression(LessThanOrEquals, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class NotEquals(e1: Expression, e2: Expression) extends BinaryExpression(NotEquals, e1, e2) {
    override val obstype: Option[ObsidianType] = Some(BoolType())
}

case class Dereference(e: Expression, f: String) extends UnaryExpression(Dereference(_, f), e) {
    override val obstype: Option[ObsidianType] = e.obstype

    override def toString: String = {
        s"$e.$f"
    }
}

case class LocalInvocation(name: String, genericParams: Seq[GenericType],
                           params: Seq[ObsidianType], args: Seq[Expression], override val obstype: Option[ObsidianType]) extends Expression {

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): LocalInvocation =
        LocalInvocation(name,
            genericParams,
            params.map(_.substitute(genericParams, actualParams)),
            args.map(_.substitute(genericParams, actualParams)), obstype)
            .setLoc(this)

    override def toString: String = s"$name(${args.mkString(",")})"
}

case class Invocation(recipient: Expression, genericParams: Seq[GenericType], params: Seq[ObsidianType],
                      name: String, args: Seq[Expression], isFFIInvocation: Boolean, override val obstype: Option[ObsidianType]) extends Expression {

    override def toString: String = s"$recipient.$name(${args.mkString(",")})"

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Invocation =
        Invocation(recipient.substitute(genericParams, actualParams),
            genericParams,
            params.map(_.substitute(genericParams, actualParams)),
            name,
            args.map(_.substitute(genericParams, actualParams)), isFFIInvocation, obstype)
            .setLoc(this)
}

case class Construction(contractType: ContractType, args: Seq[Expression], isFFIInvocation: Boolean, override val obstype: Option[ObsidianType]) extends Expression {

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Construction =
        Construction(contractType.substitute(genericParams, actualParams),
            args.map(_.substitute(genericParams, actualParams)), isFFIInvocation, obstype)
            .setLoc(this)
}

case class Disown(e: Expression) extends UnaryExpression(Disown, e) // todo there's something happening that i don't understand

case class StateInitializer(stateName: Identifier, fieldName: Identifier, override val obstype: Option[ObsidianType]) extends Expression {

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

    override def toString: String = varName
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
// thisPermission is the permission of 'this' at the time of the transition. It is used for enforcing state locks caused by dynamic state checks.
case class Transition(newStateName: String, updates: Option[Seq[(ReferenceIdentifier, Expression)]], thisPermission: Permission) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Transition = {
        def doSubstitute: ((ReferenceIdentifier, Expression)) => (ReferenceIdentifier, Expression) = {
            case (id, expr) => (id, expr.substitute(genericParams, actualParams))
        }

        Transition(newStateName, updates.map(_.map(doSubstitute)), thisPermission).setLoc(this)
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

case class IfInState(e: Expression, ePerm: Permission, typeState: TypeState, s1: Seq[Statement], s2: Seq[Statement]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Statement = {
        val newTypeState = typeState match {
            case permVar: PermVar => ObsidianType.lookupState(genericParams, actualParams)(permVar)
            case ts => ts
        }

        IfInState(e.substitute(genericParams, actualParams),
            ePerm,
            newTypeState,
            s1.map(_.substitute(genericParams, actualParams)),
            s2.map(_.substitute(genericParams, actualParams))).setLoc(this)
    }
}

case class TryCatch(s1: Seq[Statement], s2: Seq[Statement]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): TryCatch =
        TryCatch(s1.map(_.substitute(genericParams, actualParams)), s2.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}

// TODO GENERIC: We could just compile switches to an if-else tree to simplify things.
//  However, that would require a default. Given we currently have no such thing, it's basically
//  impossible to write a polymorphic switch
case class Switch(e: Expression, cases: Seq[SwitchCase]) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Switch =
        Switch(e.substitute(genericParams, actualParams),
            cases.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
}

case class SwitchCase(stateName: String, body: Seq[Statement]) extends AST {
    def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): SwitchCase = {
        SwitchCase(stateName, body.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
    }
}

case class StaticAssert(expr: Expression, typeState: TypeState) extends Statement {
    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): StaticAssert = {
        val newState = typeState match {
            case permVar: PermVar => ObsidianType.lookupState(genericParams, actualParams)(permVar)
            case ts => ts
        }

        StaticAssert(expr.substitute(genericParams, actualParams), newState).setLoc(this)
    }
}

/* Declarations */
case class TypeDecl(name: String, typ: ObsidianType) extends Declaration {
    val tag: DeclarationTag = TypeDeclTag

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): TypeDecl =
        copy(typ = typ.substitute(genericParams, actualParams))
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

        if (idx >= 0 && idx < actualParams.length) {
            Field(isConst, actualParams(idx), name, availableIn).setLoc(this)
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

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Constructor = {
        Constructor(name, args.map(_.substitute(genericParams, actualParams)),
            resultType.substitute(genericParams, actualParams).asInstanceOf[NonPrimitiveType],
            body.map(_.substitute(genericParams, actualParams)))
            .setLoc(this)
    }
}

case class Transaction(name: String,
                       params: Seq[GenericType],
                       args: Seq[VariableDeclWithSpec],
                       retType: Option[ObsidianType],
                       ensures: Seq[Ensures],
                       body: Seq[Statement],
                       isStatic: Boolean,
                       isPrivate: Boolean,
                       thisType: NonPrimitiveType,
                       thisFinalType: NonPrimitiveType,
                       override val initialFieldTypes: Map[String, ObsidianType] = Map.empty,
                       override val finalFieldTypes: Map[String, ObsidianType] = Map.empty
                       // will populate initial and final field types after parsing
                      ) extends InvokableDeclaration with IsAvailableInStates {
    val tag: DeclarationTag = TransactionDeclTag

    def availableIn: Option[Set[String]] = thisType match {
        case StateType(_, stateNames, _) => Some(stateNames)
        case _ => None
    }

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): Transaction =
        Transaction(name,
            params,
            args.map(_.substitute(genericParams, actualParams)),
            retType.map(_.substitute(genericParams, actualParams)),
            ensures.map(_.substitute(genericParams, actualParams)),
            body.map(_.substitute(genericParams, actualParams)),
            isStatic,
            isPrivate,
            thisType.substitute(genericParams, actualParams).asInstanceOf[NonPrimitiveType],
            thisFinalType.substitute(genericParams, actualParams).asInstanceOf[NonPrimitiveType],
            initialFieldTypes.view.mapValues(_.substitute(genericParams, actualParams)).toMap,
            finalFieldTypes.view.mapValues(_.substitute(genericParams, actualParams)).toMap)
            .setLoc(this)

    def declarationStr: String =
        retType match {
            case Some(typ) =>
                s"transaction $name(${args.mkString(", ")}) returns $typ;"
            case None =>
                s"transaction $name(${args.mkString(", ")});"
        }
}

case class FSMEdge(fromState: Identifier, toState: Identifier) extends AST

case class Transitions(edges: Seq[FSMEdge]) extends AST


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

sealed trait ContractModifier extends HasLocation

case class IsAsset() extends ContractModifier

case class IsMain() extends ContractModifier

case class IsImport() extends ContractModifier

case class Import(name: String) extends AST

/* Layer */
sealed abstract class Contract(name: String, val sourcePath: String) extends Declaration {
    def params: Seq[GenericType]

    def declarations: Seq[Declaration]

    def modifiers: Set[ContractModifier] = Set.empty

    val isAsset: Boolean = modifiers.contains(IsAsset())
    val isMain: Boolean = modifiers.contains(IsMain())
    val isImport: Boolean = modifiers.contains(IsImport())

    def isInterface: Boolean

    def bound: ContractType
}

case class ObsidianContractImpl(override val modifiers: Set[ContractModifier],
                                name: String, params: Seq[GenericType], bound: ContractType,
                                override val declarations: Seq[Declaration],
                                transitions: Option[Transitions],
                                isInterface: Boolean,
                                sp: String) extends Contract(name, sp) {
    val tag: DeclarationTag = ContractDeclTag

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): ObsidianContractImpl =
        ObsidianContractImpl(modifiers, name, params, bound,
            declarations.map(_.substitute(genericParams, actualParams)),
            transitions,
            isInterface,
            sp).setLoc(this)
}

/* FFI contract for Java */
case class JavaFFIContractImpl(name: String,
                               interface: String,
                               javaPath: Seq[Identifier],
                               sp: String,
                               override val declarations: Seq[Declaration] = Seq.empty) extends Contract(name, sp) {
    val tag: DeclarationTag = ContractDeclTag

    override def substitute(genericParams: Seq[GenericType], actualParams: Seq[ObsidianType]): JavaFFIContractImpl = this

    override def bound: ContractType = ContractType(interface, Nil)

    override def isInterface: Boolean = false

    override def params: Seq[GenericType] = Nil
}


/* Program */
case class Program(imports: Seq[Import], contracts: Seq[Contract]) extends AST

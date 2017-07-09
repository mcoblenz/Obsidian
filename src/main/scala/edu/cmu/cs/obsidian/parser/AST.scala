package edu.cmu.cs.obsidian.parser

import scala.util.parsing.input.{NoPosition, Position}
import edu.cmu.cs.obsidian.lexer.Token

sealed abstract class AST() {
    var loc: Position = NoPosition
    def setLoc(t: Token): this.type = { loc = t.pos; this }
    def setLoc(ast: AST): this.type = { loc = ast.loc; this }
    def setLoc(id: (String, Position)): this.type = { loc = id._2; this }
}
sealed abstract class Statement() extends AST

/* All expressions are statements. We relegate the pruning of expressions
 * that don't have effects to a later analysis */
sealed abstract class Expression() extends Statement
sealed abstract class Declaration() extends AST {
    val name: String
}
sealed abstract class InvokableDeclaration() extends Declaration {
    val args: Seq[VariableDecl]
    val retType: Option[AstType]
    val body: Seq[Statement]
    val ensuresState: Option[Set[String]]
}
sealed abstract class AstType() extends AST

/* Expressions */
case class Variable(name: String) extends Expression
case class NumLiteral(value: Int) extends Expression
case class StringLiteral(value: String) extends Expression
case class TrueLiteral() extends Expression
case class FalseLiteral() extends Expression
case class This() extends Expression
case class Conjunction(e1: Expression, e2: Expression) extends Expression
case class Disjunction(e1: Expression, e2: Expression) extends Expression
case class LogicalNegation(e: Expression) extends Expression
case class Add(e1: Expression, e2: Expression) extends Expression
case class Subtract(e1: Expression, e2: Expression) extends Expression
case class Divide(e1: Expression, e2: Expression) extends Expression
case class Multiply(e1: Expression, e2: Expression) extends Expression
case class Equals(e1: Expression, e2: Expression) extends Expression
case class GreaterThan(e1: Expression, e2: Expression) extends Expression
case class GreaterThanOrEquals(e1: Expression, e2: Expression) extends Expression
case class LessThan(e1: Expression, e2: Expression) extends Expression
case class LessThanOrEquals(e1: Expression, e2: Expression) extends Expression
case class NotEquals(e1: Expression, e2: Expression) extends Expression
case class Dereference(e: Expression, f: String) extends Expression
case class LocalInvocation(name: String, args: Seq[Expression]) extends Expression
case class Invocation(recipient: Expression, name: String, args: Seq[Expression]) extends Expression
case class Construction(name: String, args: Seq[Expression]) extends Expression

/* statements and control flow constructs */
case class VariableDecl(typ: AstType, varName: String) extends Statement
case class VariableDeclWithInit(typ: AstType, varName: String, e: Expression) extends Statement
case class Return() extends Statement
case class ReturnExpr(e: Expression) extends Statement
case class Transition(newStateName: String, updates: Seq[(Variable, Expression)]) extends Statement
case class Assignment(assignTo: Expression, e: Expression) extends Statement
case class Throw() extends Statement
case class If(eCond: Expression, s: Seq[Statement]) extends Statement
case class IfThenElse(eCond: Expression, s1: Seq[Statement], s2: Seq[Statement]) extends Statement
case class TryCatch(s1: Seq[Statement], s2: Seq[Statement]) extends Statement
case class Switch(e: Expression, cases: Seq[SwitchCase]) extends Statement
case class SwitchCase(stateName: String, body: Seq[Statement]) extends AST

sealed abstract class TypeModifier() extends AST
case class IsReadOnly() extends TypeModifier
case class IsBorrowed() extends TypeModifier
case class IsRemote() extends TypeModifier
case class AstIntType() extends AstType
case class AstBoolType() extends AstType
case class AstStringType() extends AstType
case class AstContractType(modifiers: Seq[TypeModifier], name: String) extends AstType
case class AstStateType(modifiers: Seq[TypeModifier],
                        contractName: String,
                        stateName: String) extends AstType
case class AstPathContractType(modifiers: Seq[TypeModifier],
                               path: Seq[String],
                               name: String) extends AstType
case class AstPathStateType(modifiers: Seq[TypeModifier],
                            path: Seq[String],
                            contractName: String,
                            stateName: String) extends AstType

/* Declarations */
case class TypeDecl(name: String, typ: AstType) extends Declaration

case class Field(isConst: Boolean, typ: AstType, name: String) extends Declaration

case class Constructor(name: String,
                       args: Seq[VariableDecl],
                       ensuresState: Option[Set[String]],
                       body: Seq[Statement]) extends InvokableDeclaration {
    val retType: Option[AstType] = None
}
case class Func(name: String,
                args: Seq[VariableDecl],
                retType: Option[AstType],
                body: Seq[Statement]) extends InvokableDeclaration {
    val ensuresState: Option[Set[String]] = None
}
case class Transaction(name: String,
                       args: Seq[VariableDecl],
                       retType: Option[AstType],
                       ensures: Seq[Ensures],
                       ensuresState: Option[Set[String]],
                       body: Seq[Statement]) extends InvokableDeclaration
case class State(name: String, declarations: Seq[Declaration]) extends Declaration

case class Ensures(expr: Expression) extends AST

sealed abstract class ContractModifier() extends AST
case class IsOwned() extends ContractModifier
case class IsShared() extends ContractModifier
case class IsMain() extends ContractModifier

case class Import(name: String) extends AST

case class Contract(mod: Option[ContractModifier],
                    name: String,
                    declarations: Seq[Declaration]) extends Declaration

/* Program */
case class Program(imports: Seq[Import], contracts: Seq[Contract]) extends AST
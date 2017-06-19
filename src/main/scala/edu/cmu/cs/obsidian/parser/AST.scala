package edu.cmu.cs.obsidian.parser

sealed trait AST
sealed trait Statement extends AST

/* All expressions are statements. We relegate the pruning of expressions
 * that don't have effects to a later analysis */
sealed trait Expression extends Statement
sealed trait Declaration extends AST
sealed trait AstType extends AST

/* Expressions */
case class Variable(x: String) extends Expression
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
case object Return extends Statement
case class ReturnExpr(e: Expression) extends Statement
case class Transition(newStateName: String, updates: Seq[(Variable, Expression)]) extends Statement
case class Assignment(assignTo: Expression, e: Expression) extends Statement
case class Throw() extends Statement
case class If(eCond: Expression, s: Seq[Statement]) extends Statement
case class IfThenElse(eCond: Expression, s1: Seq[Statement], s2: Seq[Statement]) extends Statement
case class TryCatch(s1: Seq[Statement], s2: Seq[Statement]) extends Statement
case class Switch(e: Expression, cases: Seq[SwitchCase]) extends Statement
case class SwitchCase(stateName: String, body: Seq[Statement]) extends AST

sealed trait TypeModifier
case object IsReadOnly extends TypeModifier
case object IsBorrowed extends TypeModifier
case object IsRemote extends TypeModifier

case class AstIntType() extends AstType
case class AstBoolType() extends AstType
case class AstStringType() extends AstType
case class AstContractType(modifiers: Seq[TypeModifier], name: String) extends AstType
case class AstStateType(modifiers: Seq[TypeModifier],
                        contractName: String,
                        stateName: String) extends AstType

/* Declarations */
case class TypeDecl(name: String, typ: AstType) extends Declaration

case class Field(typ: AstType, fieldName: String) extends Declaration

case class Constructor(name: String,
                       args: Seq[VariableDecl],
                       body: Seq[Statement]) extends Declaration
case class Func(name: String,
                args: Seq[VariableDecl],
                retType: Option[AstType],
                body: Seq[Statement]) extends Declaration
case class Transaction(name: String,
                       args: Seq[VariableDecl],
                       retType: Option[AstType],
                       ensures: Seq[Ensures],
                       body: Seq[Statement]) extends Declaration
case class State(name: String, declarations: Seq[Declaration]) extends Declaration

case class Ensures(expr: Expression)

sealed trait ContractModifier
case object IsOwned extends ContractModifier
case object IsShared extends ContractModifier
case object IsMain extends ContractModifier

case class Import(name: String)

case class Contract(mod: Option[ContractModifier], name: String, declarations: Seq[Declaration]) extends Declaration

/* Program */
case class Program(imports: Seq[Import], contracts: Seq[Contract]) extends AST
package edu.cmu.cs.obsidian.codegen

// reminder: use abstract class if want to create a base class that requires constructor arguments
sealed trait YulAST

object LiteralKind extends Enumeration {
    type LiteralKind = Value
    val number, boolean, string = Value
}
trait Expression extends YulAST
trait YulStatement extends YulAST


// for each asm struct, create a case class
case class TypedName (name: String, ntype: String) extends YulAST
case class Case (value: Literal, body: Block) extends YulAST

case class Literal (kind: LiteralKind.LiteralKind, value: String, vtype: String) extends Expression
case class Identifier (name: String) extends Expression
case class FunctionCall (functionName: Identifier, arguments: Seq[Expression]) extends Expression

case class Assignment (variableNames: Seq[Identifier], value: Expression) extends YulStatement
case class VariableDeclaration (variables: Seq[TypedName]) extends YulStatement
case class FunctionDefinition (
                                  name: String,
                                  parameters: Seq[TypedName],
                                  returnVariables: Seq[TypedName],
                                  body: Block) extends YulStatement
case class If (condition: Expression, body: Block) extends YulStatement
case class Switch (expression: Expression, cases: Seq[Case]) extends YulStatement
case class ForLoop (pre: Block, condition: Expression, post: Block, body: Block) extends YulStatement
case class Break () extends YulStatement
case class Continue () extends YulStatement
case class Leave () extends YulStatement
case class ExpressionStatement (expression: Expression) extends YulStatement
case class Block (statements: Seq[YulStatement]) extends YulStatement


/*
    Object = 'object' StringLiteral '{' Code ( Object | Data )* '}'
    Code = 'code' Block
    Data = 'data' StringLiteral ( HexLiteral | StringLiteral )
    HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')
    StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'
 */
case class YulObject (name: String, code: Code, subObjects: Seq[YulObject], data: Seq[Data]) extends YulAST
case class Code (block: Block) extends YulAST
case class Data (name: String, hex: Seq[HexLiteral], str: Seq[StringLiteral]) extends YulAST
case class HexLiteral (content: String) extends YulAST
case class StringLiteral (content: String) extends YulAST


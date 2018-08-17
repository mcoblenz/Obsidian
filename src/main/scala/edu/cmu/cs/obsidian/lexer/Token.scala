package edu.cmu.cs.obsidian.lexer

import scala.util.parsing.input.Positional

sealed trait Token extends Positional

/* Although most of the below tokens don't appear to have any data, they must
 * be case classes instead of case objects because Token extends Positional */

/* keywords */
case class ContractT() extends Token { override def toString: String = "contract" }
case class InterfaceT() extends Token { override def toString: String = "interface" }
case class IfT() extends Token { override def toString: String = "if" }
case class ElseT() extends Token { override def toString: String = "else" }
case class TransactionT() extends Token { override def toString: String = "transaction" }
case class StaticT() extends Token { override def toString: String = "static" }
case class TypeT() extends Token { override def toString: String = "type" }
case class StateT() extends Token { override def toString: String = "state" }
case class TryT() extends Token { override def toString: String = "try" }
case class CatchT() extends Token { override def toString: String = "catch" }
case class ThrowT() extends Token { override def toString: String = "throw" }
case class NotT() extends Token { override def toString: String = "not" }
case class AndT() extends Token { override def toString: String = "and" }
case class OrT() extends Token { override def toString: String = "or" }
case class ReturnT() extends Token { override def toString: String = "return" }
case class ReturnsT() extends Token { override def toString: String = "returns" }
case class NewT() extends Token { override def toString: String = "new" }
case class SwitchT() extends Token { override def toString: String = "switch"}
case class CaseT() extends Token { override def toString: String = "case"}
case class OwnedT() extends Token { override def toString: String = "owned"}
case class MainT() extends Token { override def toString: String = "main"}
case class IntT() extends Token { override def toString: String = "int"}
case class StringT() extends Token { override def toString: String = "string"}
case class BoolT() extends Token { override def toString: String = "bool"}
case class TrueT() extends Token { override def toString: String = "true"}
case class FalseT() extends Token { override def toString: String = "false"}
case class ThisT() extends Token { override def toString: String = "this"}
case class ParentT() extends Token { override def toString: String = "parent"}
case class ImportT() extends Token { override def toString: String = "import"}
case class RemoteT() extends Token { override def toString: String = "remote"}
case class EnsuresT() extends Token { override def toString: String = "ensures"}
case class RequiresT() extends Token { override def toString: String = "requires"}
case class ConstT() extends Token { override def toString: String = "const" }
case class AvailableT() extends Token { override def toString: String = "available" }
case class EndsT() extends Token { override def toString: String = "ends" }
case class InT() extends Token { override def toString: String = "in" }
case class ResourceT() extends Token { override def toString: String = "resource" }
case class DisownT() extends Token { override def toString: String = "disown" }
case class PrivateT() extends Token { override def toString: String = "private" }
case class TransitionsT() extends Token { override def toString: String = "transitions" }

case class IdentifierT(x: String) extends Token { override def toString: String = s"$x" }
case class NumLiteralT(x: Int) extends Token { override def toString: String = s"$x" }
case class StringLiteralT(x: String) extends Token { override def toString: String =  s""""$x"""" }

/* punctuation */
case class LBraceT() extends Token { override def toString: String = "{" }
case class RBraceT() extends Token { override def toString: String = "}" }
case class LParenT() extends Token { override def toString: String = "(" }
case class RParenT() extends Token { override def toString: String = ")" }
case class DotT() extends Token { override def toString: String = "." }
case class CommaT() extends Token { override def toString: String = "," }
case class SemicolonT() extends Token { override def toString: String = ";" }
case class EqT() extends Token { override def toString: String = "=" }
case class EqEqT() extends Token { override def toString: String = "==" }
case class NotEqT() extends Token { override def toString: String = "!=" }
case class LtT() extends Token { override def toString: String = "<" }
case class GtT() extends Token { override def toString: String = ">" }
case class GtEqT() extends Token { override def toString: String = ">=" }
case class LtEqT() extends Token { override def toString: String = "<=" }
case class RightArrowT() extends Token { override def toString: String = "->" }
case class BigRightArrowT() extends Token { override def toString: String = "=>" }
case class ColonColonT() extends Token { override def toString: String = "::" }
case class PlusT() extends Token { override def toString: String = "+" }
case class StarT() extends Token { override def toString: String = "*" }
case class ForwardSlashT() extends Token { override def toString: String = "/" }
case class MinusT() extends Token { override def toString: String = "-" }
case class AtT() extends Token { override def toString: String = "@" }
case class LBracketT() extends Token { override def toString: String ="[" }
case class RBracketT() extends Token { override def toString: String ="]" }
case class PipeT() extends Token { override def toString: String ="|" }
case class ChevT() extends Token { override def toString: String =">>" }


    /* comment token: the parser never sees this; comments should be pruned in the lexer */
case class CommentT() extends Token
sealed trait AST

/* Expressions */
case class Variable(x : String) extends AST 
case class Field(x : String, f : String) extends AST 
case class NumLiteral(value : Int) extends AST 
case class Conjunction(e1 : AST, e2 : AST) extends AST 
case class Disjunction(e1 : AST, e2 : AST) extends AST 
case class LogicalNegation(e : AST) extends AST 
case class Add(e1 : AST, e2 : AST) extends AST 
case class Subtract(e1 : AST, e2 : AST) extends AST 
case class Divide(e1 : AST, e2 : AST) extends AST 
case class Multiply(e1 : AST, e2 : AST) extends AST 
case class Equals(e1 : AST, e2 : AST) extends AST 
case class GreaterThan(e1 : AST, e2 : AST) extends AST 
case class GreaterThanOrEquals(e1 : AST, e2 : AST) extends AST 
case class LessThan(e1 : AST, e2 : AST) extends AST 
case class LessThanOrEquals(e1 : AST, e2 : AST) extends AST 
case class NotEquals(e1 : AST, e2 : AST) extends AST 
case class LocalInvocation(name : String, args : Seq[AST]) extends AST 
case class Invocation(recipientName : String, name : String, args : Seq[AST]) extends AST 
case class Construction(args : Seq[AST]) extends AST 

/* Statements */
case class Return(e : AST) extends AST 
case class Transition(newStateName : String) extends AST 
case class Assignment(assignTo : AST, e : AST) extends AST
case class Throw() extends AST 
case class If(eCond : AST, s : AST) extends AST 
case class IfThenElse(eCond : AST, s1 : AST, s2 : AST) extends AST 
case class TryCatch(s1 : AST, s2 : AST) extends AST 
case class Sequence(s1 : AST, s2 : AST) extends AST 

case class Type(isLinear : Boolean, name : String) extends AST

/* Declarations */
case class TypeDeclaration(name : String, typ : Type) extends AST
case class VarDeclaration(typ : Type, varName : String) extends AST
case class FuncDeclaration(name : String,
                           args : Seq[(String, VarDeclaration)],
                           body : AST) extends AST
case class TransactionDeclaration(name : String,
                                  args : Seq[(String, VarDeclaration)],
                                  body : AST) extends AST
case class StateDeclaration(name : String, declarations : Seq[AST]) extends AST
case class Contract(name : String, declarations : Seq[AST]) extends AST
package edu.cmu.cs.obsidian.codegen

/* combinators to for producing strings to reduce repeated code, used in yulAST and CodeGenYul */
object Util {
  def brace(s: String): String = "{" + s + "}"
  def paren(s: String): String = "(" + s + ")"
  def ilit(i: Int): Literal = Literal(LiteralKind.number,i.toString,"int")
  def blit(b : Boolean): Literal = Literal(LiteralKind.boolean,b.toString,"bool")
  val true_lit: Literal = blit(true)
  val false_lit: Literal = blit(false)
}

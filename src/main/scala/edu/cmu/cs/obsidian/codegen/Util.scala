package edu.cmu.cs.obsidian.codegen

/* utility functions shared between yulAST and CodeGenYul */
object Util {
  def brace(str: String): String = s"{$str}"
  def paren(str: String): String = s"($str)"
  def ilit(i: Int): Literal = Literal(LiteralKind.number,i.toString,"int")
  def blit(b : Boolean): Literal = Literal(LiteralKind.boolean,b.toString,"bool")
  val true_lit: Literal = blit(true)
  val false_lit: Literal = blit(false)

  // TODO unimplemented; hardcode to uint256 for now
  def mapObsTypeToABI(ntype: String): String = {
    "uint256"
  }

  // TODO unimplemented; hardcode for now; bouncycastle library may be helpful
  def keccak256(s: String): String = {
    "0x70a08231"
  }

  def hashFunction(f: FunctionDefinition): String = {
    // TODO/iev until the above todo gets resolved, the outer call makes this function
    // basically \_ => "0x70a08231". the original implementation didn't have a
    // separator so i don't know what it should be but probably not " "
    keccak256(f.name + paren(f.parameters.map(p=>mapObsTypeToABI(p.ntype)).mkString(" ")))
    // TODO truncate and keep the first 4 bytes
  }
}

package edu.cmu.cs.obsidian.codegen
import org.bouncycastle.jcajce.provider.digest.Keccak
import org.bouncycastle.util.encoders.Hex

/* utility functions shared between yulAST and CodeGenYul */
object Util {
  def brace(str: String): String = s"{$str}"
  def paren(str: String): String = s"($str)"
  def ilit(i: Int): Literal = Literal(LiteralKind.number,i.toString,"int")
  def blit(b : Boolean): Literal = Literal(LiteralKind.boolean,b.toString,"bool")
  val true_lit: Literal = blit(true)
  val false_lit: Literal = blit(false)

  def mapObsTypeToABI(ntype: String): String = {
    // todo: this covers the primitive types from ObsidianType.scala but is hard to maintain because
    // it's basically hard coded, and doesn't traverse the structure of more complicated types.
    ntype match {
      case "bool" => "boolean"
      case "int" => "int256"
      case "string" => "string"
      case "Int256" => "int256"
      case "unit" => assert(assertion = false, "unimplemented: unit type not encoded in Yul");""
      case _ => assert(assertion = false, "yul codegen encountered an obsidian type without a mapping to the ABI"); ""
    }
  }

  def keccak256(s: String): String = {
    val digestK: Keccak.Digest256 = new Keccak.Digest256()
    Hex.toHexString(digestK.digest(s.getBytes).slice(0,4)) //todo: i'm not sure if it should be the first or last 4.
  }

  def hashOfFunctionDef(f: FunctionDefinition): String = {
    keccak256(f.name + paren(f.parameters.map(p=>mapObsTypeToABI(p.ntype)).mkString(" ")))
  }
}

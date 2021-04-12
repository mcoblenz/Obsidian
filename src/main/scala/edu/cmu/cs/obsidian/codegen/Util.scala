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

  // TODO unimplemented; hardcode to uint256 for now
  def mapObsTypeToABI(ntype: String): String = {
    "uint256"
  }

  def keccak256(s: String): String = {
    val digestK: Keccak.Digest256 = new Keccak.Digest256()
    val digest: Array[Byte] = digestK.digest(s.getBytes)
    Hex.toHexString(digest.slice(0,4)) //todo: i'm not sure if it should be the first or last 4.
  }

  def hashFunction(f: FunctionDefinition): String = {
    keccak256(f.name + paren(f.parameters.map(p=>mapObsTypeToABI(p.ntype)).mkString(" ")))
  }
}

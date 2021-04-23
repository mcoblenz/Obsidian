package edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.codegen
import org.bouncycastle.jcajce.provider.digest.Keccak
import org.bouncycastle.util.encoders.Hex

/* utility functions shared between yulAST and CodeGenYul */
object Util {
  def brace(str: String): String = s"{$str}"
  def paren(str: String): String = s"($str)"
  def quote(str: String): String = "\"" + str + "\"" // escape characters are known to not work with string interpolation
  def ilit(i: Int): Literal = Literal(LiteralKind.number, i.toString, "int")
  def blit(b : Boolean): Literal = Literal(LiteralKind.boolean, b.toString, "bool")
  def hexlit(s: String): Literal = Literal(LiteralKind.number, s, "int")
  def stringlit(s: String): Literal = Literal(LiteralKind.string, s, "string")
  def callvaluecheck: YulStatement = codegen.If(FunctionCall(Identifier("callvalue"), Seq()), Block(Seq(ExpressionStatement(FunctionCall(Identifier("revert"), Seq(ilit(0), ilit(0)))))))
  def unary_ap(n: String, e: Expression): Expression = FunctionCall(Identifier(n),Seq(e))
  def binary_ap(n : String, e1: Expression, e2: Expression): Expression = FunctionCall(Identifier(n), Seq(e1, e2))
  def triple(n : String, e1: Expression, e2: Expression, e3: Expression): Expression = FunctionCall(Identifier(n), Seq(e1, e2, e3))
  def store_then_ret(retvar:Identifier, e:Expression) = Seq(edu.cmu.cs.obsidian.codegen.Assignment(Seq(retvar),e)) //todo this may not be needed once i work out more cases

  val true_lit: Literal = blit(true)
  val false_lit: Literal = blit(false)

  def mapObsTypeToABI(ntype: String): String = {
    // todo: this covers the primitive types from ObsidianType.scala but is hard to maintain because
    // it's basically hard coded, and doesn't traverse the structure of more complicated types.
    ntype match {
      case "bool" => "boolean"
      case "int" => "u256"
      case "string" => "string"
      case "Int256" => "int256"
      case "unit" => assert(assertion = false, "unimplemented: unit type not encoded in Yul"); ""
      case _ => assert(assertion = false, "yul codegen encountered an obsidian type without a mapping to the ABI"); ""
    }
  }

  def functionRename(name: String): String = {
    name //todo some sort of alpha variation here combined with consulting a mapping
  }

  def keccak256(s: String): String = {
    val digestK: Keccak.Digest256 = new Keccak.Digest256()
    s"0x${Hex.toHexString(digestK.digest(s.getBytes).slice(0, 4))}" //todo: i'm not sure if it should be the first or last 4.
  }

  def hashOfFunctionDef(f: FunctionDefinition): String = {
    keccak256(f.name + paren(f.parameters.map(p=>mapObsTypeToABI(p.ntype)).mkString(" ")))
  }
}

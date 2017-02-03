sealed trait Token

case object ContractT extends Token
case object IfT extends Token
case object ElseT extends Token
case object TransactionT extends Token
case object FunctionT extends Token
case object TypeT extends Token
case object StateT extends Token
case object TryT extends Token
case object CatchT extends Token
case object ThrowT extends Token
case object NotT extends Token
case object AndT extends Token
case object OrT extends Token

case class IdentifierT(x : String) extends Token
case class NumLiteralT(x : Int) extends Token

case object LBraceT extends Token
case object RBraceT extends Token
case object LParenT extends Token
case object RParenT extends Token
case object DotT extends Token
case object CommaT extends Token
case object SemicolonT extends Token
case object EqT extends Token
case object EqEqT extends Token
case object NotEqT extends Token
case object LtT extends Token
case object GtT extends Token
case object GtEqT extends Token
case object LtEqT extends Token
case object RightArrowT extends Token
case object BigRightArrowT extends Token
case object PlusT extends Token
case object StarT extends Token
case object ForwardSlashT extends Token
case object MinusT extends Token

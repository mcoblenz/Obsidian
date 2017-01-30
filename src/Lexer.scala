import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
    def decimalP = """(0|[1-9]\d*)""".r ^^ (x => NumLiteralT(x.toInt))
    def hexP = """(0x\d*)""".r ^^ (x => NumLiteralT(Integer.parseInt(x, 16)))
    def identifierP = """[_a-zA-Z][_a-zA-Z0-9]*""".r ^^ (x => IdentifierT(x))

    /* Keywords */
    def ifP = """if""".r ^^ (_ => IfT)
    def elseP = """else""".r ^^ (_ => ElseT)
    def contractP = """contract""".r ^^ (_ => ContractT)
    def transactionP = """transaction""".r ^^ (_ => TransactionT)
    def functionP = """function""".r ^^ (_ => FunctionT)
    def typeP = """type""".r ^^ (_ => TypeT)
    def stateP = """state""".r ^^ (_ => StateT)
    def tryP = """try""".r ^^ (_ => TryT)
    def catchP = """catch""".r ^^ (_ => CatchT)
    def throwP = """throw""".r ^^ (_ => ThrowT)
    def notP = """not""".r ^^ (_ => NotT)
    def andP = """and""".r ^^ (_ => AndT)
    def orP = """or""".r ^^ (_ => OrT)

    /* Punctuation */
    def lBraceP = """\{""".r ^^ (_ => LBraceT)
    def rBraceP = """\}""".r ^^ (_ => RBraceT)
    def lParenP = """\(""".r ^^ (_ => LParenT)
    def rParenP = """\)""".r ^^ (_ => RParenT)
    def dotP = """\.""".r ^^ (_ => DotT)
    def commaP = """,""".r ^^ (_ => CommaT)
    def semicolonP = """;""".r ^^ (_ => SemicolonT)
    def eqP = """=""".r ^^ (_ => EqT)
    def ltP = """<""".r ^^ (_ => LtT)
    def gtP = """>""".r ^^ (_ => GtT)
    def gtEqP = """>=""".r ^^ (_ => GtEqT)
    def ltEqP = """<=""".r ^^ (_ => LtEqT)
    def rightArrowP = """->""".r ^^ (_ => RightArrowT)
    def bigRightArrowP = """=>""".r ^^ (_ => BigRightArrowT)


    def tokenize : Parser[Seq[Token]] =
        phrase(rep1(
            ifP | elseP | contractP | transactionP | functionP | typeP | stateP | tryP | catchP | throwP | notP |
            andP | orP |

            decimalP | hexP | identifierP |

            lBraceP | rBraceP | lParenP | rParenP | commaP | dotP | semicolonP |

            /* order is important here because some tokens contain the others */
            gtEqP | ltEqP | rightArrowP | bigRightArrowP | ltP | gtP | eqP
        ))
}

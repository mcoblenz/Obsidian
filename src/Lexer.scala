import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
    private def decimalP = """(0|[1-9]\d*)""".r ^^ (x => NumLiteralT(x.toInt))
    private def hexP = """(0x\d*)""".r ^^ (x => NumLiteralT(Integer.parseInt(x, 16)))
    private def identifierP = """[_a-zA-Z][_a-zA-Z0-9]*""".r ^^ (x => IdentifierT(x))

    /* Keywords */
    private def ifP = """if""".r ^^ (_ => IfT)
    private def elseP = """else""".r ^^ (_ => ElseT)
    private def contractP = """contract""".r ^^ (_ => ContractT)
    private def transactionP = """transaction""".r ^^ (_ => TransactionT)
    private def functionP = """function""".r ^^ (_ => FunctionT)
    private def typeP = """type""".r ^^ (_ => TypeT)
    private def stateP = """state""".r ^^ (_ => StateT)
    private def tryP = """try""".r ^^ (_ => TryT)
    private def catchP = """catch""".r ^^ (_ => CatchT)
    private def throwP = """throw""".r ^^ (_ => ThrowT)
    private def notP = """not""".r ^^ (_ => NotT)
    private def andP = """and""".r ^^ (_ => AndT)
    private def orP = """or""".r ^^ (_ => OrT)

    /* Punctuation */
    private def plusP = """\+""".r ^^ (_ => PlusT)
    private def starP = """\*""".r ^^ (_ => StarT)
    private def forwardSlashP = """/""".r ^^ (_ => ForwardSlashT)
    private def minusP = """-""".r ^^ (_ => MinusT)
    private def lBraceP = """\{""".r ^^ (_ => LBraceT)
    private def rBraceP = """\}""".r ^^ (_ => RBraceT)
    private def lParenP = """\(""".r ^^ (_ => LParenT)
    private def rParenP = """\)""".r ^^ (_ => RParenT)
    private def dotP = """\.""".r ^^ (_ => DotT)
    private def commaP = """,""".r ^^ (_ => CommaT)
    private def semicolonP = """;""".r ^^ (_ => SemicolonT)
    private def eqP = """=""".r ^^ (_ => EqT)
    private def notEqP = """!=""".r ^^ (_ => NotEqT)
    private def ltP = """<""".r ^^ (_ => LtT)
    private def gtP = """>""".r ^^ (_ => GtT)
    private def gtEqP = """>=""".r ^^ (_ => GtEqT)
    private def eqEqP = """==""".r ^^ (_ => EqEqT)
    private def ltEqP = """<=""".r ^^ (_ => LtEqT)
    private def rightArrowP = """->""".r ^^ (_ => RightArrowT)
    private def bigRightArrowP = """=>""".r ^^ (_ => BigRightArrowT)


    private def tokenParser : Parser[Seq[Token]] =
        phrase(rep1(
            ifP | elseP | contractP | transactionP | functionP | typeP | stateP | tryP | catchP | throwP | notP |
            andP | orP |

            decimalP | hexP | identifierP |

            lBraceP | rBraceP | lParenP | rParenP | commaP | dotP | semicolonP |

            plusP | starP | forwardSlashP | minusP |

            /* order is important here because some tokens contain the others */
            gtEqP | ltEqP | eqEqP | notEqP | rightArrowP | bigRightArrowP | ltP | gtP | eqP
        ))

    def tokenize(src : String) : Either[String, Seq[Token]] = {
        parse(tokenParser, src) match {
            case Success(matched , _) => Right(matched)
            case Failure(msg ,_) => Left("FAILURE: " + msg)
            case Error(msg , _) => Left("ERROR: " + msg)
        }
    }
}

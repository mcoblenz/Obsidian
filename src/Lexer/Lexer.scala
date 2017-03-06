package edu.cmu.cs.obsidian.lexer
import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
    private def decimalP = ("""(0|([1-9]\d*))""".r) ^^ {
        case x => NumLiteralT(x.toInt)
    }

    private def hexP = ("""(0x\d*)""".r) ^^ {
        case x => NumLiteralT(Integer.parseInt(x, 16))
    }

    private def stringLitP = ("\".*\"".r) ^^ {
        case s => StringLiteralT(s)
    }

    /* we could give everything here a separate parser, but we want to enforce
     * (as most languages seem to) that keywords/literals/identifiers are
     * separated by white space */
    private def atomP = """[_a-zA-Z][_a-zA-Z0-9]*""".r ^^ {
        case "if" => IfT()
        case "else" => ElseT()
        case "contract" => ContractT()
        case "transaction" => TransactionT()
        case "function" => FunctionT()
        case "type" => TypeT()
        case "state" => StateT()
        case "try" => TryT()
        case "catch" => CatchT()
        case "throw" => ThrowT()
        case "not" => NotT()
        case "and" => AndT()
        case "or" => OrT()
        case "return" => ReturnT()
        case "new" => NewT()
        case "linear" => LinearT()
        case "switch" => SwitchT()
        case "case" => CaseT()
        case "unique" => UniqueT()
        case "shared" => SharedT()
        case "main" => MainT()
        case "int" => IntT()
        case "bool" => BoolT()
        case "string" => StringT()
        case "true" => TrueT()
        case "false" => FalseT()
        case "this" => ThisT()
        case id => IdentifierT(id)
    }

    /* Punctuation */
    private def plusP = """\+""".r ^^ (_ => PlusT())
    private def starP = """\*""".r ^^ (_ => StarT())
    private def forwardSlashP = """/""".r ^^ (_ => ForwardSlashT())
    private def minusP = """-""".r ^^ (_ => MinusT())
    private def lBraceP = """\{""".r ^^ (_ => LBraceT())
    private def rBraceP = """\}""".r ^^ (_ => RBraceT())
    private def lParenP = """\(""".r ^^ (_ => LParenT())
    private def rParenP = """\)""".r ^^ (_ => RParenT())
    private def dotP = """\.""".r ^^ (_ => DotT())
    private def commaP = """,""".r ^^ (_ => CommaT())
    private def semicolonP = """;""".r ^^ (_ => SemicolonT())
    private def eqP = """=""".r ^^ (_ => EqT())
    private def notEqP = """!=""".r ^^ (_ => NotEqT())
    private def ltP = """<""".r ^^ (_ => LtT())
    private def gtP = """>""".r ^^ (_ => GtT())
    private def gtEqP = """>=""".r ^^ (_ => GtEqT())
    private def eqEqP = """==""".r ^^ (_ => EqEqT())
    private def ltEqP = """<=""".r ^^ (_ => LtEqT())
    private def rightArrowP = """->""".r ^^ (_ => RightArrowT())
    private def bigRightArrowP = """=>""".r ^^ (_ => BigRightArrowT())

    private def oneToken: Parser[Token] = (decimalP | hexP | stringLitP | atomP |

    lBraceP | rBraceP | lParenP | rParenP | commaP | dotP | semicolonP |

    /* order is important here because some tokens contain the others */
    gtEqP | ltEqP | eqEqP | notEqP | rightArrowP | bigRightArrowP | ltP | gtP | eqP |

    plusP | starP | forwardSlashP | minusP)

    private def tokenParser: Parser[Seq[Token]] = phrase(rep1(positioned(oneToken)))

    def tokenize(src: String): Either[String, Seq[Token]] = {
        parse(tokenParser, src) match {
            case Success(matched , _) => Right(matched)
            case Failure(msg ,_) => Left("FAILURE: " + msg)
            case Error(msg , _) => Left("ERROR: " + msg)
        }
    }
}

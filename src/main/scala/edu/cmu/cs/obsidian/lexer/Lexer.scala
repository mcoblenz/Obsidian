package edu.cmu.cs.obsidian.lexer
import scala.util.parsing.combinator._

object Lexer extends RegexParsers {
    private def decimalP = ("""(0|([1-9]\d*))""".r) ^^ {
        case x => NumLiteralT(x.toInt)
    }

    private def hexP = ("""(0x\d*)""".r) ^^ {
        case x => NumLiteralT(Integer.parseInt(x, 16))
    }

    private def stringLitP = """"[^"]*"""".r ^^ {
        case s =>
            val content = s.substring(1, s.length - 1)
            StringLiteralT(content)
    }

    /* we could give everything here a separate parser, but we want to enforce
     * (as most languages seem to) that keywords/literals/identifiers are
     * separated by white space */
    private def atomP = """[_a-zA-Z][_a-zA-Z0-9]*""".r ^^ {
        case "if" => IfT()
        case "else" => ElseT()
        case "contract" => ContractT()
        case "interface" => InterfaceT()
        case "transaction" => TransactionT()
        case "method" => MethodT()
        case "static" => StaticT()
        case "type" => TypeT()
        case "state" => StateT()
        case "try" => TryT()
        case "catch" => CatchT()
        case "revert" => RevertT()
        case "return" => ReturnT()
        case "returns" => ReturnsT()
        case "new" => NewT()
        case "switch" => SwitchT()
        case "case" => CaseT()
        case "owned" => OwnedT()
        case "main" => MainT()
        case "int" => IntT()
        case "int256" => Int256T()
        case "bool" => BoolT()
        case "string" => StringT()
        case "true" => TrueT()
        case "false" => FalseT()
        case "this" => ThisT()
        case "import" => ImportT()
        case "remote" => RemoteT()
        case "ensures" => EnsuresT()
        case "requires" => RequiresT()
        case "const" => ConstT()
        case "available" => AvailableT()
        case "in" => InT()
        case "asset" => AssetT()
        case "disown" => DisownT()
        case "private" => PrivateT()
        case "transitions" => TransitionsT()
        case "with" => WithT()
        case "implements" => ImplementsT()
        case "where" => WhereT()
        case "is" => IsT()
        case id => IdentifierT(id)
    }

    /* comment parser: must be parsed before [forwardSlashP] */
    private def longCommentP = {
        "/*" ~ rep(not("*/") ~ "(?s).".r) ~ "*/" ^^^ CommentT()
    }


    /* comment parser: must be parsed before [forwardSlashP] */
    private def commentP = {
        """//""".r ~! """.*""".r ^^^ CommentT()
    }

    /* Punctuation */
    private def notP = """!""".r ^^^ NotT()
    private def andP = """&&""".r ^^^ AndT()
    private def orP = """\|\|""".r ^^^ OrT()
    private def plusP = """\+""".r ^^^  PlusT()
    private def starP = """\*""".r ^^^  StarT()
    private def forwardSlashP = """/""".r ^^^  ForwardSlashT()
    private def percentP = """%""".r ^^^  PercentT()
    private def minusP = """-""".r ^^^  MinusT()
    private def lBraceP = """\{""".r ^^^  LBraceT()
    private def rBraceP = """\}""".r ^^^  RBraceT()
    private def lParenP = """\(""".r ^^^  LParenT()
    private def rParenP = """\)""".r ^^^  RParenT()
    private def dotP = """\.""".r ^^^  DotT()
    private def commaP = """,""".r ^^^  CommaT()
    private def semicolonP = """;""".r ^^^  SemicolonT()
    private def eqP = """=""".r ^^^  EqT()
    private def notEqP = """!=""".r ^^^  NotEqT()
    private def ltP = """<""".r ^^^  LtT()
    private def gtP = """>""".r ^^^  GtT()
    private def gtEqP = """>=""".r ^^^  GtEqT()
    private def eqEqP = """==""".r ^^^  EqEqT()
    private def ltEqP = """<=""".r ^^^  LtEqT()
    private def rightArrowP = """->""".r ^^^  RightArrowT()
    private def bigRightArrowP = """=>""".r ^^^  BigRightArrowT()
    private def coloncolonP = """::""".r ^^^  ColonColonT()
    private def atP = """@""".r ^^^  AtT()
    private def lBracketP = """\[""".r ^^^  LBracketT()
    private def rBracketP = """\]""".r ^^^  RBracketT()
    private def pipeP = """\|""".r ^^^  PipeT()
    private def chevP = """>>""".r ^^^  ChevT()
    private def colonP  = """:""".r ^^^ ColonT()


    private def oneToken: Parser[Token] =

    (
        longCommentP | commentP | decimalP | hexP | stringLitP | atomP |

        lBraceP | rBraceP | lParenP | rParenP | commaP | dotP | semicolonP |

        /* order is important here because some tokens contain the others */
        orP | andP | chevP | gtEqP | ltEqP | eqEqP | notEqP | notP | rightArrowP | bigRightArrowP | ltP | gtP | eqP |

        plusP | starP | forwardSlashP | percentP | minusP | coloncolonP | atP | lBracketP | rBracketP | pipeP | colonP
    )

    private def tokenParser: Parser[Seq[Token]] = phrase(rep1(positioned(oneToken)))

    private def removeComments(tokens: Seq[Token]): Seq[Token] = {
        tokens.filter({ _ match { case CommentT() => false ; case _ => true } })
    }

    def tokenize(src: String): Either[String, Seq[Token]] = {
        parse(tokenParser, src) match {
            case Success(matched , _) => Right(removeComments(matched))
            case Failure(msg ,_) => Left("FAILURE: " + msg)
            case Error(msg , _) => Left("ERROR: " + msg)
        }
    }
}

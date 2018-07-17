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
        case "static" => StaticT()
        case "type" => TypeT()
        case "state" => StateT()
        case "try" => TryT()
        case "catch" => CatchT()
        case "throw" => ThrowT()
        case "not" => NotT()
        case "and" => AndT()
        case "or" => OrT()
        case "return" => ReturnT()
        case "returns" => ReturnsT()
        case "new" => NewT()
        case "switch" => SwitchT()
        case "case" => CaseT()
        case "owned" => OwnedT()
        case "main" => MainT()
        case "int" => IntT()
        case "bool" => BoolT()
        case "string" => StringT()
        case "true" => TrueT()
        case "false" => FalseT()
        case "this" => ThisT()
        case "parent" => ParentT()
        case "import" => ImportT()
        case "remote" => RemoteT()
        case "ensures" => EnsuresT()
        case "requires" => RequiresT()
        case "const" => ConstT()
        case "available" => AvailableT()
        case "ends" => EndsT()
        case "in" => InT()
        case "resource" => ResourceT()
        case "disown" => DisownT()
        case "private" => PrivateT()
        case id => IdentifierT(id)
    }

    /* comment parser: must be parsed before [forwardSlashP] */
    private def longCommentP = {
        val beginning = """/\*""".r ^^ { _ => () }

        beginning ~! """((.|\n)*?)\*/""".r ^^ ( _ => CommentT() )
    }


    /* comment parser: must be parsed before [forwardSlashP] */
    private def commentP = {
        val beginning = """//""".r ^^ { _ => () }

        beginning ~! """.*""".r ^^ ( _ => CommentT() )
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
    private def coloncolonP = """::""".r ^^ (_ => ColonColonT())
    private def atP = """@""".r ^^ (_ => AtT())
    private def lBracketP = """\[""".r ^^ (_ => LBracketT())
    private def rBracketP = """\]""".r ^^ (_ => RBracketT())
    private def pipeP = """\|""".r ^^ (_ => PipeT())
    private def chevP = """>>""".r ^^ (_ => ChevT())


    private def oneToken: Parser[Token] =

    (
        longCommentP | commentP | decimalP | hexP | stringLitP | atomP |

        lBraceP | rBraceP | lParenP | rParenP | commaP | dotP | semicolonP |

        /* order is important here because some tokens contain the others */
        chevP | gtEqP | ltEqP | eqEqP | notEqP | rightArrowP | bigRightArrowP | ltP | gtP | eqP |

        plusP | starP | forwardSlashP | minusP | coloncolonP | atP | lBracketP | rBracketP | pipeP
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

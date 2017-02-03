import scala.util.parsing.combinator._
import scala.collection._

/*

E ::= E9
E9 ::= E8 and E9 | E8
E8 ::= E7 or E8 | E7
E7 ::= E6 == E7 | E6 != E7 | E6 < E7 | E6 > E7 | E6 <= E7 | E6 >= E7 | E6
E6 ::= E5 + E6 | E5
E5 ::= E4 - E5 | E4
E4 ::= E3 * E4 | E3
E3 ::= E2 / E3 | E2
E2 ::= not E1 | E1
E1 ::= ( E ) | n | x | true | false

 */

object ObsidianParser extends Parsers {
    override type Elem = Token

    private def parseNumLiteral : Parser[NumLiteral] = {
        accept("numeric literal", { case NumLiteralT(n) => NumLiteral(n) })
    }

    private def parseIdString : Parser[String] = {
        accept("identifier", { case IdentifierT(name) => name })
    }

    private def parseVariable : Parser[Variable] = {
        parseIdString ^^ { case name => Variable(name) }
    }

    private def parseField : Parser[Field] = {
        val unpackId = accept("field identifier", { case IdentifierT(name) => name })
        unpackId ~ DotT ~ unpackId ^^ { case x ~ _ ~ f => Field(x, f) }
    }

    private def parseArgList : Parser[Seq[AST]] = repsep(parseExpr, CommaT)

    private def parseStatement : Parser[AST] = parseAtomicStatement ~ opt(parseStatement) ^^ {
            case s ~ None => s
            case s1 ~ Some(s2) => Sequence(s1, s2)
        }

    private def parseAtomicStatement : Parser[AST] = {
        val parseReturn = ReturnT ~ parseExpr ~ SemicolonT ^^ {
            case _ ~ e ~ _ => Return(e)
        }
        val parseTransition = RightArrowT ~ parseIdString ~ SemicolonT ^^ {
            case _ ~ name ~ _ => Transition(name)
        }

        val parseFieldAssignment = parseField ~ EqT ~ parseExpr ~ SemicolonT ^^ {
            case f ~ _ ~ e ~ _ => Assignment(f, e)
        }
        val parseVarAssignment = parseVariable ~ EqT ~ parseExpr ~ SemicolonT ^^ {
            case x ~ _ ~ e ~ _ => Assignment(x, e)
        }


        val parseThrow = ThrowT ~ SemicolonT ^^ { case _ => Throw() }

        val parseOnlyIf = IfT ~ parseExpr ~ LBraceT ~ parseStatement ~ RBraceT
        val parseElse = ElseT ~ LBraceT ~ parseStatement ~ RBraceT

        val parseIf = parseOnlyIf ~ opt(parseElse) ^^ {
            case _ ~ e ~ _ ~ s ~ _ ~ None => If(e, s)
            case _ ~ e ~ _ ~ s1 ~ _ ~ Some(_ ~ _ ~ s2 ~ _) => IfThenElse(e, s1, s2)
        }

        val parseTryCatch = TryT ~ LBraceT ~ parseStatement ~ RBraceT ~
                            CatchT ~ LBraceT ~ parseStatement ~ RBraceT ^^ {
            case _ ~ _ ~ s1 ~ _ ~ _ ~ _ ~ s2 ~ _ => TryCatch(s1, s2)
        }

        parseReturn | parseTransition | parseFieldAssignment |
        parseVarAssignment | parseThrow | parseIf | parseTryCatch
    }


    /* this is a lot better than manually writing code for all the AST operators */
    private def parseBinary(t : Token,
                            makeExpr : (AST, AST) => AST,
                            nextParser : Parser[AST]
                           ) : Parser[AST] = {
        val hasOpParser = t ~ parseBinary(t, makeExpr, nextParser) ^^ {
            case _ ~ e => e
        }

        nextParser ~ opt(hasOpParser) ^^ {
            case e ~ None => e
            case e1 ~ Some(e2) => makeExpr(e1, e2)
        }
    }

    private def parseUnary(t : Token,
                   makeExpr : AST => AST,
                   nextParser : Parser[AST]
                  ) : Parser[AST] = {
        val hasOpParser = t ~ parseUnary(t, makeExpr, nextParser) ^^ {
            case _ ~ e => makeExpr(e)
        }

        hasOpParser | nextParser
    }

    private def parseExpr = parseAnd
    private def parseAnd = parseBinary(AndT, Conjunction.apply, parseOr)
    private def parseOr = parseBinary(OrT, Disjunction.apply, parseEq)

    private def parseEq = parseBinary(EqEqT, Equals.apply, parseNeq)
    private def parseNeq = parseBinary(NotEqT, NotEquals.apply, parseGt)
    private def parseGt = parseBinary(GtT, GreaterThan.apply, parseLt)
    private def parseLt = parseBinary(LtT, LessThan.apply, parseLtEq)
    private def parseLtEq = parseBinary(LtEqT, LessThanOrEquals.apply, parseGtEq)
    private def parseGtEq = parseBinary(GtEqT, GreaterThanOrEquals.apply, parseAddition)

    private def parseAddition = parseBinary(PlusT, Add.apply, parseSubtraction)
    private def parseSubtraction = parseBinary(MinusT, Subtract.apply, parseMultiplication)
    private def parseMultiplication = parseBinary(StarT, Multiply.apply, parseDivision)
    private def parseDivision = parseBinary(ForwardSlashT, Divide.apply, parseNot)
    private def parseNot = parseUnary(NotT, LogicalNegation.apply, parseExprBottom)


    private def parseExprBottom : Parser[AST] = {
        val parenExpr = LParenT ~ parseExpr ~ RParenT ^^ {
            case _ ~ e ~ _ => e
        }

        parseNumLiteral | parseField | parseVariable | parenExpr
    }

    def parseAST(tokens : Seq[Token]) : Either[String, AST] = {
        val reader = new TokenReader(tokens)
        parseStatement(reader) match {
            case Success(result, _) => Right(result)
            case Failure(msg ,_) => Left("FAILURE: " + msg)
            case Error(msg , _) => Left("ERROR: " + msg)
        }
    }
}

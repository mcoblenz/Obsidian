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

    private def parseVariable : Parser[Variable] = {
        accept("variable identifier", { case IdentifierT(name) => Variable(name) })
    }

    private def parseField : Parser[Field] = {
        val unpackId = accept("field identifier", { case IdentifierT(name) => name})
        unpackId ~ DotT ~ unpackId ^^ { case x ~ _ ~ f => Field(x, f) }
    }

    private def parseArgList : Parser[Seq[AST]] = {
        val moreP = parseExpr ~ CommaT ~ parseArgList ^^ { case e1 ~ _ ~ theRest => Seq(e1) ++ theRest }
        val oneMoreP = parseExpr ^^ { case e => Seq(e) }
        (moreP | oneMoreP) ^^ {
            case s : Seq[AST] => s
            case _ => Seq()
        }
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
        parseExpr(reader) match {
            case Success(result, _) => Right(result)
            case Failure(msg ,_) => Left("FAILURE: " + msg)
            case Error(msg , _) => Left("ERROR: " + msg)
        }
    }
}

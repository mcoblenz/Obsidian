package edu.cmu.cs.obsidian.parser

import edu.cmu.cs.obsidian.lexer._

import scala.collection._
import scala.util.parsing.combinator._
import scala.util.parsing.input.Position

/*

The following sort of expression grammar is needed to eliminate left recursion
and allow operators to have the appropriate precedence

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

object Parser extends Parsers {
    class ParseException (val message : String) extends Exception {

    }

    override type Elem = Token

    type Identifier = (String, Position)

    /* other parsers need to know the position of the identifier, but adding identifiers
     * to the AST itself is clunky and adds unnecessary indirection */
    private def parseId: Parser[Identifier] = {
        accept("identifier", { case t@IdentifierT(name) => (name, t.pos) })
    }

    private def parseTypeModifier = {
        val linearP = ReadOnlyT() ^^ (t => IsReadOnly().setLoc(t))
        val borrowsP = BorrowsT() ^^ (t => IsBorrowed().setLoc(t))
        val remoteP = RemoteT() ^^ (t => IsRemote().setLoc(t))
        linearP | remoteP
    }

    private def parseType = {
        val nonPrim = rep(parseTypeModifier) ~ parseId ~ opt(DotT() ~ parseId) ^^ {
            case mod ~ nameC ~ Some(_ ~ nameS) => AstStateType(mod, nameC._1, nameS._1).setLoc(nameC)
            case mod ~ nameC ~ None => AstContractType(mod, nameC._1).setLoc(nameC)
        }

        val intPrim = IntT() ^^ { t => AstIntType().setLoc(t) }
        val boolPrim = BoolT() ^^ { t => AstBoolType().setLoc(t) }
        val stringPrim = StringT() ^^ { t => AstStringType().setLoc(t) }

        nonPrim | intPrim | boolPrim | stringPrim
    }

    private def parseArgList: Parser[Seq[Expression]] = repsep(parseExpr, CommaT())

    private def parseArgDefList: Parser[Seq[VariableDecl]] = {
        val oneDecl = parseType ~ parseId ^^ {
            case typ ~ name => VariableDecl(typ, name._1).setLoc(typ)
        }
        repsep(oneDecl, CommaT())
    }

    private def parseBody: Parser[Seq[Statement]] =
        parseAtomicStatement ~ opt(parseBody) ^^ {
            case s ~ None => Seq(s)
            case s1 ~ Some(s2) => s1 +: s2
        }

    private def parseAtomicStatement: Parser[Statement] = {
        val parseReturn = ReturnT() ~ opt(parseExpr) ~! SemicolonT() ^^ {
            case ret ~ Some(e) ~ _ => ReturnExpr(e).setLoc(ret)
            case ret ~ None ~ _ => Return().setLoc(ret)
        }

        val parseUpdate = {
            val oneUpdate = parseId ~! EqT() ~! parseExpr ^^ {
                case f ~ _ ~ e => (Variable(f._1).setLoc(f), e)
            }
            LParenT() ~ LBraceT() ~ repsep(oneUpdate, CommaT()) ~
                RBraceT() ~ RParenT() ^^ {
                case _ ~ _ ~ updates ~ _ ~ _ => updates
            }
        }

        val parseTransition = RightArrowT() ~ parseId ~
                              opt(parseUpdate) ~! SemicolonT() ^^ {
            case arrow ~ name ~ None ~ _ => Transition(name._1, List.empty).setLoc(arrow)
            case arrow ~ name ~ Some(updates) ~ _ => Transition(name._1, updates).setLoc(arrow)
        }

        val parseVarDeclAssn =
            parseType ~ parseId ~ EqT() ~! parseExpr ~! SemicolonT() ^^ {
                case typ ~ name ~ _ ~ e ~ _ =>
                    VariableDeclWithInit(typ, name._1, e).setLoc(typ)
        }

        val parseVarDecl =
            parseType ~ parseId ~! SemicolonT() ^^ {
                case typ ~ name ~ _ => VariableDecl(typ, name._1).setLoc(typ)
            }

        val assign = EqT() ~! parseExpr ^^ {
            case eqSign ~ e2 => (e1: Expression) => Assignment(e1, e2).setLoc(eqSign)
        }

        val parseThrow = ThrowT() ~! SemicolonT() ^^ { case t ~ _ => Throw().setLoc(t) }

        val parseOnlyIf = IfT() ~! parseExpr ~! LBraceT() ~! parseBody ~! RBraceT()
        val parseElse = ElseT() ~! LBraceT() ~! parseBody ~! RBraceT()

        val parseIf = parseOnlyIf ~ opt(parseElse) ^^ {
            case _if ~ e ~ _ ~ s ~ _ ~ None => If(e, s).setLoc(_if)
            case _if ~ e ~ _ ~ s1 ~ _ ~ Some(_ ~ _ ~ s2 ~ _) => IfThenElse(e, s1, s2).setLoc(_if)
        }

        val parseTryCatch = TryT() ~! LBraceT() ~! parseBody ~! RBraceT() ~!
                            CatchT() ~! LBraceT() ~! parseBody <~ RBraceT() ^^ {
            case _try ~ _ ~ s1 ~ _ ~ _ ~ _ ~ s2 => TryCatch(s1, s2).setLoc(_try)
        }

        val parseCase = CaseT() ~! parseId ~! LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case _case ~ name ~ _ ~ body ~ _ => SwitchCase(name._1, body).setLoc(_case)
        }

        val parseSwitch =
            SwitchT() ~! parseExpr ~! LBraceT() ~! rep(parseCase) ~! RBraceT() ^^ {
                case switch ~ e ~ _ ~ cases ~ _ => Switch(e, cases).setLoc(switch)
        }

        /* allow arbitrary expr as a statement and then check at later stage if
         * the expressions makes sense as the recipient of an assignment or
         * as a side-effect statement (e.g. func invocation) */
        val parseExprFirst = {
            parseExpr ~ opt(assign) ~ SemicolonT() ^^ {
                case e ~ Some(assn) ~ _ => assn(e)
                case e ~ None ~ _ => e
            }
        }

        parseReturn | parseTransition | parseThrow |
        parseVarDeclAssn | parseVarDecl | parseIf | parseSwitch |
        parseTryCatch | parseExprFirst
    }


    /* this is a lot better than manually writing code for all the AST operators */
    private def parseBinary(t: Token,
                            makeExpr: (Expression, Expression) => Expression,
                            nextParser: Parser[Expression]
                           ): Parser[Expression] = {
        val hasOpParser = t ~ parseBinary(t, makeExpr, nextParser) ^^ {
            case _ ~ e => e
        }

        nextParser ~ opt(hasOpParser) ^^ {
            case e ~ None => e
            case e1 ~ Some(e2) => makeExpr(e1, e2).setLoc(e1)
        }
    }

    private def parseUnary(t: Token,
                   makeExpr: Expression => Expression,
                   nextParser: Parser[Expression]
                  ): Parser[Expression] = {
        val hasOpParser = t ~ parseUnary(t, makeExpr, nextParser) ^^ {
            case op ~ e => makeExpr(e).setLoc(op)
        }

        hasOpParser | nextParser
    }

    private def parseExpr = parseAnd
    private def parseAnd = parseBinary(AndT(), Conjunction.apply, parseOr)
    private def parseOr = parseBinary(OrT(), Disjunction.apply, parseEq)

    private def parseEq = parseBinary(EqEqT(), Equals.apply, parseNeq)
    private def parseNeq = parseBinary(NotEqT(), NotEquals.apply, parseGt)
    private def parseGt = parseBinary(GtT(), GreaterThan.apply, parseLt)
    private def parseLt = parseBinary(LtT(), LessThan.apply, parseLtEq)
    private def parseLtEq = parseBinary(LtEqT(), LessThanOrEquals.apply, parseGtEq)
    private def parseGtEq = parseBinary(GtEqT(), GreaterThanOrEquals.apply, parseAddition)

    private def parseAddition = parseBinary(PlusT(), Add.apply, parseSubtraction)
    private def parseSubtraction = parseBinary(MinusT(), Subtract.apply, parseMultiplication)
    private def parseMultiplication = parseBinary(StarT(), Multiply.apply, parseDivision)
    private def parseDivision = parseBinary(ForwardSlashT(), Divide.apply, parseNot)
    private def parseNot = parseUnary(NotT(), LogicalNegation.apply, parseExprBottom)


    /* parsing of invocations and dereferences is used in both statements and expressions */

    private def parseLocalInv = {
        parseId ~ LParenT() ~ parseArgList ~ RParenT() ^^ {
            case name ~ _ ~ args ~ _ => LocalInvocation(name._1, args).setLoc(name)
        }
    }

    /* avoids left recursion by parsing from the dot, e.g. ".f(a)", not "x.f(a)" */

    type DotExpr = Either[Identifier, (Identifier, Seq[Expression])]

    private def foldDotExpr(e: Expression, dots: Seq[DotExpr]): Expression = {
        dots.foldLeft(e)(
            (e: Expression, inv: DotExpr) => inv match {
                case Left(fieldName) => Dereference(e, fieldName._1).setLoc(fieldName)
                case Right((funcName, args)) => Invocation(e, funcName._1, args).setLoc(funcName)
            }
        )
    }

    private def parseDots: Parser[Expression => Expression] = {
        val parseOne = DotT() ~! parseId ~ opt(LParenT() ~ parseArgList ~ RParenT()) ^^ {
            case _ ~ name ~ Some(_ ~ args ~ _) => Right((name, args))
            case _ ~ name ~ None => Left(name)
        }

        rep(parseOne) ^^ {
            case lst => (e: Expression) => foldDotExpr(e, lst)
        }
    }

    val parseStringLiteral = {
        accept("string literal", { case t@StringLiteralT(s) => StringLiteral(s).setLoc(t) })
    }

    private def parseExprBottom: Parser[Expression] = {
        val parenExpr = LParenT() ~! parseExpr ~! RParenT() ^^ {
            case _ ~ e ~ _ => e
        }

        val parseVar = parseId ^^ { (id: Identifier) => Variable(id._1).setLoc(id) }

        val parseNumLiteral = {
            accept("numeric literal", { case t@NumLiteralT(n) => NumLiteral(n).setLoc(t) })
        }


        val parseNew = {
            NewT() ~! parseId ~! LParenT() ~! parseArgList ~! RParenT() ^^ {
                case _new ~ name ~ _ ~ args ~ _ => Construction(name._1, args).setLoc(_new)
            }
        }

        val parseTrue = { accept("bool literal", { case t@TrueT() => TrueLiteral().setLoc(t) })}
        val parseFalse = { accept("bool literal", { case t@FalseT() => FalseLiteral().setLoc(t) })}

        val fail = failure("expression expected")

        val parseLiterals = parseTrue | parseFalse | parseNumLiteral | parseStringLiteral

        val parseThis = { ThisT() ^^ { case t => This().setLoc(t) } }

        val simpleExpr = parseThis | parseNew | parseLocalInv | parseLiterals | parseVar | parenExpr | fail

        simpleExpr ~ parseDots ^^ { case e ~ applyDots => applyDots(e) }
    }

    private def parseFieldDecl = {
        parseType ~ parseId ~! SemicolonT() ^^ {
            case typ ~ name ~ semi => Field(typ, name._1).setLoc(semi)
        }
    }

    private def parseReturns = ReturnsT() ~! parseType ^^ {
        case _ ~ typ => typ
    }

    private def parseFuncDecl = {
        FunctionT() ~! parseId ~! LParenT() ~! parseArgDefList ~! RParenT() ~!
            opt(parseReturns) ~! LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case f ~ name ~ _ ~ args ~ _ ~ ret ~ _ ~ body ~ _ =>
                Func(name._1, args, ret, body).setLoc(f)
        }
    }

    private def parseEnsures = {
        EnsuresT() ~! parseExpr ~! SemicolonT() ^^ {
            case ensures ~ expr ~ _ => Ensures(expr).setLoc(ensures)
        }
    }




    private def parseTransDecl = {
        TransactionT() ~! (parseId | MainT()) ~! LParenT() ~! parseArgDefList ~! RParenT() ~!
        opt(parseReturns) ~! rep(parseEnsures) ~! LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case t ~ name ~ _ ~ args ~ _ ~ ret ~ ensures ~ _ ~ body ~ _ =>
                val nameString: String = name match {
                    case MainT() => "main"
                    case (id: Identifier) => id._1
                }
                Transaction(nameString, args, ret, ensures, body).setLoc(t)
        }
    }

    private def parseStateDecl = {
        StateT() ~! parseId ~! LBraceT() ~! rep(parseDecl) ~! RBraceT() ^^ {
            case st ~ name ~ _ ~ defs ~ _ => State(name._1, defs).setLoc(st)
        }
    }

    // TODO
    // maybe we can check here that the constructor has the appropriate name?
    private def parseConstructor = {
        parseId ~ LParenT() ~! parseArgDefList ~! RParenT() ~! LBraceT() ~!
        parseBody ~! RBraceT() ^^ {
            case name ~ _ ~ args ~ _ ~ _ ~ body ~ _ => Constructor(name._1, args, body).setLoc(name)
        }
    }

    private def parseDecl: Parser[Declaration] = {
        parseFieldDecl | parseFuncDecl | parseTransDecl |
        parseStateDecl | parseConstructor | parseContractDecl | failure("declaration expected")
    }

    private def parseContractModifier = {
        val mainP = MainT() ^^ (t => IsMain().setLoc(t))
        val uniqueP = UniqueT() ^^ (t => IsOwned().setLoc(t))
        val sharedP = SharedT() ^^ (t => IsShared().setLoc(t))
        opt(mainP | uniqueP | sharedP)
    }

    private def parseContractDecl = {
        parseContractModifier ~ ContractT() ~! parseId ~!
            LBraceT() ~! rep(parseDecl) ~! RBraceT() ^^ {
            case mod ~ ct ~ name ~ _ ~ defs ~ _ => Contract(mod, name._1, defs).setLoc(ct)
        }
    }

    private def parseImport = {
        ImportT() ~! parseStringLiteral ^^ {
            case _import ~ StringLiteral(name) => Import(name).setLoc(_import)
        }
    }

    private def parseProgram = {
        phrase(rep(parseImport) ~ rep1(parseContractDecl)) ^^ {
            case imports ~ contracts => Program(imports, contracts).setLoc(contracts.head)
        }
    }

    def parseProgram(tokens: Seq[Token]): Either[String, Program] = {
        val reader = new TokenReader(tokens)
        parseProgram(reader) match {
            case Success(result, _) => Right(result)
            case Failure(msg , _) => Left(s"FAILURE: $msg")
            case Error(msg , next) => {
                val line = next.first.pos.line
                val col = next.first.pos.column
                Left(s"Error: `$msg at $line:$col")
            }
        }
    }

    def parseFileAtPath(srcPath: String, printTokens: Boolean): Program = {
        val bufferedSource = scala.io.Source.fromFile(srcPath)
        val src = try bufferedSource.getLines() mkString "\n" finally bufferedSource.close()

        val tokens: Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => throw new ParseException(msg)
            case Right(ts) => ts
        }

        if (printTokens) {
            println("Tokens:")
            println()
            println(tokens)
            println()
        }

        val ast: Program = parseProgram(tokens) match {
            case Left(msg) => throw new ParseException(msg + " in " + srcPath)
            case Right(tree) => tree
        }

        ast
    }
}

package edu.cmu.cs.obsidian.parser

import edu.cmu.cs.obsidian.lexer._

import scala.collection.immutable.TreeSet
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

    private def firstIndexOf(s: String, p: Function[Char, Boolean]): Int = {
        var i = 0
        for (c <- s) {
            if (p(c)) return i
            i += 1
        }
        i
    }

    // qualifies, e.g. "___C", "C" as uppercase, but not "__c", "c"
    private def upperCasePred(s: String): Boolean = {
        val firstUpper = firstIndexOf(s, (c: Char) => c.isUpper)
        val firstLower = firstIndexOf(s, (c: Char) => c.isLower)
        firstUpper < firstLower
    }

    private def parseIdLower: Parser[Identifier] = {
        accept("lowercase identifier",
            { case t@IdentifierT(name) if !upperCasePred(name) => (name, t.pos) })
    }

    private def parseIdUpper: Parser[Identifier] = {
        accept("uppercase identifier",
            { case t@IdentifierT(name) if upperCasePred(name) => (name, t.pos) })
    }

    private def parseTypeModifier = {
        val linearP = ReadOnlyT() ^^ (t => IsReadOnly().setLoc(t))
        val remoteP = RemoteT() ^^ (t => IsRemote().setLoc(t))
        linearP | remoteP
    }

    private def parseType = {
        val parsePathNode = (parseIdLower | ThisT() | ParentT()) ~ DotT() ^^ {
            case name ~ DotT() =>
                name match {
                    case _: ThisT => "this"
                    case _: ParentT => "parent"
                    case id => id.asInstanceOf[Identifier]._1
                }
        }

        val nonPrim = rep(parseTypeModifier) ~ rep(parsePathNode) ~
            parseIdUpper ~ opt(DotT() ~ parseIdUpper) ^^ {
            case mods ~ path ~ cName ~ Some(sName) if path.isEmpty =>
                AstStateType(mods, cName._1, sName._2._1).setLoc(cName)
            case mods ~ path ~ cName ~ None if path.isEmpty =>
                AstContractType(mods, cName._1).setLoc(cName)
            case mods ~ path ~ cName ~ None =>
                AstPathContractType(mods, path, cName._1).setLoc(cName)
            case mods ~ path ~ cName ~ Some(sName) =>
                AstPathStateType(mods, path, cName._1, sName._2._1).setLoc(cName)
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
            val oneUpdate = parseIdLower ~! EqT() ~! parseExpr ^^ {
                case f ~ _ ~ e => (Variable(f._1).setLoc(f), e)
            }
            LParenT() ~ LBraceT() ~ repsep(oneUpdate, CommaT()) ~
                RBraceT() ~ RParenT() ^^ {
                case _ ~ _ ~ updates ~ _ ~ _ => updates
            }
        }

        val parseTransition = RightArrowT() ~ parseIdUpper ~
                              opt(parseUpdate) ~! SemicolonT() ^^ {
            case arrow ~ name ~ None ~ _ => Transition(name._1, List.empty).setLoc(arrow)
            case arrow ~ name ~ Some(updates) ~ _ => Transition(name._1, updates).setLoc(arrow)
        }

        val parseVarDeclAssn =
            parseType ~ parseIdLower ~ EqT() ~! parseExpr ~! SemicolonT() ^^ {
                case typ ~ name ~ _ ~ e ~ _ =>
                    VariableDeclWithInit(typ, name._1, e).setLoc(typ)
        }

        val parseVarDecl =
            parseType ~ parseIdLower ~! SemicolonT() ^^ {
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

        val parseCase = CaseT() ~! parseIdUpper ~! LBraceT() ~! parseBody ~! RBraceT() ^^ {
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
        parseIdLower ~ LParenT() ~ parseArgList ~ RParenT() ^^ {
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
        val parseOne = DotT() ~! parseIdLower ~ opt(LParenT() ~ parseArgList ~ RParenT()) ^^ {
            case _ ~ name ~ Some(_ ~ args ~ _) => Right((name, args))
            case _ ~ name ~ None => Left(name)
        }

        rep(parseOne) ^^ (lst => (e: Expression) => foldDotExpr(e, lst))
    }

    private val parseStringLiteral = {
        accept("string literal", { case t@StringLiteralT(s) => StringLiteral(s).setLoc(t) })
    }

    private def parseExprBottom: Parser[Expression] = {
        val parenExpr = LParenT() ~! parseExpr ~! RParenT() ^^ {
            case _ ~ e ~ _ => e
        }

        val parseVar = parseIdLower ^^ { (id: Identifier) => Variable(id._1).setLoc(id) }

        val parseNumLiteral = {
            accept("numeric literal", { case t@NumLiteralT(n) => NumLiteral(n).setLoc(t) })
        }


        val parseNew = {
            NewT() ~! parseIdUpper ~! LParenT() ~! parseArgList ~! RParenT() ^^ {
                case _new ~ name ~ _ ~ args ~ _ => Construction(name._1, args).setLoc(_new)
            }
        }

        val parseTrue = { accept("bool literal", { case t@TrueT() => TrueLiteral().setLoc(t) })}
        val parseFalse = { accept("bool literal", { case t@FalseT() => FalseLiteral().setLoc(t) })}

        val parseLiterals = parseTrue | parseFalse | parseNumLiteral | parseStringLiteral

        val parseThis = { ThisT() ^^ (t => This().setLoc(t))}
        val parseParent = { ParentT() ^^ (p => Parent().setLoc(p))}

        val fail = failure("expression expected")

        val simpleExpr = parseThis | parseParent | parseNew | parseLocalInv |
            parseLiterals | parseVar | parenExpr | fail

        simpleExpr ~ parseDots ^^ { case e ~ applyDots => applyDots(e) }
    }

    private def parseFieldDecl = {
        opt(ConstT()) ~ parseType ~ parseIdLower ~! SemicolonT() ^^ {
            case isConst ~ typ ~ name ~ _ =>
                isConst match {
                    case Some(constToken) =>
                        Field(isConst = true, typ, name._1).setLoc(constToken)
                    case None =>
                        Field(isConst = false, typ, name._1).setLoc(typ)
            }
        }
    }

    private def parseReturns = ReturnsT() ~! parseType ^^ {
        case _ ~ typ => typ
    }

    private def parseStatesList: Parser[Set[String]] =
        rep(parseIdUpper ~ OrT()) ~! parseIdUpper ^^ {
        case ors ~ last => ors.map(_._1._1).toSet + last._1
    }

    private def parseEndsInState: Parser[Set[String]] =
        EndsT() ~! InT() ~! parseStatesList ^^ {
            case _ ~ s => s
        }

    private def parseFuncDecl = {
        FunctionT() ~! parseIdLower ~! LParenT() ~! parseArgDefList ~! RParenT() ~!
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

    private def parseAvailableIn = {
        AvailableT() ~> InT() ~> rep(parseId)
    }

    private def parseTransDecl = {
        TransactionT() ~! (parseIdLower | MainT()) ~! LParenT() ~! parseArgDefList ~! RParenT() ~!
        opt(parseReturns) ~! opt(parseAvailableIn) ~! opt(parseEndsInState) ~! rep(parseEnsures) ~!

            LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case t ~ name ~ _ ~ args ~ _ ~ returnType ~ availableIn ~
                 ensuresState ~ ensures ~ _ ~ body ~ _ =>
                val nameString = name match {
                    case MainT() => "main"
                    case id => id.asInstanceOf[Identifier]._1
                }
                val availableTransactions = availableIn match {
                    case None => List.empty
                    case Some(l) => l
                }
                Transaction(nameString, args, returnType, availableTransactions, ensures,
                     ensuresState, body).setLoc(t)
        }
    }

    private def parseStateDecl = {
        StateT() ~! parseIdUpper ~! LBraceT() ~! rep(parseDeclInState) ~! RBraceT() ^^ {
            case st ~ name ~ _ ~ defs ~ _ => State(name._1, defs).setLoc(st)
        }
    }

    // maybe we can check here that the constructor has the appropriate name?
    private def parseConstructor() = {
        parseIdUpper ~ LParenT() ~! parseArgDefList ~! RParenT() ~! opt(parseEndsInState) ~!
        LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case name ~ _ ~ args ~ _ ~ ensuresState ~ _ ~ body ~ _ =>
                Constructor(name._1, args, ensuresState, body).setLoc(name)

        }
    }

    private def parseDeclInState: Parser[Declaration] = {
        parseFieldDecl | parseFuncDecl |
        parseStateDecl | parseConstructor | parseContractDecl | failure("declaration expected")
    }

    private def parseDeclInContract: Parser[Declaration] = {
        parseDeclInState | parseTransDecl
    }

    private def parseContractModifier = {
        val mainP = MainT() ^^ (t => IsMain().setLoc(t))
        val ownedP = OwnedT() ^^ (t => IsOwned().setLoc(t))
        val sharedP = SharedT() ^^ (t => IsShared().setLoc(t))
        opt(mainP | ownedP | sharedP)
    }

    private def parseContractDecl = {
        parseContractModifier ~ ContractT() ~! parseIdUpper ~!
            LBraceT() ~! rep(parseDeclInContract) ~! RBraceT() ^^ {
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
            case Error(msg , next) =>
                val line = next.first.pos.line
                val col = next.first.pos.column
                Left(s"Error: $msg at $line:$col")
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

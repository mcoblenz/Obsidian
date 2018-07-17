package edu.cmu.cs.obsidian.parser

import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.typecheck._

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

    class ParseException(val message: String) extends Exception {

    }

    override type Elem = Token

    type Identifier = (String, Position)

    /* other parsers need to know the position of the identifier, but adding identifiers
     * to the AST itself is clunky and adds unnecessary indirection */
    private def parseId: Parser[Identifier] = {
        accept("identifier", {
            case t@IdentifierT(name) => (name, t.pos)
            case t@ThisT() => ("this", t.pos)})
    }

    private def parseIdAlternatives: Parser[Seq[Identifier]] = {
        val withParens = LParenT() ~ repsep(parseId, PipeT()) ~ RParenT() ^^ {
            case _ ~ seq ~ _ => seq
        }
        val withoutParens = repsep(parseId, PipeT())

        withParens | withoutParens
    }

    def resolvePermission(ident: String): Option[Permission] = {
        if (ident == "Shared") {
            Some(Shared())
        }
        else if (ident == "Owned") {
            Some(Owned())
        }
        else if (ident == "Unowned") {
            Some(Unowned())
        }
        else {
            None
        }
    }

    private def parseType: Parser[ObsidianType] = {
        def parseDotPath: Parser[Seq[Identifier]] = DotT() ~ (parseId | ParentT()) ~ opt(parseDotPath) ^^ {
            case _ ~ ident ~ rest => {
                val identifier: Identifier = ident match {
                    case t: Token => (t.toString, t.pos)
                    // For obscure type erasure reasons, a pattern match on Identifier type doesn't work.
                    case ident => ident.asInstanceOf[Identifier]
                }
                rest match {
                    case Some(path) => List(identifier) ++ path
                    case None => List(identifier)
                }
            }
        }



        //        val parseNonPrimitive: Parser[NonPrimitiveType] =
        //            rep(parseTypeModifier) ~ (parseId | ThisT() | ParentT()) ~ opt(parseDotPath) ^^ {
        //                case mods ~ id ~ path => {
        //                    val (identString, position: Position) = id match {
        //                        case t: Token => (t.toString, t.pos)
        //                        // For obscure type erasure reasons, a pattern match on Identifier type doesn't work.
        //                        case ident => (ident.asInstanceOf[Identifier]._1, ident.asInstanceOf[Identifier]._2)
        //                    }
        //                    path match {
        //                        case Some(idents) =>
        //                            val pathStrings = List(identString) ++ idents.map(ident => ident._1)
        //                            UnresolvedNonprimitiveType(pathStrings, mods.toSet, Shared()).setLoc(position)
        //                        case None =>
        //                            val permission =
        //                                if (mods.contains(IsOwned())) {
        //                                    Owned()
        //                                }
        //                                else {
        //                                    Shared()
        //                                }
        //
        //                            UnresolvedNonprimitiveType(List(identString), mods.toSet, permission).setLoc(position)
        //                    }
        //
        //                }
        // For now, support only one state specification

        val parseNonPrimitive: Parser[NonPrimitiveType] = {
             opt(RemoteT()) ~ parseId ~ opt(AtT() ~! parseIdAlternatives) ^^ {
                case remote ~ id ~ permissionToken => {
                    val isRemote = remote.isDefined
                    val typ = extractTypeFromPermission(permissionToken, id._1, isRemote)
                    typ.setLoc(id)
                }
            }
        }

        val intPrim = IntT() ^^ { t => IntType().setLoc(t) }
        val boolPrim = BoolT() ^^ { t => BoolType().setLoc(t) }
        val stringPrim = StringT() ^^ { t => StringType().setLoc(t) }

        parseNonPrimitive | intPrim | boolPrim | stringPrim
    }

    private def extractTypeFromPermission(permission: Option[~[Token, Seq[Identifier]]], name: String, isRemote: Boolean): NonPrimitiveType = {
        permission match {
            case None => ContractReferenceType(ContractType(name), Inferred(), isRemote)
            case Some(_ ~ permissionIdentSeq) =>
                if (permissionIdentSeq.size == 1) {
                    val thePermissionOrState = permissionIdentSeq.head
                    val permission = resolvePermission(thePermissionOrState._1)
                    permission match {
                        case None => StateType(name, thePermissionOrState._1, isRemote)
                        case Some(p) => ContractReferenceType(ContractType(name), p, isRemote)
                    }
                }
                else {
                    val stateNames = permissionIdentSeq.map(_._1)
                    StateType(name, stateNames.toSet, isRemote)
                }
        }
    }

    private def parseArgList: Parser[Seq[Expression]] = repsep(parseExpr, CommaT())

    private def parseArgDefList: Parser[Seq[VariableDeclWithSpec]] = {
        val oneDecl = parseArgumentSpec ~ parseId ^^ {
            case (typIn, typOut) ~ name => VariableDeclWithSpec(typIn, typOut, name._1).setLoc(name)
        }
        repsep(oneDecl, CommaT())
    }

    private def parseArgumentSpec: Parser[(ObsidianType, ObsidianType)] = {
        parseType ~! opt(ChevT() ~! parseIdAlternatives) ^^ {
            case typ ~ permission => {
                typ match {
                    case t: NonPrimitiveType => {
                        permission match {
                            case None =>
                                val correctedType = t match {
                                    case ContractReferenceType(ct, Inferred(), isRemote) => ContractReferenceType(ct, ReadOnlyState(), isRemote)
                                    case _ => t
                                }
                                (correctedType, correctedType)
                            case Some(_ ~ idSeq) => {
                                val typOut = extractTypeFromPermission(permission, t.contractName, t.isRemote)
                                (t, typOut)
                            }
                        }
                    }
                    case _ => (typ, typ)
                }
            }
        }
    }

    private def parseBody: Parser[Seq[Statement]] =
        rep(parseAtomicStatement) ^^ {
            case statements => statements
        }

    private def parseAtomicStatement: Parser[Statement] = {
        val parseReturn = ReturnT() ~ opt(parseExpr) ~! SemicolonT() ^^ {
            case ret ~ Some(e) ~ _ => ReturnExpr(e).setLoc(ret)
            case ret ~ None ~ _ => Return().setLoc(ret)
        }

        val parseUpdate = {
            val oneUpdate = parseId ~! EqT() ~! parseExpr ^^ {
                case f ~ _ ~ e => (ReferenceIdentifier(f._1).setLoc(f), e)
            }
            LParenT() ~ repsep(oneUpdate, CommaT()) ~ RParenT() ^^ {
                case _ ~ updates ~ _ => updates
            }
        }

        val parseTransition = RightArrowT() ~ parseId ~
                              opt(parseUpdate) ~! SemicolonT() ^^ {
            case arrow ~ name ~ updates ~ _ => Transition(name._1, updates).setLoc(arrow)
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
            case eqSign ~ e2 => (e1: Expression) =>
                Assignment(e1, e2).setLoc(eqSign)
        }

        val parseThrow = ThrowT() ~! SemicolonT() ^^ {
            case t ~ _ => Throw().setLoc(t)
        }

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

        val parseStaticAssertion = LBracketT() ~! parseExpr ~ AtT() ~ parseIdAlternatives ~ RBracketT() ~! SemicolonT() ^^ {
            case _ ~ expr ~ at ~ idents ~ _ ~ _ => StaticAssert(expr, idents).setLoc(at)
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
        parseTryCatch | parseExprFirst | parseStaticAssertion
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

    private def parseExpr = parseDisown
    private def parseDisown = parseUnary(DisownT(), Disown.apply, parseAnd)
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


    private val parseStringLiteral: Parser[StringLiteral] = {
        val partialFunction: PartialFunction[Token, StringLiteral] = {
            case t@StringLiteralT(s) => StringLiteral(s).setLoc(t)
        }

        accept("string literal", partialFunction)
    }

    private def parseExprBottom: Parser[Expression] = {
        def parseLocalInv = {
            parseId ~ LParenT() ~ parseArgList ~ RParenT() ^^ {
                case name ~ _ ~ args ~ _ => LocalInvocation(name._1, args).setLoc(name)
            }
        }

        /* avoids left recursion by parsing from the dot, e.g. ".f(a)", not "x.f(a)" */
        type DotExpr = Either[Identifier, (Identifier, Seq[Expression])]

        def foldDotExpr(e: Expression, dots: Seq[DotExpr]): Expression = {
            dots.foldLeft(e)(
                (e: Expression, inv: DotExpr) => inv match {
                    case Left(fieldName) => Dereference(e, fieldName._1).setLoc(fieldName)
                    case Right((funcName, args)) => Invocation(e, funcName._1, args).setLoc(funcName)
                }
            )
        }

        def parseDots: Parser[Expression => Expression] = {
            val parseOne: Parser[DotExpr] = DotT() ~! parseId ~ opt(LParenT() ~ parseArgList ~ RParenT()) ^^ {
                case _ ~ name ~ Some(_ ~ args ~ _) => Right((name, args))
                case _ ~ name ~ None => Left(name)
            }

            rep(parseOne) ^^ ((lst: Seq[DotExpr]) => (e: Expression) => foldDotExpr(e, lst))
        }

        val parenExpr = LParenT() ~! parseExpr ~! RParenT() ^^ {
            case _ ~ e ~ _ => e
        }

        val parseVar = parseId ^^ { (id: Identifier) => ReferenceIdentifier(id._1).setLoc(id) }

        val parseStateInitializer = parseId ~ ColonColonT() ~! parseId ^^ {
            case stateName ~ _ ~ fieldName => StateInitializer(stateName, fieldName)
        }

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

        val parseLiterals: Parser[Expression] =
            parseTrue | parseFalse | parseNumLiteral | parseStringLiteral

        val parseThis = { ThisT() ^^ (t => This().setLoc(t))}
        val parseParent = { ParentT() ^^ (p => Parent().setLoc(p))}

        val fail = failure("expression expected")

        val simpleExpr: Parser[Expression] =
            parseThis | parseParent | parseNew | parseLocalInv |
            parseLiterals | parseStateInitializer | parseVar | parenExpr | fail

        simpleExpr ~ parseDots ^^ { case e ~ applyDots => applyDots(e) }
    }

    private def parseFieldDecl: Parser[Declaration] = {
        opt(ConstT()) ~ parseType ~ parseId ~! opt(parseAvailableIn) ~!
                opt(EqT() ~! parseExpr ~! failure("fields may only be assigned inside of transactions")) ~!
                SemicolonT() ^^ {
            case isConst ~ typ ~ name ~ availableIn ~ None ~ _ =>
                val availableInSet = availableIn match {
                    case Some(idents) => Some(idents.map(_._1))
                    case None => None
                }
                isConst match {
                    case Some(constToken) =>
                        Field(isConst = true, typ, name._1, availableInSet).setLoc(constToken)
                    case None =>
                        Field(isConst = false, typ, name._1, availableInSet).setLoc(typ)
            }
        }
    }
    private def parseReturns = ReturnsT() ~! parseType ^^ {
        case _ ~ typ => typ
    }

    private def parseStatesList: Parser[Set[Identifier]] =
        rep(parseId ~ CommaT()) ~! parseId ^^ {
        case ors ~ last => ors.map(_._1).toSet + last
    }

    private def parseEndsInState: Parser[Set[Identifier]] =
        EndsT() ~! InT() ~! parseStatesList ^^ {
            case _ ~ s => s
        }


    private def parseEndsInStateAlt: Parser[EndsInState] =
        EndsT() ~! InT() ~! parseStatesList ^^ {
            case _ ~ s => EndsInState(s)
        }

    private def parseEnsures = {
        EnsuresT() ~! parseExpr ~! SemicolonT() ^^ {
            case ensures ~ expr ~ _ => Ensures(expr).setLoc(ensures)
        }
    }

    private def parseAvailableIn: Parser[Set[Identifier]] = {
        AvailableT() ~! InT() ~! parseStatesList ^^ {
            case _ ~ s => s
        }
    }

    private def parseAvailableInAlt: Parser[AvailableIn] = {
        AvailableT() ~! InT() ~! parseStatesList ^^ {
            case _ ~ s => AvailableIn(s)
        }
    }

    case class AvailableIn (val identifiers: Set[Identifier])
    case class EndsInState (val identifiers: Set[Identifier])


    private def parseTransBody(isInterface:Boolean) =  {
        if(isInterface) SemicolonT() ^^ {
            case _ => Seq.empty[Statement]
        }
        else LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case _ ~ body ~ _ => body
        }
    }

    private def parseTransDecl(isInterface:Boolean)(contractName: String): Parser[Transaction] = {
        opt(StaticT()) ~ TransactionT() ~! (parseId | MainT()) ~! LParenT() ~! parseArgDefList ~! RParenT() ~!
          opt(parseReturns) ~! rep(parseEnsures) ~!  parseTransBody(isInterface) ^^ {
            case static ~ t ~ name ~ _ ~ args ~ _ ~ returns ~ ensures ~ body =>
                val isStatic = static match {
                    case None => false
                    case Some(static) => true
                }
                val nameString = name match {
                    case MainT() => "main"
                    case id => id.asInstanceOf[Identifier]._1
                }

                val (thisArg, filteredArgs) = args.headOption match {
                    case None => (None, args)
                    case Some(v) =>
                        if (v.varName == "this") {
                            if (!v.typIn.isInstanceOf[NonPrimitiveType]) {
                                return err("Type of 'this' must be a contract type.")
                            }
                            else {
                                (Some(v), args.tail)
                            }
                        } else {
                            (None, args)
                        }
                }

                val finalType = thisArg match {
                    case None => ContractReferenceType(ContractType(contractName), Shared(), false)
                    case Some(v) => v.typOut
                }

                // contract name is THIS as there is no way to access it at the moment
                val thisType = thisArg match {
                    case None => ContractReferenceType(ContractType(contractName), Shared(), false)
                    case Some(variableDecl) => variableDecl.typIn.asInstanceOf[NonPrimitiveType]

                }

                Transaction(nameString, filteredArgs, returns,
                    ensures, body, isStatic, thisType, finalType.asInstanceOf[NonPrimitiveType]).setLoc(t)
        }
    }

    private def parseStateDecl = {
        StateT() ~! parseId ~! opt(LBraceT() ~! rep(parseDeclInState) ~! RBraceT()) ~ opt(SemicolonT()) ^^ {
            case st ~ name ~ maybeDefs ~ _ =>
                maybeDefs match {
                    case None => State(name._1, Seq.empty).setLoc(st)
                    case Some (_ ~ defs ~ _)  => State(name._1, defs).setLoc(st)
                }
        }
    }

    // maybe we can check here that the constructor has the appropriate name?
    private def parseConstructor = {
        parseId ~ opt(AtT() ~! parseIdAlternatives) ~! LParenT() ~! parseArgDefList ~! RParenT() ~! LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case name ~ permission ~ _ ~ args ~ _ ~ _ ~ body ~ _ =>
                val resultType = extractTypeFromPermission(permission, name._1, isRemote = false)

                Constructor(name._1, args, resultType, body).setLoc(name)
        }
    }

    private def parseDeclInState: Parser[Declaration] = {
        parseFieldDecl |
        parseStateDecl | parseConstructor | parseContractDecl | failure("declaration expected")
    }

    private def parseDeclInContract(isInterface:Boolean)(contractName: String):  Parser[Declaration] = {

        parseDeclInState | parseTransDecl(isInterface)(contractName)
    }

    private def parseContractModifier = {
        val mainP: Parser[ContractModifier] = MainT() ^^ (t => IsMain().setLoc(t))
        val resourceP: Parser[ContractModifier] = ResourceT() ^^ (t => IsResource().setLoc(t))
        mainP | resourceP
    }

    private def parseContractDecl = {
        rep(parseContractModifier) ~ (ContractT() | InterfaceT()) ~! parseId >> {
            case mod ~ ct ~ name =>
                val isInterface = ct == InterfaceT()

                LBraceT() ~! rep(parseDeclInContract(isInterface)(name._1)) ~! RBraceT() ^^ {
                case _ ~ defs ~ _ =>
                    Contract(mod.toSet, name._1, defs, isInterface).setLoc(ct)
            }
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
            case Failure(msg , _) => Left(s"PARSER FAILURE: $msg")
            case Error(msg , next) =>
                if (next.atEnd) {
                    Left(s"Parser Error: $msg at end of file")
                }
                else {
                    val line = next.first.pos.line
                    val col = next.first.pos.column
                    Left(s"Error: $msg at $line:$col")
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

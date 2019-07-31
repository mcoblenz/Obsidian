package edu.cmu.cs.obsidian.parser

import java.io.{InputStream, StringWriter}

import edu.cmu.cs.obsidian.{lexer, parser}
import edu.cmu.cs.obsidian.lexer._
import edu.cmu.cs.obsidian.typecheck._
import org.apache.commons.io.IOUtils

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



    private def parseDotPath: Parser[Seq[Identifier]] = DotT() ~ parseId ~ opt(parseDotPath) ^^ {
        case _ ~ ident ~ rest => {
            rest match {
                case Some(path) => List(ident) ++ path
                case None => List(ident)
            }
        }
    }

    private def parsePath: Parser[Seq[Identifier]] = parseId ~ opt(parseDotPath) ^^{
      case ident ~ rest => {
        rest match {
          case Some(path) => List(ident) ++ path
          case None => List(ident)
        }
      }
    }

    private def parseNonPrimitive: Parser[NonPrimitiveType] = {
        opt(RemoteT()) ~ parseId ~ opt(LBracketT() ~ repsep(parseType, CommaT()) ~ RBracketT()) ~ opt(AtT() ~! parseIdAlternatives) ^^ {
            case remote ~ id ~ genericParams ~ permissionToken => {
                val isRemote = remote.isDefined
                val actualGenerics = genericParams.map {
                    case _ ~ params ~ _ => params
                }.getOrElse(List())

                val typ = extractTypeFromPermission(permissionToken, id._1, actualGenerics, isRemote, defaultOwned = false)
                typ.setLoc(id)
            }
        }
    }

    private def parseType: Parser[ObsidianType] = {
        val intPrim = IntT() ^^ { t => IntType().setLoc(t) }
        val boolPrim = BoolT() ^^ { t => BoolType().setLoc(t) }
        val stringPrim = StringT() ^^ { t => StringType().setLoc(t) }

        parseNonPrimitive | intPrim | boolPrim | stringPrim
    }

    private def extractTypeFromPermission(permission: Option[~[Token, Seq[Identifier]]],
                                          name: String,
                                          genericParams: Seq[ObsidianType],
                                          isRemote: Boolean,
                                          defaultOwned: Boolean): NonPrimitiveType = {
        val defaultPermission = if (defaultOwned) Owned() else Inferred()

        val contractType = ContractType(name, genericParams)

        permission match {
            case None => ContractReferenceType(contractType, defaultPermission, isRemote)
            case Some(_ ~ permissionIdentSeq) =>
                if (permissionIdentSeq.size == 1) {
                    val thePermissionOrState = permissionIdentSeq.head
                    val permission = resolvePermission(thePermissionOrState._1)
                    permission match {
                        case None => StateType(contractType, thePermissionOrState._1, isRemote)
                        case Some(p) => ContractReferenceType(contractType, p, isRemote)
                    }
                } else {
                    val stateNames = permissionIdentSeq.map(_._1)
                    StateType(contractType, stateNames.toSet, isRemote)
                }
        }
    }

    private def parseArgList: Parser[Seq[Expression]] = repsep(parseExpr, CommaT())

    private def parseArgDefList(contractName: String): Parser[Seq[VariableDeclWithSpec]] = {
        val processOneArg = new PartialFunction[(ObsidianType, ObsidianType) ~ Identifier, VariableDeclWithSpec] {
            def apply(x: (ObsidianType, ObsidianType) ~ Identifier): VariableDeclWithSpec = {
                x match {
                    case (typIn, typOut) ~ name => VariableDeclWithSpec(typIn, typOut, name._1).setLoc(name)
                }
            }
            def isDefinedAt(x: (ObsidianType, ObsidianType) ~ Identifier): Boolean = {
                x match {
                    case ((typIn, typOut) ~ name) =>
                        if (name._1 == "this") {
                            typIn match {
                                case n: NonPrimitiveType => n.contractName == contractName
                                case _ => false
                            }
                        }
                        else {
                            true
                        }

                    case _ => false
                }
            }
        }

        val oneDecl = (parseArgumentSpec ~ parseId) ^? (processOneArg, (_ => "error message"))

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
                                    case ContractReferenceType(ct, Inferred(), isRemote) => ContractReferenceType(ct, Unowned(), isRemote)
                                    case _ => t
                                }
                                (correctedType, correctedType)
                            case Some(_ ~ idSeq) => {
                                val typOut = extractTypeFromPermission(permission, t.contractName, t.genericParams, t.isRemote, false)
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
        rep(parseAtomicStatement) ^^ (statements => statements)

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
            case arrow ~ name ~ updates ~ _ => Transition(name._1, updates, Inferred()).setLoc(arrow)
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

        val parseRevert = RevertT() ~! opt(parseExpr) ~! SemicolonT() ^^ {
            case t ~ expr ~ _ => Revert(expr).setLoc(t)
        }



        val parseOnlyIf = IfT() ~! opt(LParenT()) ~ parseExpr ~! opt(InT() ~! parseId) ~ opt(RParenT()) ~! LBraceT() ~! parseBody ~! RBraceT()
        val parseElse = ElseT() ~! LBraceT() ~! parseBody ~! RBraceT()

        val parseIf = parseOnlyIf ~ opt(parseElse) ^^ {
            case _if ~ _ ~ e ~ stOpt ~ _ ~ _ ~ s ~ _ ~ None =>
                stOpt match {
                    case None => If(e, s).setLoc(_if)
                    case Some(_ ~ stateName) => IfInState(e, Inferred(), States(Set(stateName._1)), s, Seq.empty).setLoc(_if)
                }

            case _if ~ _ ~ e ~ stOpt ~ _ ~ _ ~ s1 ~ _ ~ Some(_ ~ _ ~ s2 ~ _) =>
                stOpt match {
                    case None => IfThenElse(e, s1, s2).setLoc(_if)
                    case Some(_ ~ stateName) => IfInState(e, Inferred(), States(Set(stateName._1)), s1, s2).setLoc(_if)
                }

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

        val parseTypeState = parseIdAlternatives ^^ {
            idents =>
                if (idents.size == 1) {
                    resolvePermission(idents.head._1) match {
                        case Some(perm) => perm
                        case None => States(idents.map(_._1).toSet)
                    }
                } else {
                    States(idents.map(_._1).toSet)
                }
        }

        val parseStaticAssertion = LBracketT() ~! parseExpr ~ AtT() ~ parseTypeState ~ RBracketT() ~! SemicolonT() ^^ {
            case _ ~ expr ~ at ~ typestate ~ _ ~ _ =>
                StaticAssert(expr, typestate).setLoc(at)
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

        parseReturn | parseTransition | parseRevert |
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
    private def parseDivision = parseBinary(ForwardSlashT(), Divide.apply, parseMod)
    private def parseMod = parseBinary(PercentT(), Mod.apply, parseNot)
    private def parseNot = parseUnary(NotT(), LogicalNegation.apply, parseUnaryMinus)
    private def parseUnaryMinus = parseUnary(MinusT(), Negate.apply, parseExprBottom)


    private val parseStringLiteral: Parser[StringLiteral] = {
        val partialFunction: PartialFunction[Token, StringLiteral] = {
            case t@StringLiteralT(s) => StringLiteral(s).setLoc(t)
        }

        accept("string literal", partialFunction)
    }

    private def parseTypeList: Parser[Seq[ObsidianType]] =
        opt(LBracketT() ~ repsep(parseType, CommaT()) ~ RBracketT()) ^^ {
            case Some(_ ~ params ~ _) => params
            case None => List()
        }

    private def parseExprBottom: Parser[Expression] = {
        def parseLocalInv = {
            parseId ~ parseTypeList ~ LParenT() ~ parseArgList ~ RParenT() ^^ {
                // genericParams will be filled in later by the typechecker
                case name ~ params ~ _ ~ args ~ _ => LocalInvocation(name._1, Nil, params, args).setLoc(name)
            }
        }

        /* avoids left recursion by parsing from the dot, e.g. ".f(a)", not "x.f(a)" */
        type DotExpr = Either[Identifier, (Identifier, Seq[ObsidianType], Seq[Expression])]

        def foldDotExpr(e: Expression, dots: Seq[DotExpr]): Expression = {
            dots.foldLeft(e)(
                (e: Expression, inv: DotExpr) => inv match {
                    case Left(fieldName) => Dereference(e, fieldName._1).setLoc(fieldName)
                    // genericParams will be filled in later by the typechecker
                    case Right((funcName, params, args)) => Invocation(e, Nil, params, funcName._1, args, false).setLoc(funcName)
                }
            )
        }

        def parseDots: Parser[Expression => Expression] = {
            val parseOne: Parser[DotExpr] = DotT() ~! parseId ~
                parseTypeList ~ opt(LParenT() ~ parseArgList ~ RParenT()) ^^ {
                case _ ~ name ~ params ~ Some(_ ~ args ~ _) => Right((name, params, args))
                case _ ~ name ~ params ~ None => Left(name)
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
            NewT() ~! parseId ~! opt(LBracketT() ~ repsep(parseType, CommaT()) ~ RBracketT()) ~ LParenT() ~! parseArgList ~! RParenT() ^^ {
                case _new ~ name ~ typeParamsOpt ~ _ ~ args ~ _ =>
                    val typeParams = typeParamsOpt match {
                        case Some(_ ~ params ~ _) => params
                        case None => Nil
                    }

                    Construction(ContractType(name._1, typeParams), args, false).setLoc(_new)
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

    private def parseFieldDecl: Parser[Field] = {
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

    case class TransactionOptions (isStatic: Boolean, isPrivate: Boolean)

    private def parseTransactionOptions: Parser[TransactionOptions] = {
        rep(StaticT() | PrivateT()) ^^ {
            case opts =>
                var options = TransactionOptions(false, false)

                for (opt <- opts) {
                    options =
                        opt match {
                            case StaticT() =>
                                if (options.isStatic) {
                                    return err("Duplicate static keyword in transaction declaration.")
                                }
                                else {
                                    TransactionOptions(true, options.isPrivate)
                                }
                            case PrivateT() =>
                                if (options.isPrivate) {
                                    return err("Duplicate private keyword in transaction declaration.")
                                }
                                else {
                                    TransactionOptions(options.isStatic, true)
                                }
                            case _ =>
                                return err(s"Unexpected option found: $opt")
                        }
                }
                options
        }
    }


    private def parseTransDecl(params: List[GenericType], isInterface:Boolean)(contractName: String, contractParams: Seq[GenericType]): Parser[Transaction] = {
        parseTransactionOptions ~ opt(LParenT() ~! parseArgDefList(contractName) ~! RParenT()) ~ TransactionT() ~! (parseId | MainT()) ~!
            opt(LBracketT() ~ repsep(genericParam, CommaT()) ~ RBracketT()) ~! LParenT() ~! parseArgDefList(contractName) ~! RParenT() ~!
            opt(parseReturns) ~! rep(parseEnsures) ~!  parseTransBody(isInterface) ^^ {
            case opts ~ privateMethodFieldTypes ~ t ~ name ~ paramsOpt ~ _ ~ args ~ _ ~ returns ~ ensures ~ body =>
                val nameString = name match {
                    case MainT() => "main"
                    case id => id.asInstanceOf[Identifier]._1
                }

                val params = paramsOpt match {
                    case Some(_ ~ ps ~ _) => ps
                    case None => Nil
                }

                val (thisArg, filteredArgs) = args.headOption match {
                    case None => (None, args)
                    case Some(v) =>
                        if (v.varName == "this") {
                            (Some(v), args.tail)
                        } else {
                            (None, args)
                        }
                }

                val thisContractType = ContractType(contractName, contractParams)

                val finalType = thisArg match {
                    case None => ContractReferenceType(thisContractType, Shared(), false)
                    case Some(v) => v.typOut.withParams(contractParams)
                }

                val thisType = thisArg match {
                    case None => ContractReferenceType(ContractType(contractName, contractParams), Shared(), false)
                    case Some(variableDecl) => variableDecl.typIn.asInstanceOf[NonPrimitiveType].withParams(contractParams)
                }

                val initialFieldTypes = privateMethodFieldTypes match {
                    case None => Map.empty
                    case Some(_ ~ argDefList ~ _) => argDefList.map((v: VariableDeclWithSpec) => (v.varName, v.typIn))
                }

                val finalFieldTypes = privateMethodFieldTypes match {
                    case None => Map.empty
                    case Some(_ ~ argDefList ~ _) => argDefList.map((v: VariableDeclWithSpec) => (v.varName, v.typOut))
                }

                Transaction(nameString, params, filteredArgs, returns,
                    ensures, body, opts.isStatic, opts.isPrivate,
                    thisType, finalType.asInstanceOf[NonPrimitiveType],
                    initialFieldTypes.toMap, finalFieldTypes.toMap).setLoc(t)
        }
    }

    private def parseStateDecl = {
        opt(AssetT()) ~ StateT() ~! parseId ~! opt(LBraceT() ~! rep(parseFieldDecl) ~! RBraceT()) ~ opt(SemicolonT()) ^^ {
            case isAsset ~ st ~ name ~ maybeDefs ~ _ =>
                maybeDefs match {
                    case None => State(name._1, Seq.empty, isAsset.isDefined).setLoc(st)
                    case Some (_ ~ defs ~ _)  => State(name._1, defs, isAsset.isDefined).setLoc(st)
                }
        }
    }

    // TODO: maybe we can check here that the constructor has the appropriate name?
    private def parseConstructor (params: Seq[GenericType]) = {
        parseId ~ opt(AtT() ~! parseIdAlternatives) ~! LParenT() ~! parseArgDefList("") ~! RParenT() ~! LBraceT() ~! parseBody ~! RBraceT() ^^ {
            case name ~ permission ~ _ ~ args ~ _ ~ _ ~ body ~ _ =>
                val resultType = extractTypeFromPermission(permission, name._1, params, isRemote = false, defaultOwned = false)
                Constructor(name._1, args, resultType, body).setLoc(name)
        }
    }

    private def parseDeclInContract(params: List[GenericType], isInterface:Boolean,
                                    sourcePath: String)(contractName: String, contractParams: List[GenericType]):  Parser[Declaration] = {
        parseFieldDecl | parseStateDecl | parseConstructor(contractParams) | parseContractDecl(sourcePath) |
            parseTransDecl(params, isInterface)(contractName, contractParams)
    }

    private def parseContractModifier = {
        val mainP: Parser[ContractModifier] = MainT() ^^ (t => IsMain().setLoc(t))
        val resourceP: Parser[ContractModifier] = AssetT() ^^ (t => IsAsset().setLoc(t))
        mainP | resourceP
    }

    private def parseFSMEdgeLine: Parser[Seq[FSMEdge]] = {
        parseId ~ RightArrowT() ~ repsep(parseId, CommaT()) ~ SemicolonT() ^^ {
            case fromState ~ _ ~ toStates ~ _ =>
                toStates.map(toState => FSMEdge(fromState, toState))
        }
    }

    private def parseTransitions: Parser[Transitions] = {
        TransitionsT() ~! LBraceT() ~ rep(parseFSMEdgeLine) ~ RBraceT() ^^ {
            case transitions ~ _ ~ edges ~ _ =>
                val allEdges = edges.reduce((e1, e2) => e1 ++ e2)
                Transitions(allEdges).setLoc(transitions)
        }
    }

    private def parseContractType = {
        parseId ~ opt(LBracketT() ~ repsep(parseType, CommaT()) ~ RBracketT()) ^^ {
            case name ~ optParams =>
                val params = optParams.map(_._1._2).getOrElse(List())
                ContractType(name._1, params)
        }
    }

    private def parseWhereImplements: Parser[Option[ContractType]] = {
        WhereT() ~ parseId ~ ImplementsT() ~ parseType ^^ {
            case _ ~ typeVar ~ _ ~ typeBound => typeBound match {
                case np: NonPrimitiveType => Some(np.contractType)
                case _ => failure("Contract cannot implement primitive type."); None
            }
        }
    }

    private def parseWhereSubperm: Parser[Either[Permission, Set[String]]] = {
        WhereT() ~ parseId ~ IsT() ~ parseIdAlternatives ^^ {
            case _ ~ permVar ~ _ ~ permBound =>
                if (permBound.length == 1) {
                    resolvePermission(permBound.head._1) match {
                        case Some(perm) => Left(perm)
                        case None => Right(permBound.map(_._1).toSet)
                    }
                } else {
                    Right(permBound.map(_._1).toSet)
                }
        }
    }

    private def genericParam = {
        opt(AssetT()) ~ parseId ~ opt(AtT() ~ parseId) ~ opt(parseWhereImplements) ~ opt(parseWhereSubperm) ^^ {
            case assetMod ~ varId ~ stateVarId ~ implements ~ subpermBound =>
                val stateVar = stateVarId.map(_._2._1)

                val definesInterface = implements.flatten.isDefined
                val contractBound = implements.flatten.getOrElse(ContractType("Top", Nil))
                val gVar = GenericVar(assetMod.isDefined, varId._1, stateVar)

                val bound = subpermBound match {
                    case Some(Left(perm)) => GenericBoundPerm(definesInterface, permSpecified = true, contractBound, perm)
                    case Some(Right(states)) => GenericBoundStates(definesInterface, permSpecified = true, contractBound, states)
                    case None => GenericBoundPerm(definesInterface, permSpecified = false, contractBound, Unowned())
                }

                GenericType(gVar, bound)
        }
    }

    private def parseGenericParams = {
        opt(RemoteT()) ~ parseId ~ LBracketT() ~ repsep(parseType, CommaT()) ~ RBracketT() ~ opt(AtT() ~ parseIdAlternatives) ^^ {
            case isRemote ~ name ~ _ ~ genParams ~ _ ~ permission =>
                extractTypeFromPermission(permission, name._1, genParams, isRemote.isDefined, defaultOwned = false)
        }
    }

    private def parseGenericId = {
        parseId ~! opt(LBracketT() ~ repsep(genericParam, CommaT()) ~ RBracketT()) ^^ {
            case id ~ optParams =>
                optParams match {
                    case Some(_ ~ params ~ _) =>  (id, params)
                    case None => (id, List())
                }
        }
    }

    private def parseContractDecl(srcPath: String) = {
        rep(parseContractModifier) ~ (ContractT() | InterfaceT()) ~! parseGenericId ~ opt(ImplementsT() ~ parseContractType) >> {
            case mod ~ ct ~ identifier ~ impl =>
                val isInterface = ct == InterfaceT()

                val (name, params) = identifier

                val implementBound = impl match {
                    case Some(_ ~ bound) => bound
                    case None => ContractType("Top", Nil)
                }

                LBraceT() ~! rep(parseTransitions | parseDeclInContract(params, isInterface, srcPath)(name._1, params)) ~! RBraceT() ^^ {
                case _ ~ contents ~ _ =>
                    // Make sure there's only one transition diagram.
                    val separateTransitions = contents.filter({
                        case t: Transitions => true
                        case _ => false
                    }).map(_.asInstanceOf[Transitions])

                    // For now, combine all the state transitions from all the different
                    // transitions blocks.
                    // Maybe eventually require that there is only one transitions block.
                    val transitionsEdges = separateTransitions.foldLeft(List.empty[FSMEdge])((edges, t) => edges ++ t.edges)

                    val transitions =
                        if (transitionsEdges.isEmpty) {
                            None
                        } else {
                            Some(Transitions(transitionsEdges))
                        }

                    val decls = contents.filter({
                        case d: Declaration => true
                        case _ => false
                    }).map(_.asInstanceOf[Declaration])

                    ObsidianContractImpl(mod.toSet, name._1, params, implementBound, decls, transitions, isInterface, srcPath).setLoc(ct)
            }
        }
    }

    private def parseImport = {
        ImportT() ~! parseStringLiteral ^^ {
            case _import ~ StringLiteral(name) => Import(name).setLoc(_import)
        }
    }

    private def parseFFIContractImpl(srcPath: String) = {
        /* contract IOImpl implements IO with java: edu.cmu.cs.obsidian.stdlib.IO */
        ContractT() ~! parseId ~! ImplementsT() ~! parseId ~! WithT() ~! parseId ~! ColonT() ~! parsePath ~! SemicolonT() ^^ {
            case _ ~ implName ~ _ ~ interfaceName ~ _ ~ _ ~ _ ~ javaSource ~ _ =>
                JavaFFIContractImpl(implName._1, interfaceName._1, javaSource, srcPath).setLoc(implName)
        }
    }


    private def parseProgram(srcPath: String) = {
        phrase(rep(parseImport) ~ rep1(parseContractDecl(srcPath) | parseFFIContractImpl(srcPath))) ^^ {
            case imports ~ contracts => Program(imports, contracts).setLoc(contracts.head)
        }
    }

    def parseProgram(tokens: Seq[Token], srcPath: String): Either[String, Program] = {
        val reader = new TokenReader(tokens)
        parseProgram(srcPath)(reader) match {
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

    def parseFileAtPath(srcPath: String, input: InputStream, printTokens: Boolean): Program = {
        val src = try {
            val writer = new StringWriter()
            IOUtils.copy(input, writer)
            writer.toString
        } finally {
            input.close()
        }

        val tokens: Seq[Token] = Lexer.tokenize(src) match {
            case Left(msg) => throw new ParseException(msg)
            case Right(ts) => ts
        }

        if (printTokens) {
            println("Tokens:")
            println(tokens)
            println()
        }

        val ast: Program = parseProgram(tokens, srcPath) match {
            case Left(msg) => throw new ParseException(msg + " in " + srcPath)
            case Right(tree) => tree
        }
        ast
    }
}

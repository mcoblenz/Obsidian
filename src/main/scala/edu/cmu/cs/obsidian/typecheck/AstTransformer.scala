package edu.cmu.cs.obsidian.typecheck

import com.helger.jcodemodel.JPackage
import edu.cmu.cs.obsidian.codegen.{Client, Server}
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.util.Util

import scala.collection.Map
import scala.util.parsing.input.Position
import scala.collection.immutable.{HashSet, TreeMap, TreeSet}

/* The only purpose of this compilation phase at the moment is to disambiguate
 * path-types. For example, If [T] is defined as a dependent type of [C], then
 * inside of [C], the types [T] and [this.T] refer to the same thing. This
 * must be clarified. */

/* Important Note: be sure to take into account the fact that AST nodes need a location.
 * To construct a new AST node in this file, explicitly set the location using [setLoc] */

object AstTransformer {

    type FoldFunc[TFrom, TTo] = Int
    type Context = Map[String, ObsidianType]
    val emptyContext = new TreeMap[String, ObsidianType]()

    def transformProgram(table: SymbolTable): (SymbolTable, Seq[ErrorRecord]) = {
        var errorRecords = List.empty[ErrorRecord]
        var contracts = List.empty[Contract]
        assert(table.ast.imports.isEmpty, "Imports should be empty after processing.")


        for ((contractName, contractTable) <- table.contractLookup) {
            val (newContract, errors) = transformContract(table, contractTable)
            errorRecords = errorRecords ++ errors
            contracts = contracts :+ newContract
        }

        val newProgram = Program(Seq.empty, contracts).setLoc(table.ast)

        val newTable = new SymbolTable(newProgram)
        (newTable, errorRecords)
    }

    def transformContract(table: SymbolTable, cTable: ContractTable): (Contract, Seq[ErrorRecord]) = {
        var newDecls: Seq[Declaration] = Nil
        var errors = List.empty[ErrorRecord]
        for (d <- cTable.contract.declarations) {
            val newDecl: Declaration = d.tag match {
                case TransactionDeclTag =>
                    val (newTransaction, newErrors) = transformTransaction(table, cTable, d.asInstanceOf[Transaction])
                    errors = errors ++ newErrors
                    newTransaction
                case FuncDeclTag =>
                    val (newFunction, newErrors) = transformFunc(table, cTable, d.asInstanceOf[Func])
                    errors = errors ++ newErrors
                    newFunction
                case ConstructorDeclTag =>
                    val (newConstructor, newErrors) = transformConstructor(table, cTable, d.asInstanceOf[Constructor])
                    errors = errors ++ newErrors
                    newConstructor
                case FieldDeclTag =>
                    val (newField, newErrors) = transformField(table, cTable, d.asInstanceOf[Field])
                    errors = errors ++ newErrors
                    newField
                case StateDeclTag =>
                    val stateTable = cTable.state(d.asInstanceOf[State].name).get
                    val (newState, newErrors) = transformState(table, stateTable)
                    errors = errors ++ newErrors
                    newState
                case ContractDeclTag =>
                    val contractTable = cTable.childContract(d.asInstanceOf[Contract].name).get
                    val (newContract, newErrors) = transformContract(table, contractTable)
                    errors = errors ++ newErrors
                    newContract
                case TypeDeclTag => null
            }
            newDecls = newDecl +: newDecls
        }

        newDecls = newDecls.reverse

        val newContract = Contract(cTable.contract.modifiers, cTable.contract.name, newDecls, cTable.contract.isInterface).setLoc(cTable.contract)

        (newContract, errors.reverse)
    }

    def transformState(table: SymbolTable, sTable: StateTable): (State, Seq[ErrorRecord]) = {
        var newDecls: Seq[Declaration] = Nil
        var errors = List.empty[ErrorRecord]

        for (d <- sTable.ast.declarations) {
            val (newDecl, newErrors) = d.tag match {
                case TransactionDeclTag =>
                    transformTransaction(table, sTable, d.asInstanceOf[Transaction])
                case FuncDeclTag =>
                    transformFunc(table, sTable, d.asInstanceOf[Func])
                case ConstructorDeclTag =>
                    transformConstructor(table, sTable, d.asInstanceOf[Constructor])
                case FieldDeclTag =>
                    transformField(table, sTable, d.asInstanceOf[Field])
                case StateDeclTag => null
                case ContractDeclTag => null
                case TypeDeclTag => null
            }
            newDecls = newDecl +: newDecls
            errors = errors ++ newErrors
        }

        newDecls = newDecls.reverse

        val newState = sTable.ast.copy(declarations = newDecls)
        (newState, errors.reverse)
    }

    def transformField(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            f: Field): (Field, Seq[ErrorRecord]) = {
        val context = startContext(lexicallyInsideOf, List.empty, Owned()) // Permission of this is irrelevant when transforming fields
        val (newType, errors) = transformType(table, lexicallyInsideOf, context, f.typ, f.loc)
        (f.copy(typ = newType).setLoc(f), errors)
    }

    def transformExpression(e: Expression): Expression = {
        e match {
            case v: ReferenceIdentifier => v
            case n: NumLiteral => n
            case s: StringLiteral => s
            case t: TrueLiteral => TrueLiteral().setLoc(t)
            case f: FalseLiteral => FalseLiteral().setLoc(f)
            case t: This => This().setLoc(t)
            case p: Parent => Parent().setLoc(p)
            case c: Conjunction =>
                Conjunction(transformExpression(c.e1), transformExpression(c.e2)).setLoc(c)
            case d: Disjunction =>
                Disjunction(transformExpression(d.e1), transformExpression(d.e2)).setLoc(d)
            case n: LogicalNegation =>
                LogicalNegation(transformExpression(n.e)).setLoc(n)
            case a: Add =>
                Add(transformExpression(a.e1), transformExpression(a.e2)).setLoc(a)
            case s: Subtract =>
                Subtract(transformExpression(s.e1), transformExpression(s.e2)).setLoc(s)
            case d: Divide =>
                Divide(transformExpression(d.e1), transformExpression(d.e2)).setLoc(d)
            case m: Multiply =>
                Multiply(transformExpression(m.e1), transformExpression(m.e2)).setLoc(m)
            case eq: Equals =>
                Equals(transformExpression(eq.e1), transformExpression(eq.e2)).setLoc(eq)
            case g: GreaterThan =>
                GreaterThan(transformExpression(g.e1), transformExpression(g.e2)).setLoc(g)
            case g: GreaterThanOrEquals =>
                GreaterThanOrEquals(transformExpression(g.e1), transformExpression(g.e2)).setLoc(g)
            case l: LessThan =>
                LessThan(transformExpression(l.e1), transformExpression(l.e2)).setLoc(l)
            case l: LessThanOrEquals =>
                LessThanOrEquals(transformExpression(l.e1), transformExpression(l.e2)).setLoc(l)
            case ne: NotEquals =>
                NotEquals(transformExpression(ne.e1), transformExpression(ne.e2)).setLoc(ne)
            case d: Dereference => d.copy(e = transformExpression(d.e)).setLoc(d)
            case i: LocalInvocation =>
                i.copy(args = i.args.map(eArg => transformExpression(eArg))).setLoc(i)
            case i: Invocation =>
                i.copy(recipient = transformExpression(i.recipient), args = i.args.map(eArg => transformExpression(eArg))).setLoc(i)
            case c: Construction =>
                c.copy(args = c.args.map(eArg => transformExpression(eArg))).setLoc(c)
            case d@Disown(e) =>
                Disown(transformExpression(e)).setLoc(d)
            case t@OwnershipTransfer(e) =>
                OwnershipTransfer(transformExpression(e)).setLoc(t)
            case s: StateInitializer => s.copy().setLoc(s)

        }
    }

    def startContext(lexicallyInsideOf: DeclarationTable, args: Seq[VariableDecl], thisPermission: Permission): Context = {
        var startContext = emptyContext

        val simpleType =  ContractReferenceType(lexicallyInsideOf.contractType, thisPermission)
        val contractType = UnresolvedNonprimitiveType(List("this"), Set(), thisPermission)
        startContext = startContext.updated("this", contractType)

        for (a <- args) {
            startContext = startContext.updated(a.varName, a.typ)
        }
        startContext
    }

    def transformArgs(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            args: Seq[VariableDecl],
            thisPermission: Permission): (Seq[VariableDecl], Seq[ErrorRecord]) = {
        var errors = List.empty[ErrorRecord]
        var newArgs: Seq[VariableDecl] = Nil
        val context = startContext(lexicallyInsideOf, args, thisPermission)
        for (a <- args) {
            val (transformedType, newErrors) = transformType(table, lexicallyInsideOf, context - a.varName, a.typ, a.loc)
            errors = errors ++ newErrors

            val aNew = a.copy(typ = transformedType)
            newArgs = aNew +: newArgs
        }
        (newArgs.reverse, errors)
    }

    def transformTransaction(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            t: Transaction): (Transaction, Seq[ErrorRecord]) = {
        val context = startContext(lexicallyInsideOf, t.args, t.thisPermission)


        var (newArgs, argErrors) = transformArgs(table, lexicallyInsideOf, t.args, t.thisPermission)

        val newEnsures = t.ensures.map(en => en.copy(expr = transformExpression(en.expr)))

        val (newRetType, retTypeErrors) = t.retType match {
            case None => (None, List.empty)
            case Some(retType) =>
                val (transformedType, errs) = transformType(table, lexicallyInsideOf, context, retType, t.loc)
                (Some(transformedType), errs)
        }
        val (newTransactionBody, _, bodyErrors) =  transformBody(table, lexicallyInsideOf, context, t.body)
        val newTransaction = t.copy(retType = newRetType, args = newArgs, ensures = newEnsures,
            body = newTransactionBody).setLoc(t)
        (newTransaction, retTypeErrors ++ argErrors ++ bodyErrors)
    }

    def transformConstructor(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            c: Constructor): (Constructor, Seq[ErrorRecord]) = {

        // Constructors always own "this".
        val (newArgs, argsTransformErrors) = transformArgs(table, lexicallyInsideOf, c.args, Owned())
        val context = startContext(lexicallyInsideOf, c.args, Owned())
        var (newBody, _, bodyTransformErrors) = transformBody(table, lexicallyInsideOf, context, c.body)
        val errors = argsTransformErrors ++ bodyTransformErrors

        (c.copy(args = newArgs, body = newBody).setLoc(c), errors)
    }

    def transformFunc(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            f: Func): (Func, Seq[ErrorRecord]) = {

        val (newArgs, argTransformErrors) = transformArgs(table, lexicallyInsideOf, f.args, f.thisPermission)
        val context = startContext(lexicallyInsideOf, f.args, f.thisPermission)
        val newRetType = f.retType.map(transformType(table, lexicallyInsideOf, context, _, f.loc))
        var errors = List.empty[ErrorRecord]
        val (transformedRetType, retTypeErrors) = newRetType match {
            case None => (None, List.empty)
            case Some(p) => (Some(p._1), p._2)
        }

        val (transformedBody, _, bodyTransformErrors) = transformBody(table, lexicallyInsideOf, context, f.body)
        val transformedF = f.copy(retType = transformedRetType, args = newArgs,
               body = transformedBody)

        errors = retTypeErrors ++ argTransformErrors ++ bodyTransformErrors

        (transformedF, errors)

    }

    def transformBody(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            inScope: Context,
            b: Seq[Statement]): (Seq[Statement], Context, Seq[ErrorRecord]) = {
        b match {
            case Seq() => (Seq(), inScope, Seq())
            case s +: rest =>
                val (sNew, inScopeNew, errors) = transformStatement(table, lexicallyInsideOf, inScope, s)
                val (restStatements, finalContext, restErrors) = transformBody(table, lexicallyInsideOf, inScopeNew, rest)
                (sNew +: restStatements, finalContext, errors ++ restErrors)
        }
    }

    def transformStatement(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            context: Context,
            s: Statement): (Statement, Context, Seq[ErrorRecord]) = {
        s match {
            case oldDecl@VariableDecl(typ, varName) =>
                val newTypChoice = transformType(table, lexicallyInsideOf, context, typ, s.loc)
                val (newTyp, errors) = transformType(table, lexicallyInsideOf, context, typ, s.loc)
                (oldDecl.copy(typ = newTyp).setLoc(oldDecl), context.updated(varName, newTyp), errors)
            case oldDecl@VariableDeclWithInit(typ, varName, e) =>
                val (newTyp, errors) = transformType(table, lexicallyInsideOf, context, typ, s.loc)
                val newDecl = oldDecl.copy(typ = newTyp, e = transformExpression(e)).setLoc(oldDecl)
                (newDecl, context.updated(varName, newTyp), errors)
            case r@Return() => (r, context, Seq())
            case r@ReturnExpr(e) => (ReturnExpr(transformExpression(e)).setLoc(r), context, Seq())
            case t@Transition(newStateName, updates) =>
                updates match {
                    case None => (Transition(newStateName, updates).setLoc(t), context, Seq())
                    case Some(u) =>
                        val mapFun = (p: (ReferenceIdentifier, Expression)) => (p._1, transformExpression(p._2))
                        val transformedUpdates = u.map(mapFun)

                        (Transition(newStateName, Some(transformedUpdates)).setLoc(t), context, Seq())
                }
            case a@Assignment(assignTo, e, transfersOwnership) =>
                (Assignment(transformExpression(assignTo), transformExpression(e), transfersOwnership).setLoc(a), context, Seq())
            case t@Throw() => (t, context, Seq())
            case oldIf@If(eCond, sIf) =>
                val (sIfNew, newContext, errors) = transformBody(table, lexicallyInsideOf, context, sIf)
                val newIf = oldIf.copy(s = sIfNew, eCond = transformExpression(eCond)).setLoc(oldIf)
                (newIf, newContext, errors)
            case oldIf@IfThenElse(eCond, s1, s2) =>
                val (s1New, newContext1, errors1) = transformBody(table, lexicallyInsideOf, context, s1)
                val (s2New, newContext2, errors2) = transformBody(table, lexicallyInsideOf, context, s2)
                val newIf = oldIf.copy(
                    s1 = s1New,
                    s2 = s2New,
                    eCond = transformExpression(eCond)
                ).setLoc(oldIf)
                (newIf, context, errors1 ++ errors2)
            case oldTry@TryCatch(s1, s2) =>
                val (s1New, newContext1, errors1) = transformBody(table, lexicallyInsideOf, context, s1)
                val (s2New, newContext2, errors2) = transformBody(table, lexicallyInsideOf, context, s2)
                val newIf = oldTry.copy(s1 = s1New, s2 = s2New).setLoc(oldTry)
                (newIf, context, errors1 ++ errors2)
            case oldSwitch@Switch(e, cases) =>
                var errors = List.empty[ErrorRecord]
                val newCases = cases.map(_case => {
                    val (newBody, newContext, newErrors) = transformBody(table, lexicallyInsideOf, context, _case.body)
                    errors = errors ++ newErrors;
                    _case.copy(body = newBody).setLoc(oldSwitch)
                })
                val newSwitch = oldSwitch.copy(e = transformExpression(e),
                                               cases = newCases).setLoc(oldSwitch)
                (newSwitch, context, errors)
            case e: Expression => (transformExpression(e), context, Seq())

        }
    }

    def transformType(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            context: Context,
            t: ObsidianType,
            pos: Position): (ObsidianType, List[ErrorRecord]) = {

        // We should only be transforming potentially-unresolved types, but we can't specify that statically because ASTs are used for resolved types too.

        t match {
            case t@BoolType() => (t, List.empty[ErrorRecord])
            case t@IntType() => (t, List.empty[ErrorRecord])
            case t@StringType() => (t, List.empty[ErrorRecord])
            case nonPrim@UnresolvedNonprimitiveType(_, _, _) =>
                //val tCanonified: UnresolvedNonprimitiveType = canonifyParsableType(table, context, nonPrim)
                val result: TraverseResult = resolveNonPrimitiveTypeContext(table, lexicallyInsideOf, nonPrim,
                                                            new TreeSet(), context, pos)

                result match {
                    case Left(err) => (BottomType().setLoc(t), List(ErrorRecord(err, pos)))
                    case Right((unpermissionedType, _)) =>
                        (unpermissionedType.setLoc(t), List.empty)
                }
            case i@InterfaceContractType(_, _) => (i, List.empty)
            case b@BottomType() => (b, List.empty)
            case np: NonPrimitiveType => (np, List.empty)
        }
    }


    def decomposeTransformResult(result: Either[Error, ObsidianType], pos: Position): (ObsidianType, List[ErrorRecord]) = {
        result match {
            case Left(err) => (BottomType(), List(ErrorRecord(err, pos)))
            case Right(typ) => (typ, List.empty[ErrorRecord])
        }
    }

    type TraverseResult = Either[Error, (NonPrimitiveType, DeclarationTable)]
//
//    private def appendToPath(
//            f: String,
//            result: TraverseResult): TraverseResult = {
//        result match {
//            case Left(_) => result
//            case Right((PathType(path, ts), table)) =>
//                Right((PathType(f +: path, ts), table))
//            case Right((ts, table)) =>
//                Right((PathType(f::Nil, ts), table))
//        }
//    }

//    private def canonifyParsableType(
//            table: SymbolTable,
//            context: Context,
//            t: UnresolvedNonprimitiveType): UnresolvedNonprimitiveType = {
//        if (t.identifiers.length == 1) {
//            val cName = t.identifiers.head
//            table.contract(cName) match {
//                case Some(ct) => t
//                case None => UnresolvedNonprimitiveType("this" +: t.identifiers, t.mods)
//            }
//        }
//        else if (t.identifiers.length == 2) {
//            if (context contains t.identifiers.head) return t
//
//            val cNamePossible = t.identifiers.head
//            val sNamePossible = t.identifiers.tail.head
//
//            // see if this interpretation of the type works
//            table.contract(cNamePossible) match {
//                case Some(ct) =>
//                    ct.state(sNamePossible) match {
//                        case Some(st) => return t
//                        case None => ()
//                    }
//                case None => ()
//            }
//
//            UnresolvedNonprimitiveType("this" +: t.identifiers, t.mods)
//
//        } else {
//            if (context contains t.identifiers.head) t
//            else UnresolvedNonprimitiveType("this" +: t.identifiers, t.mods)
//        }
//    }

    /* [resolveSimpleType] returns either an error that was reached while checking
     * (if [tr] could not be traversed), or the declaration table of the type,
     * as well as new unpermissioned type: this return value is only different from [tr]
     * if [tr] starts with an implicit "this" (in this case, "this" is added) */

    private def resolveNonPrimitiveTypeContext(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            t: UnresolvedNonprimitiveType,
            visitedLocalVars: Set[String],
            context: Context,
            pos: Position): TraverseResult = {

        if (t.identifiers.length == 1) {
            val cName = t.identifiers.head
            lexicallyInsideOf.lookupContract(cName) match {
                case Some(ct) =>
                    val tRaw = ContractReferenceType(ct.contractType, t.permission)
                    Right((tRaw, ct))
                case None => Left(ContractUndefinedError(cName))
            }
        }
        else {


            val pathHead = t.identifiers.head
            val pathRest = t.identifiers.tail

//            if (pathHead == "this") {
//                val tNew = UnresolvedNonprimitiveType(pathRest, t.mods)
//                val emptySet = new HashSet[(DeclarationTable, String)]()
//                val result =
//                    resolveNonPrimitiveTypeNoContext(table, lexicallyInsideOf, tNew, emptySet, pos)
//                appendToPath("this", result)
//                return result
//            }

            if (t.identifiers.length == 2) {
                val cNamePossible = pathHead
                val sNamePossible = pathRest.head

                // see if this interpretation of the type works
                lexicallyInsideOf.lookupContract(cNamePossible) match {
                    case Some(ct) =>
                        ct.state(sNamePossible) match {
                            case Some(st) =>
                                val tr = StateType(cNamePossible, sNamePossible)
                                return Right((tr, st))
                            case None => ()
                        }
                    case None => ()
                }
            }

            return Left(DereferenceError(t))

            // Path types are deprecated for now.
//
//            /* the head must be a variable */
//
//            if (visitedLocalVars contains pathHead) {
//                Left(RecursiveVariableTypeError(pathHead))
//            }
//            val newVisited = visitedLocalVars + pathHead
//
//            val pathHeadType: UnresolvedNonprimitiveType = context(pathHead) match {
//                case trNext: UnresolvedNonprimitiveType => trNext
//                case prim =>
//                    val (primType, errors) = transformType(table, lexicallyInsideOf, context, prim, pos)
//                     return Left(DereferenceError(t))
//            }
//
//            val newInsideOf =
//                resolveNonPrimitiveTypeContext(table, lexicallyInsideOf, pathHeadType,
//                                               newVisited, context, pos) match {
//                    case l@Left(_) => return l
//                    case Right(travData) => travData._2
//                }
//
//            val tNew = UnresolvedNonprimitiveType(pathRest, t.mods)
//
//            val emptySet = new HashSet[(DeclarationTable, String)]()
//            val result = resolveNonPrimitiveTypeNoContext(table, newInsideOf, tNew, emptySet, pos)
//
//            appendToPath(pathHead, result)
        }
    }

    private def resolveNonPrimitiveTypeNoContext(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            t: UnresolvedNonprimitiveType,
            visitedFields: Set[(DeclarationTable, String)],
            pos: Position): TraverseResult = {

        if (t.identifiers.length == 1) {
            val cName = t.identifiers.head
            lexicallyInsideOf.lookupContract(cName) match {
                case Some(ct) =>
                    val tRaw = ContractReferenceType(ct.contractType, t.permission)
                    Right((tRaw, ct))
                case None => Left(ContractUndefinedError(cName))
            }
        } else {
            val pathHead = t.identifiers.head
            val pathRest = t.identifiers.tail

//            if (pathHead == "parent") {
//                if (lexicallyInsideOf.contractTable.hasParent) {
//                    val tNew = t.copy(identifiers = pathRest)
//                    val newInsideOf = lexicallyInsideOf.contractTable.parent.get
//                    val result = resolveNonPrimitiveTypeNoContext(table, newInsideOf, tNew, visitedFields, pos)
//                    appendToPath("parent", result)
//                } else {
//                    Left(NoParentError(lexicallyInsideOf.contract.name))
//                }
//            }

            if (t.identifiers.length == 2) {
                val cNamePossible = pathHead
                val sNamePossible = pathRest.head

                // see if this interpretation of the type works
                lexicallyInsideOf.lookupContract(cNamePossible) match {
                    case Some(ct) =>
                        ct.state(sNamePossible) match {
                            case Some(st) =>
                                val tr = StateType(cNamePossible, sNamePossible)
                                return Right((tr, st))
                            case None => ()
                        }
                    case None => ()
                }
            }

            /* the head must be a field */

            // Path types are deprecated for now.
            Left(DereferenceError(t))

//            val fieldLookup = lexicallyInsideOf.lookupField(pathHead)
//            if (fieldLookup.isEmpty) {
//                return Left(FieldUndefinedError(lexicallyInsideOf.simpleType, pathHead))
//            }
//
//            val field = fieldLookup.get
//
//            // paths must consist entirely of [const] fields
//            if (!field.isConst) {
//                return Left(FieldNotConstError(lexicallyInsideOf.name, pathHead))
//            }
//
//            if (visitedFields contains (lexicallyInsideOf, pathHead)) {
//                return Left(RecursiveFieldTypeError(lexicallyInsideOf.name, pathHead))
//            }
//
//            val nonPrim = field.typ match {
//                case tNonPrim: UnresolvedNonprimitiveType => tNonPrim
//                case prim =>
//                    val (tRes, errors) = transformType(table, lexicallyInsideOf, new TreeMap(), prim, pos)
//                    return Left(DereferenceError(t))
//            }
//
//            val newVisited = visitedFields.+((lexicallyInsideOf, pathHead))
//
//            val traverseField =
//                resolveNonPrimitiveTypeNoContext(table, lexicallyInsideOf, nonPrim, newVisited, pos)
//
//            val newInsideOf = traverseField match {
//                case Left(err) => return Left(err)
//                case Right(res) => res._2
//            }
//
//            val tNew = t.copy(identifiers = pathRest)
//
//            val result = resolveNonPrimitiveTypeNoContext(table, lexicallyInsideOf, tNew, newVisited, pos)
//            appendToPath(pathHead, result)
        }
    }

}

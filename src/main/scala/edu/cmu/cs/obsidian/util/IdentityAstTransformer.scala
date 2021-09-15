package edu.cmu.cs.obsidian.util

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck._

import scala.collection.immutable
import scala.collection.immutable.TreeMap
import scala.util.parsing.input.Position

class IdentityAstTransformer {

    type FoldFunc[TFrom, TTo] = Int
    type Context = immutable.Map[String, ObsidianType]
    val emptyContext = new TreeMap[String, ObsidianType]()

    var currentContractSourcePath = ""

    def transformProgram(table: SymbolTable): (SymbolTable, Seq[ErrorRecord]) = {
        var errorRecords = List.empty[ErrorRecord]
        var contracts = List[Contract](ContractType.topContractImpl)
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
        cTable.contract match {
            case obsContract: ObsidianContractImpl =>
                currentContractSourcePath = obsContract.sourcePath

                var newDecls: Seq[Declaration] = Nil
                var errors = List.empty[ErrorRecord]
                for (d <- cTable.contract.declarations) {
                    val newDecl: Declaration = d.tag match {
                        case TransactionDeclTag =>
                            val (newTransaction, newErrors) = transformTransaction(table, cTable, d.asInstanceOf[Transaction])
                            errors = errors ++ newErrors
                            newTransaction
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

                val (newTypeArgs, allTypeErrors) =
                    obsContract.bound.typeArgs.map(transformType(table, cTable, emptyContext, _, obsContract.loc, Nil)).unzip
                val newBound = obsContract.bound.copy(typeArgs = newTypeArgs)

                val (newParams, errorLists) =
                    obsContract.params.map(transformType(table, cTable, emptyContext, _, obsContract.loc, Nil)).unzip

                val oldContract = obsContract
                val newContract =
                    ObsidianContractImpl(
                        oldContract.modifiers,
                        oldContract.name,
                        newParams.map(_.asInstanceOf[GenericType]),
                        newBound,
                        newDecls,
                        oldContract.transitions,
                        oldContract.isInterface,
                        oldContract.sourcePath
                    ).setLoc(obsContract)

                (newContract, errors.reverse ++ allTypeErrors.flatten.toList ++ errorLists.flatten)

            case ffiContract: JavaFFIContractImpl =>
                (ffiContract, Nil)
        }
    }

    def transformState(table: SymbolTable, sTable: StateTable): (State, Seq[ErrorRecord]) = {
        var newDecls: Seq[Field] = Nil
        var errors = List.empty[ErrorRecord]

        for (d <- sTable.ast.fields) {
            val (newDecl, newErrors) = transformField(table, sTable, d.asInstanceOf[Field])
            newDecls = newDecl +: newDecls
            errors = errors ++ newErrors
        }

        newDecls = newDecls.reverse

        val newState = sTable.ast.copy(fields = newDecls)
        (newState, errors.reverse)
    }

    def transformField(
                          table: SymbolTable,
                          lexicallyInsideOf: DeclarationTable,
                          f: Field): (Field, Seq[ErrorRecord]) = {
        val thisType = ContractReferenceType(lexicallyInsideOf.contractType, Owned(), NotRemoteReferenceType())

        val context = startContext(lexicallyInsideOf, List.empty, thisType) // Permission of this is irrelevant when transforming fields
        val (newType, errors) = transformType(table, lexicallyInsideOf, context, f.typ, f.loc, Nil)
        (f.copy(typ = newType).setLoc(f), errors)
    }

    def transformUnary(table: SymbolTable, lexicallyInsideOf: DeclarationTable, context: Context,
                       e: Expression, params: Seq[GenericType],
                       f: Expression => Expression): (Expression, List[ErrorRecord]) = {
        val (newE, errors) = transformExpression(table, lexicallyInsideOf, context, e, params)
        (f(newE), errors)
    }

    def transformBinary(table: SymbolTable, lexicallyInsideOf: DeclarationTable, context: Context,
                        e1: Expression, e2: Expression, params: Seq[GenericType],
                        f: (Expression, Expression) => Expression): (Expression, List[ErrorRecord]) = {
        val (newE1, errors1) = transformExpression(table, lexicallyInsideOf, context, e1, params)
        val (newE2, errors2) = transformExpression(table, lexicallyInsideOf, context, e2, params)
        (f(newE1, newE2), errors1 ++ errors2)
    }

    def transformExpression(table: SymbolTable, lexicallyInsideOf: DeclarationTable,
                            context: Context, e: Expression, params: Seq[GenericType]): (Expression, List[ErrorRecord]) = {
        // TODO: The AST transformers could be made cleaner via some sort of Writer monad
        e match {
            case v: ReferenceIdentifier => (v, List())
            case n: NumLiteral => (n, List())
            case s: StringLiteral => (s, List())
            case t: TrueLiteral => (TrueLiteral().setLoc(t), List())
            case f: FalseLiteral => (FalseLiteral().setLoc(f), List())
            case t: This => (This(None).setLoc(t), List()) // todo: no idea if this is right
            case p: Parent => (Parent().setLoc(p), List())
            case c: Conjunction =>
                transformBinary(table, lexicallyInsideOf, context, c.e1, c.e2, params, Conjunction(_, _).setLoc(c))
            case d: Disjunction =>
                transformBinary(table, lexicallyInsideOf, context, d.e1, d.e2, params, Disjunction(_, _).setLoc(d))
            case n: LogicalNegation =>
                transformUnary(table, lexicallyInsideOf, context, n.e, params, LogicalNegation(_).setLoc(n))
            case a: Add =>
                transformBinary(table, lexicallyInsideOf, context, a.e1, a.e2, params, Add(_, _).setLoc(a))
            case a: StringConcat =>
                transformBinary(table, lexicallyInsideOf, context, a.e1, a.e2, params, StringConcat(_, _).setLoc(a))
            case s: Subtract =>
                transformBinary(table, lexicallyInsideOf, context, s.e1, s.e2, params, Subtract(_, _).setLoc(s))
            case d: Divide =>
                transformBinary(table, lexicallyInsideOf, context, d.e1, d.e2, params, Divide(_, _).setLoc(d))
            case m: Multiply =>
                transformBinary(table, lexicallyInsideOf, context, m.e1, m.e2, params, Multiply(_, _).setLoc(m))
            case mod: Mod =>
                transformBinary(table, lexicallyInsideOf, context, mod.e1, mod.e2, params, Mod(_, _).setLoc(mod))
            case n: Negate =>
                transformUnary(table, lexicallyInsideOf, context, n.e, params, Negate(_).setLoc(n))
            case eq: Equals =>
                transformBinary(table, lexicallyInsideOf, context, eq.e1, eq.e2, params, Equals(_, _).setLoc(eq))
            case g: GreaterThan =>
                transformBinary(table, lexicallyInsideOf, context, g.e1, g.e2, params, GreaterThan(_, _).setLoc(g))
            case g: GreaterThanOrEquals =>
                transformBinary(table, lexicallyInsideOf, context, g.e1, g.e2, params, GreaterThanOrEquals(_, _).setLoc(g))
            case l: LessThan =>
                transformBinary(table, lexicallyInsideOf, context, l.e1, l.e2, params, LessThan(_, _).setLoc(l))
            case l: LessThanOrEquals =>
                transformBinary(table, lexicallyInsideOf, context, l.e1, l.e2, params, LessThanOrEquals(_, _).setLoc(l))
            case ne: NotEquals =>
                transformBinary(table, lexicallyInsideOf, context, ne.e1, ne.e2, params, NotEquals(_, _).setLoc(ne))
            case d: Dereference =>
                transformUnary(table, lexicallyInsideOf, context, d.e, params, Dereference(_, d.f).setLoc(d))
            case i: LocalInvocation =>
                val (newParams, errors) =
                    i.params.map(transformType(table, lexicallyInsideOf, context, _, e.loc, params)).unzip
                val (newArgs, argErrors) =
                    i.args.map(transformExpression(table, lexicallyInsideOf, context, _, params)).unzip
                (i.copy(args = newArgs, params = newParams).setLoc(i), errors.flatten.toList ++ argErrors.flatten.toList)
            case i: Invocation =>
                val (newRecipient, recipientErrors) =
                    transformExpression(table, lexicallyInsideOf, context, i.recipient, params)
                val (newParams, errors) =
                    i.params.map(transformType(table, lexicallyInsideOf, context, _, e.loc, params)).unzip
                val (newArgs, argErrors) =
                    i.args.map(transformExpression(table, lexicallyInsideOf, context, _, params)).unzip
                (i.copy(recipient = newRecipient, args = newArgs, params = newParams).setLoc(i),
                    recipientErrors ++
                        errors.flatten.toList ++
                        argErrors.flatten.toList)

            case c: Construction =>
                val (newParams, errors) =
                    c.contractType.typeArgs.map(transformType(table, lexicallyInsideOf, context, _, e.loc, params)).unzip
                val (newArgs, argErrors) =
                    c.args.map(transformExpression(table, lexicallyInsideOf, context, _, params)).unzip
                (c.copy(args = newArgs, contractType = c.contractType.copy(typeArgs = newParams)).setLoc(c),
                    errors.flatten.toList ++ argErrors.flatten.toList)
            case d@Disown(e) =>
                transformUnary(table, lexicallyInsideOf, context, d.e, params, Disown(_).setLoc(d))
            case s: StateInitializer => (s.copy().setLoc(s), List())
        }
    }

    def startContext(lexicallyInsideOf: DeclarationTable, args: Seq[VariableDeclWithSpec], thisType: ObsidianType): Context = {
        var startContext = emptyContext

        startContext = startContext.updated("this", thisType)

        for (a <- args) {
            startContext = startContext.updated(a.varName, a.typIn)
        }
        startContext
    }

    def transformArgs(
                         table: SymbolTable,
                         lexicallyInsideOf: DeclarationTable,
                         args: Seq[VariableDeclWithSpec],
                         thisType: ObsidianType,
                         params: Seq[GenericType]): (Seq[VariableDeclWithSpec], Seq[ErrorRecord]) = {
        var errors = List.empty[ErrorRecord]
        var newArgs: Seq[VariableDeclWithSpec] = Nil
        val context = startContext(lexicallyInsideOf, args, thisType)
        for (a <- args) {
            val (transformedType, newErrors) = transformType(table, lexicallyInsideOf, context - a.varName, a.typIn, a.loc, params)
            val (transformedTypeOut, newErrorsOut) = transformType(table, lexicallyInsideOf, context - a.varName, a.typOut, a.loc, params)
            errors = errors ++ newErrors ++ newErrorsOut

            val aNew = a.copy(typIn = transformedType, typOut = transformedTypeOut)
            newArgs = aNew +: newArgs
        }
        (newArgs.reverse, errors)
    }

    def requireNonPrimitive(originalType: NonPrimitiveType,
                            lexicallyInsideOf: DeclarationTable,
                            pos: Position,
                            transformedType: (ObsidianType, List[ErrorRecord])): (NonPrimitiveType, List[ErrorRecord]) = {
        transformedType._1 match {
            case np: NonPrimitiveType =>
                (np, transformedType._2)

            case t =>
                (originalType, Nil)
        }
    }

    def transformTransaction(
                                table: SymbolTable,
                                lexicallyInsideOf: DeclarationTable,
                                t: Transaction): (Transaction, Seq[ErrorRecord]) = {
        val context = startContext(lexicallyInsideOf, t.args, t.thisType)

        // TODO GENERIC: Maybe there's a better way. We need to ensure we return a nonprimitivetype,
        //  but we also need to transform thisType to handle generic parameters properly
        val (newThisType, thisTypeErrors) =
        requireNonPrimitive(t.thisType, lexicallyInsideOf, t.loc, transformType(table, lexicallyInsideOf, context, t.thisType, t.loc, t.params))
        val (newThisFinalType, thisFinalTypeErrors) =
            requireNonPrimitive(t.thisFinalType, lexicallyInsideOf, t.loc, transformType(table, lexicallyInsideOf, context, t.thisFinalType, t.loc, t.params))

        val (newArgs, argErrors) = transformArgs(table, lexicallyInsideOf, t.args, t.thisType, t.params)

        val (newEnsures, allEnsureErrors) = t.ensures.map(en => {
            val (newExpr, ensureErrors) = transformExpression(table, lexicallyInsideOf, context, en.expr, t.params)
            (en.copy(expr = newExpr), ensureErrors)
        }).unzip

        val (newRetType, retTypeErrors) = t.retType match {
            case None => (None, List.empty)
            case Some(retType) =>
                val (transformedType, errs) = transformType(table, lexicallyInsideOf, context, retType, t.loc, t.params)
                (Some(transformedType), errs)
        }
        val (newTransactionBody, _, bodyErrors) = transformBody(table, lexicallyInsideOf, context, t.body, t.params)
        val newTransaction =
            t.copy(retType = newRetType,
                thisType = newThisType,
                thisFinalType = newThisFinalType,
                args = newArgs,
                ensures = newEnsures,
                body = newTransactionBody).setLoc(t)
        (newTransaction,
            thisTypeErrors ++ thisFinalTypeErrors ++ retTypeErrors ++
                argErrors ++ allEnsureErrors.flatten.toList ++ bodyErrors)
    }

    def transformConstructor(
                                table: SymbolTable,
                                lexicallyInsideOf: DeclarationTable,
                                c: Constructor): (Constructor, Seq[ErrorRecord]) = {

        val (newResultType, resTypeErrors) =
            requireNonPrimitive(c.resultType, lexicallyInsideOf, c.loc,
                transformType(table, lexicallyInsideOf, emptyContext, c.resultType, c.loc, Nil))

        // Constructors always own "this".
        val thisType = ContractReferenceType(lexicallyInsideOf.contractType, Owned(), NotRemoteReferenceType())

        val (newArgs, argsTransformErrors) = transformArgs(table, lexicallyInsideOf, c.args, thisType, Nil)
        val context = startContext(lexicallyInsideOf, c.args, thisType)
        val (newBody, _, bodyTransformErrors) = transformBody(table, lexicallyInsideOf, context, c.body, Nil)
        val errors = resTypeErrors ++ argsTransformErrors ++ bodyTransformErrors

        (c.copy(resultType = newResultType, args = newArgs, body = newBody).setLoc(c), errors)
    }

    def transformBody(
                         table: SymbolTable,
                         lexicallyInsideOf: DeclarationTable,
                         inScope: Context,
                         b: Seq[Statement],
                         params: Seq[GenericType]): (Seq[Statement], Context, Seq[ErrorRecord]) = {
        b match {
            case Seq() => (Seq(), inScope, Seq())
            case s +: rest =>
                val (sNew, inScopeNew, errors) = transformStatement(table, lexicallyInsideOf, inScope, s, params)
                val (restStatements, finalContext, restErrors) = transformBody(table, lexicallyInsideOf, inScopeNew, rest, params)
                (sNew +: restStatements, finalContext, errors ++ restErrors)
        }
    }

    def transformStatement(
                              table: SymbolTable,
                              lexicallyInsideOf: DeclarationTable,
                              context: Context,
                              s: Statement,
                              params: Seq[GenericType]): (Statement, Context, Seq[ErrorRecord]) = {
        s match {
            case oldDecl@VariableDecl(typ, varName) =>
                val (newTyp, errors) = transformType(table, lexicallyInsideOf, context, typ, s.loc, params)
                (oldDecl.copy(typ = newTyp).setLoc(oldDecl), context + (varName -> newTyp), errors)
            case oldDecl@VariableDeclWithSpec(typIn, typOut, varName) =>
                val (newTypIn, errorsIn) = transformType(table, lexicallyInsideOf, context, typIn, s.loc, params)
                val (newTypOut, errorsOut) = transformType(table, lexicallyInsideOf, context, typOut, s.loc, params)
                val newDecl = oldDecl.copy(typIn = newTypIn, typOut = newTypOut)
                val totalErrors = errorsIn ++ errorsOut
                (newDecl, context + (varName -> newTypIn), totalErrors)
            case oldDecl@VariableDeclWithInit(typ, varName, e) =>
                val (newTyp, errors) = transformType(table, lexicallyInsideOf, context, typ, s.loc, params)
                val (newE, exprErrors) = transformExpression(table, lexicallyInsideOf, context, e, params)
                val newDecl = oldDecl.copy(typ = newTyp, e = newE).setLoc(oldDecl)
                (newDecl, context + (varName -> newTyp), errors ++ exprErrors)
            case r@Return() => (r, context, Seq())
            case r@ReturnExpr(e) =>
                val (newE, exprErrors) = transformExpression(table, lexicallyInsideOf, context, e, params)
                (ReturnExpr(newE).setLoc(r), context, exprErrors)
            case t@Transition(newStateName, updates, p) =>
                updates match {
                    case None => (Transition(newStateName, updates, p).setLoc(t), context, Seq())
                    case Some(u) =>
                        val mapFun = (p: (ReferenceIdentifier, Expression)) => {
                            val (newE, errs) = transformExpression(table, lexicallyInsideOf, context, p._2, params)
                            ((p._1, newE), errs)
                        }
                        val (transformedUpdates, errors) = u.map(mapFun).unzip

                        (Transition(newStateName, Some(transformedUpdates), p).setLoc(t), context, errors.flatten.toList)
                }
            case a@Assignment(assignTo, e) =>
                val (newAssignTo, assignToErrors) = transformExpression(table, lexicallyInsideOf, context, assignTo, params)
                val (newE, exprErrors) = transformExpression(table, lexicallyInsideOf, context, e, params)
                (Assignment(newAssignTo, newE).setLoc(a), context, assignToErrors ++ exprErrors)
            case t@Revert(e) =>
                e.map(transformExpression(table, lexicallyInsideOf, context, _, params)) match {
                    case Some((newE, exprErrors)) =>
                        (Revert(Some(newE)).setLoc(t), context, exprErrors)
                    case None => (Revert(None).setLoc(t), context, List())
                }
            case oldIf@If(eCond, sIf) =>
                val (newECond, condErrors) = transformExpression(table, lexicallyInsideOf, context, eCond, params)
                val (sIfNew, newContext, errors) = transformBody(table, lexicallyInsideOf, context, sIf, params)
                val newIf = oldIf.copy(s = sIfNew, eCond = newECond).setLoc(oldIf)
                (newIf, newContext, errors ++ condErrors)
            case oldIf@IfThenElse(eCond, s1, s2) =>
                val (newECond, condErrors) = transformExpression(table, lexicallyInsideOf, context, eCond, params)
                val (s1New, newContext1, errors1) = transformBody(table, lexicallyInsideOf, context, s1, params)
                val (s2New, newContext2, errors2) = transformBody(table, lexicallyInsideOf, context, s2, params)
                val newIf = oldIf.copy(
                    s1 = s1New,
                    s2 = s2New,
                    eCond = newECond
                ).setLoc(oldIf)
                (newIf, context, condErrors ++ errors1 ++ errors2)
            case oldIf@IfInState(e, ePerm, state, s1, s2) =>
                val (newE, condErrors) = transformExpression(table, lexicallyInsideOf, context, e, params)
                val (newState, errors3) = transformTypeState(table, lexicallyInsideOf, state, oldIf.loc, params)
                val (s1New, newContext1, errors1) = transformBody(table, lexicallyInsideOf, context, s1, params)
                val (s2New, newContext2, errors2) = transformBody(table, lexicallyInsideOf, context, s2, params)
                val newIf = oldIf.copy(
                    e = newE,
                    typeState = newState,
                    s1 = s1New,
                    s2 = s2New
                ).setLoc(oldIf)
                (newIf, context, condErrors ++ errors1 ++ errors2 ++ errors3)
            case oldTry@TryCatch(s1, s2) =>
                val (s1New, newContext1, errors1) = transformBody(table, lexicallyInsideOf, context, s1, params)
                val (s2New, newContext2, errors2) = transformBody(table, lexicallyInsideOf, context, s2, params)
                val newIf = oldTry.copy(s1 = s1New, s2 = s2New).setLoc(oldTry)
                (newIf, context, errors1 ++ errors2)
            case oldSwitch@Switch(e, cases) =>
                val (newE, eErrors) = transformExpression(table, lexicallyInsideOf, context, e, params)
                var errors = List.empty[ErrorRecord]
                val newCases = cases.map(_case => {
                    val (newBody, newContext, newErrors) = transformBody(table, lexicallyInsideOf, context, _case.body, params)
                    errors = errors ++ newErrors
                    _case.copy(body = newBody).setLoc(_case)
                })
                val newSwitch = oldSwitch.copy(e = newE, cases = newCases).setLoc(oldSwitch)
                (newSwitch, context, eErrors ++ errors)
            case oldAssert@StaticAssert(e, l) =>
                val (newE, exprErrors) = transformExpression(table, lexicallyInsideOf, context, e, params)
                val (newL, errors) = transformTypeState(table, lexicallyInsideOf, l, oldAssert.loc, params)
                (StaticAssert(newE, newL).setLoc(oldAssert), context, exprErrors ++ errors)
            case e: Expression =>
                val (newE, errors) = transformExpression(table, lexicallyInsideOf, context, e, params)
                (newE, context, errors)
        }
    }

    def transformTypeState(table: SymbolTable, lexicallyInsideOf: DeclarationTable, typeState: TypeState,
                           pos: Position, params: Seq[GenericType]): (TypeState, List[ErrorRecord]) =
        (typeState, List())

    def transformType(
                         table: SymbolTable,
                         lexicallyInsideOf: DeclarationTable,
                         context: Context,
                         t: ObsidianType,
                         pos: Position,
                         params: Seq[GenericType]): (ObsidianType, List[ErrorRecord]) = {
        (t, List.empty[ErrorRecord])
    }
}

package edu.cmu.cs.obsidian.util

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck._

import scala.collection.Map
import scala.collection.immutable.{TreeMap, TreeSet}
import scala.util.parsing.input.Position

class IdentityAstTransformer {

    type FoldFunc[TFrom, TTo] = Int
    type Context = Map[String, ObsidianType]
    val emptyContext = new TreeMap[String, ObsidianType]()

    var currentContractSourcePath = ""

    def transformProgram(table: SymbolTable): (SymbolTable, Seq[ErrorRecord]) = {
        var errorRecords = List.empty[ErrorRecord]
        var contracts = List[Contract](ObsidianContractImpl(Set(), "Top", Nil, GenericBoundPerm("Top", Nil, Unowned()), Nil, None, isInterface = true, ""))
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

                val oldContract = obsContract
                val newContract =
                    ObsidianContractImpl(
                        oldContract.modifiers,
                        oldContract.name,
                        oldContract.params,
                        oldContract.implementBound,
                        newDecls,
                        oldContract.transitions,
                        oldContract.isInterface,
                        oldContract.sourcePath
                    ).setLoc(obsContract)

                (newContract, errors.reverse)

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
        val thisType =  ContractReferenceType(lexicallyInsideOf.contractType, Owned(), false)

        val context = startContext(lexicallyInsideOf, List.empty, thisType) // Permission of this is irrelevant when transforming fields
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
            case mod: Mod =>
                Mod(transformExpression(mod.e1), transformExpression(mod.e2)).setLoc(mod)
            case n: Negate =>
                Negate(transformExpression(n.e)).setLoc(n)
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

            // TODO GENERIC: Probably handle type params here?
            case c: Construction =>
                c.copy(args = c.args.map(eArg => transformExpression(eArg))).setLoc(c)
            case d@Disown(e) =>
                Disown(transformExpression(e)).setLoc(d)
            case s: StateInitializer => s.copy().setLoc(s)
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
            thisType: ObsidianType): (Seq[VariableDeclWithSpec], Seq[ErrorRecord]) = {
        var errors = List.empty[ErrorRecord]
        var newArgs: Seq[VariableDeclWithSpec] = Nil
        val context = startContext(lexicallyInsideOf, args, thisType)
        for (a <- args) {
            val (transformedType, newErrors) = transformType(table, lexicallyInsideOf, context - a.varName, a.typIn, a.loc)
            errors = errors ++ newErrors

            val aNew = a.copy(typIn = transformedType)
            newArgs = aNew +: newArgs
        }
        (newArgs.reverse, errors)
    }

    def transformTransaction(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            t: Transaction): (Transaction, Seq[ErrorRecord]) = {
        val context = startContext(lexicallyInsideOf, t.args, t.thisType)


        var (newArgs, argErrors) = transformArgs(table, lexicallyInsideOf, t.args, t.thisType)

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
        val thisType =  ContractReferenceType(lexicallyInsideOf.contractType, Owned(), false)

        val (newArgs, argsTransformErrors) = transformArgs(table, lexicallyInsideOf, c.args, thisType)
        val context = startContext(lexicallyInsideOf, c.args, thisType)
        var (newBody, _, bodyTransformErrors) = transformBody(table, lexicallyInsideOf, context, c.body)
        val errors = argsTransformErrors ++ bodyTransformErrors

        (c.copy(args = newArgs, body = newBody).setLoc(c), errors)
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
            case oldDecl@VariableDeclWithSpec(typIn, typOut, varName) =>
                val (newTypIn, errorsIn) = transformType(table, lexicallyInsideOf, context, typIn, s.loc)
                val (newTypOut, errorsOut) = transformType(table, lexicallyInsideOf, context, typOut, s.loc)
                val newDecl = oldDecl.copy(typIn = newTypIn, typOut = newTypOut)
                val totalErrors = errorsIn ++ errorsOut
                (newDecl, context.updated(varName, newTypIn), totalErrors)
            case oldDecl@VariableDeclWithInit(typ, varName, e) =>
                val (newTyp, errors) = transformType(table, lexicallyInsideOf, context, typ, s.loc)
                val newDecl = oldDecl.copy(typ = newTyp, e = transformExpression(e)).setLoc(oldDecl)
                (newDecl, context.updated(varName, newTyp), errors)
            case r@Return() => (r, context, Seq())
            case r@ReturnExpr(e) => (ReturnExpr(transformExpression(e)).setLoc(r), context, Seq())
            case t@Transition(newStateName, updates, p) =>
                updates match {
                    case None => (Transition(newStateName, updates, p).setLoc(t), context, Seq())
                    case Some(u) =>
                        val mapFun = (p: (ReferenceIdentifier, Expression)) => (p._1, transformExpression(p._2))
                        val transformedUpdates = u.map(mapFun)

                        (Transition(newStateName, Some(transformedUpdates), p).setLoc(t), context, Seq())
                }
            case a@Assignment(assignTo, e) =>
                (Assignment(transformExpression(assignTo), transformExpression(e)).setLoc(a), context, Seq())
            case t@Revert(e) => (t, context, Seq())
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
            case oldIf@IfInState(e, state, s1, s2) =>
                val (s1New, newContext1, errors1) = transformBody(table, lexicallyInsideOf, context, s1)
                val (s2New, newContext2, errors2) = transformBody(table, lexicallyInsideOf, context, s2)
                val newIf = oldIf.copy(
                    s1 = s1New,
                    s2 = s2New,
                    e = transformExpression(e)
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
                    _case.copy(body = newBody).setLoc(_case)
                })
                val newSwitch = oldSwitch.copy(e = transformExpression(e),
                                               cases = newCases).setLoc(oldSwitch)
                (newSwitch, context, errors)
            case oldAssert@StaticAssert(e, l) =>
                (StaticAssert(transformExpression(e), l).setLoc(oldAssert), context, Seq())
            case e: Expression => (transformExpression(e), context, Seq())
        }
    }

    def transformType(
            table: SymbolTable,
            lexicallyInsideOf: DeclarationTable,
            context: Context,
            t: ObsidianType,
            pos: Position): (ObsidianType, List[ErrorRecord]) = {
        (t, List.empty[ErrorRecord])
    }
}

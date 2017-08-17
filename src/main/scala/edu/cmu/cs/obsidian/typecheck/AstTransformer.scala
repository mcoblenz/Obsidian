package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._

import scala.collection.immutable.TreeSet

/* The only purpose of this compilation phase at the moment is to disambiguate
 * path-types. For example, If [T] is defined as a dependent type of [C], then
 * inside of [C], the types [T] and [this.T] refer to the same thing. This
 * must be clarified. */

/* Important Note: be sure to take into account the fact that AST nodes need a location.
 * To construct a new AST node in this file, explicitly set the location using [setLoc] */

object AstTransformer {

    /* [disamiguate] functions change the [AstType]s in a ast node to have a standardized form.
     * This standardized form has the following rule:
     * - all path-dependent types start with either a local variable or "this"
     */

    def disambiguateProgram(table: SymbolTable): SymbolTable = {
        val newContracts = table.contractLookup.values.map(disambiguateContract)
        val newProgram = table.ast.copy(contracts = newContracts.toSeq).setLoc(table.ast)
        /* to make this faster, we could just mutate the AST nodes instead of
         * making the symbol table again entirely */
        new SymbolTable(newProgram)
    }

    def disambiguateContract(table: ContractTable): Contract = {
        val ast = table.ast
        val newDecls = ast.declarations.map(disambiguateDecl(table, _))
        ast.copy(declarations = newDecls).setLoc(ast)
    }

    def disambiguateState(table: StateTable): State = {
        val ast = table.ast
        val newDecls = ast.declarations.map(disambiguateDecl(table, _))
        ast.copy(declarations = newDecls).setLoc(ast)
    }

    def disambiguateDecl(insideOf: DeclarationTable, decl: Declaration): Declaration = {
        decl match {

            case f: Field =>
                val newTyp = disambiguateType(insideOf, TreeSet[String](), f.typ)
                f.copy(typ = newTyp).setLoc(f)

            case t: Transaction =>
                val inScope = t.args.map(_.varName).toSet
                val newArgs = t.args.map(arg => {
                    val newType = disambiguateType(insideOf, inScope, arg.typ)
                    arg.copy(typ = newType).setLoc(arg)
                })
                val newRet = t.retType.map(disambiguateType(insideOf, inScope, _))
                val newBody = disambiguateBody(insideOf, inScope, t.body)
                t.copy(args = newArgs, retType = newRet, body = newBody).setLoc(t)

            case f: Func =>
                val inScope = f.args.map(_.varName).toSet
                val newArgs = f.args.map(arg => {
                    val newType = disambiguateType(insideOf, inScope, arg.typ)
                    arg.copy(typ = newType).setLoc(arg)
                })
                val newRet = f.retType.map(disambiguateType(insideOf, inScope, _))
                val newBody = disambiguateBody(insideOf, inScope, f.body)
                f.copy(args = newArgs, retType = newRet, body = newBody).setLoc(f)

            case c: Constructor =>
                val inScope = c.args.map(_.varName).toSet
                val newArgs = c.args.map(arg => {
                    val newType = disambiguateType(insideOf, inScope, arg.typ)
                    arg.copy(typ = newType).setLoc(arg)
                })
                val newBody = disambiguateBody(insideOf, inScope, c.body)

                c.copy(args = newArgs, body = newBody).setLoc(c)

            case s: State => disambiguateState(insideOf.contract.state(s.name).get)

            case c: Contract => disambiguateContract(insideOf.contract(c.name).get)

            case t: TypeDecl => t

        }
    }

    def disambiguateType(
            insideOf: DeclarationTable,
            inScope: Set[String],
            typ: AstType): AstType = {
        typ match {

            case AstContractType(mods, cName) =>
                if (insideOf.contract.hasParent && insideOf.contract.name == cName) {
                    AstPathContractType(mods, "this"::"parent"::Nil, cName).setLoc(typ)
                }
                else if (insideOf.contract.childContract(cName).isDefined) {
                    AstPathContractType(mods, "this"::Nil, cName).setLoc(typ)
                }
                else {
                    typ
                }

            case AstStateType(mods, cName, sName) =>
                if (insideOf.contract.hasParent && insideOf.contract.name == cName) {
                    AstPathStateType(mods, "this"::"parent"::Nil, cName, sName).setLoc(typ)
                }
                else if (insideOf.contract.childContract(cName).isDefined) {
                    AstPathStateType(mods, "this"::Nil, cName, sName).setLoc(typ)
                }
                else {
                    typ
                }

            case oldTyp@AstPathContractType(_, path, _) =>
                if ((inScope contains path.head) || path.head == "this")
                    typ
                else
                    oldTyp.copy(path = "this" +: oldTyp.path).setLoc(oldTyp)

            case oldTyp@AstPathStateType(_, path, _, _) =>
                if ((inScope contains path.head) || path.head == "this")
                    typ
                else
                    oldTyp.copy(path = "this" +: oldTyp.path).setLoc(oldTyp)

            case other => other

        }
    }

    def disambiguateStatement(
            insideOf: DeclarationTable,
            inScope: Set[String],
            s: Statement): (Statement, Set[String]) = {
        s match {
            case oldDecl@VariableDecl(typ, varName) =>
                val newTyp = disambiguateType(insideOf, inScope, typ)
                (oldDecl.copy(typ = newTyp).setLoc(oldDecl), inScope + varName)
            case oldDecl@VariableDeclWithInit(typ, varName, _) =>
                val newTyp = disambiguateType(insideOf, inScope, typ)
                (oldDecl.copy(newTyp).setLoc(oldDecl), inScope + varName)
            case oldIf@If(_, sIf) =>
                val sIfNew = disambiguateBody(insideOf, inScope, sIf)
                (oldIf.copy(s = sIfNew).setLoc(oldIf), inScope)
            case oldIf@IfThenElse(e, s1, s2) =>
                val s1New = disambiguateBody(insideOf, inScope, s1)
                val s2New = disambiguateBody(insideOf, inScope, s2)
                (oldIf.copy(s1 = s1New, s2 = s2New).setLoc(oldIf), inScope)
            case oldTry@TryCatch(s1, s2) =>
                val s1New = disambiguateBody(insideOf, inScope, s1)
                val s2New = disambiguateBody(insideOf, inScope, s2)
                (oldTry.copy(s1 = s1New, s2 = s2New).setLoc(oldTry), inScope)
            case oldSwitch@Switch(_, cases) =>
                val newCases = cases.map(_case => {
                    val newBody = disambiguateBody(insideOf, inScope, _case.body)
                    _case.copy(body = newBody).setLoc(oldSwitch)
                })
                (oldSwitch.copy(cases = newCases).setLoc(oldSwitch), inScope)
            case _ => (s, inScope)
        }
    }

    def disambiguateBody(
            insideOf: DeclarationTable,
            inScope: Set[String],
            body: Seq[Statement]): Seq[Statement] = {
        body match {
            case Seq() => Seq()
            case s +: rest =>
                val (sNew, inScopeNew) = disambiguateStatement(insideOf, inScope, s)
                sNew +: disambiguateBody(insideOf, inScopeNew, rest)
        }
    }
}

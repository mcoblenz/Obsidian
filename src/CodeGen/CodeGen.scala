package CodeGen

import edu.cmu.cs.obsidian.parser._
import com.sun.codemodel.internal._

class CodeGen {

    private val model: JCodeModel = new JCodeModel()

    def translateProgram(program: Program): JCodeModel = {
        val programPackage: JPackage = model._package("edu.cmu.cs.obsidian.generated-code") // Put all generated code in the same package.


        for (aContract: Contract <- program.contracts) {
            translateContract(aContract, programPackage)
        }

        model
    }


    // Contracts translate to compilation units containing one class.
    private def translateContract (aContract: Contract, programPackage: JPackage): Unit = {
        val newClass: JDefinedClass = programPackage._class(aContract.name)
        newClass.field(JMod.PRIVATE, model.ref("String"), "__state")
        val stateMeth = newClass.method(JMod.PUBLIC, model.ref("String"), "getState")
        stateMeth.body()._return(JExpr.ref("__state"))

        for (decl <- aContract.declarations) {
            translateDeclaration(decl, newClass)
        }
    }

    private def translateDeclaration(declaration: Declaration, newClass: JDefinedClass): Unit = {
        declaration match {
            case t@TypeDecl(_,_) => () // TODO
            case f@Field(_,_) => translateFieldDecl(f, newClass)
            case f@Func(_,_,_) => translateFuncDecl(f, newClass)
            case t@Transaction(_,_,_) => translateTransDecl(t, newClass)
            case s@State(_,_) => translateStateDecl(s, newClass)
        }
    }

    private def resolveType(typ: Type): JType = {
        typ match {
            case IntType() => model.INT
            case BoolType() => model.BOOLEAN
            case StringType() => model.ref("String")
            case NonPrimitiveType(mods, name) => model.ref(name)
        }
    }

    private def translateFieldDecl(decl: Field, newClass: JDefinedClass): Unit = {
        newClass.field(JMod.PRIVATE, resolveType(decl.typ), decl.fieldName)
    }

    /* returns an expr because exprs are built bottom-up (unlike everything else) */
    private def translateExpr(e: Expression): JExpression = {
        /* helper for invocations */
        val foldF = (inv: JInvocation, arg: Expression) => inv.arg(translateExpr(arg))

        e match {
            case Variable(x) => JExpr.ref(x)
            case NumLiteral(n) => JExpr._new(model.ref("BigInteger")).arg(JExpr.lit(n))
            case StringLiteral(s) => JExpr.lit(s)
            case Conjunction(e1, e2) => translateExpr(e1).band(translateExpr(e2))
            case Disjunction(e1, e2) => translateExpr(e1).bor(translateExpr(e2))
            case LogicalNegation(e) => translateExpr(e).not()
            case Add(e1, e2) => translateExpr(e1).invoke("add").arg(translateExpr(e2))
            case Subtract(e1, e2) => translateExpr(e1).invoke("subtract").arg(translateExpr(e2))
            case Multiply(e1, e2) => translateExpr(e1).invoke("multiply").arg(translateExpr(e2))
            case Divide(e1, e2) => translateExpr(e1).invoke("divide").arg(translateExpr(e2))
            case Equals(e1, e2) => translateExpr(e1).invoke("equals").arg(translateExpr(e2))
            case GreaterThan(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).eq(JExpr.lit(1))
            case GreaterThanOrEquals(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).ne(JExpr.lit(-1))
            case LessThan(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).eq(JExpr.lit(-1))
            case LessThanOrEquals(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).ne(JExpr.lit(1))
            case NotEquals(e1, e2) =>
                translateExpr(e1).invoke("equals").arg(translateExpr(e2)).not()
            case Dereference(e, f) => translateExpr(e).ref(f)
            case LocalInvocation(name, args) => args.foldLeft(JExpr.invoke(name))(foldF)
            case Invocation(rec, name, args) => args.foldLeft(JExpr.invoke(translateExpr(rec), name))(foldF)
            case Construction(name, args) => args.foldLeft(JExpr._new(model.ref(name)))(foldF)
        }
    }

    private def translateStatement(body: JBlock, statement: Statement): Unit = {
        statement match {
            case VariableDecl(typ, name) => body.decl(resolveType(typ), name)
            case Return => body._return()
            case ReturnExpr(e) => body._return(translateExpr(e))
            case Transition(newState) => body.assign(JExpr.ref("__state"), JExpr.lit(newState))
            case Assignment(Dereference(eDeref, field), e) => {
                // TODO: this should call method to set state, not assign manually
                val fRef = JExpr.ref(translateExpr(eDeref), field)
                body.assign(fRef, translateExpr(e))
            }
            case Assignment(Variable(x), e) => {
                val ref = JExpr.ref(x)
                body.assign(ref, translateExpr(e))
            }
            case Throw() => {
                body._throw(JExpr._new(model.ref("RuntimeException")))
            }
            case If(e, s) => translateBody(body._if(translateExpr(e))._then(), s)
            case IfThenElse(e, s1, s2) => {
                val jIf = body._if(translateExpr(e))
                translateBody(jIf._then(), s1)
                translateBody(jIf._else(), s2)
            }
            case TryCatch(s1, s2) => {
                val jTry = body._try()
                val jCatch = jTry._catch(model.ref("RuntimeException"))
                translateBody(jTry.body(), s1)
                translateBody(jCatch.body(), s2)
            }
            case Switch(e, cases) => {
                val h :: _ = cases
                val jEx = translateExpr(e)
                val eqState = (s: String) =>
                    JExpr.invoke(jEx, "getState").eq(JExpr.lit(s))

                val jIf = body._if(eqState(h.stateName))
                translateBody(jIf._then(), h.body)

                var jPrev = jIf
                for (_case <- cases) {
                    jPrev = jPrev._elseif(eqState(_case.stateName))
                    translateBody(jPrev._then(), _case.body)
                }
            }
            case LocalInvocation(methName, args) => {
                val inv = body.invoke(methName)

                for (arg <- args) {
                    inv.arg(translateExpr(arg))
                }
            }
            case Invocation(e, methName, args) => {
                val inv = body.invoke(translateExpr(e), methName)

                for (arg <- args) {
                    inv.arg(translateExpr(arg))
                }
            }

            /* all expressions can be statements but no other expressions have a reasonable meaning */
            case _ => ()
        }
    }

    private def translateBody(body: JBlock, statements: Seq[Statement]): Unit = {
        for (st <- statements) {
            translateStatement(body, st)
        }
    }

    private def translateFuncDecl(decl: Func, newClass: JDefinedClass): Unit = {
        val meth: JMethod = newClass.method(JMod.PRIVATE, model.VOID, decl.name)

        /* add args */
        for (arg <- decl.args) {
            meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */
        translateBody(meth.body(), decl.body)
    }

    private def translateTransDecl(decl: Transaction, newClass: JDefinedClass): Unit = {
        val meth: JMethod = newClass.method(JMod.PUBLIC, model.VOID, decl.name)

        /* add args */
        for (arg <- decl.args) {
            meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */
        translateBody(meth.body(), decl.body)
    }

    private def translateStateDecl(state: State, newClass: JDefinedClass): Unit = {
        for (decl <- state.declarations) {
            translateDeclaration(decl, newClass)
        }
    }
}

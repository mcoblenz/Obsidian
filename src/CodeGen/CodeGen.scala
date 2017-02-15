package CodeGen

import java.io._

import Parser._
import com.sun.codemodel.internal._

class CodeGen {

    val model : JCodeModel = new JCodeModel()

    def translateProgram (program : Program) : Unit = {
        val programPackage : JPackage = model._package("edu.cmu.cs.obsidian.generated-code") // Put all generated code in the same package.


        for (aContract : Contract <- program.contracts) {
            translateContract(aContract, programPackage)
        }

        model.build(new File("out/generated_java"))
    }


    // Contracts translate to compilation units containing one class.
    def translateContract (aContract : Contract, programPackage : JPackage): Unit = {
        val newClass : JDefinedClass = programPackage._class(aContract.name)
        for (decl <- aContract.declarations) {
            translateDeclaration(decl, newClass)
        }
    }

    def translateDeclaration(declaration: Declaration, newClass : JDefinedClass): Unit = {
        declaration match {
            case t@TypeDecl(_,_) => () // TODO
            case f@Field(_,_) => translateFieldDecl(f, newClass)
            case f@Func(_,_,_) => // TODO
            case t@Transaction(_,_,_) =>
            case s@State(_,_) =>
        }
    }

    def resolveType(name : String) : String = {
        name match {
            case "ether" => "Ether"
            case "int" => "BigInteger"
            case other => other
        }
    }

    def translateFieldDecl(decl : Field, newClass : JDefinedClass) : Unit = {
        newClass.field(JMod.PRIVATE, model.ref(decl.typ.name), decl.fieldName)
    }

    /* returns an expr because exprs are built bottom-up (unlike everything else) */
    def translateExpr(e : Expression): JExpression = {
        null
    }

    def translateStatement(body : JBlock, statement : Statement): Unit = {
        statement match {
            case VariableDecl(typ, name) => body.decl(model.ref(typ.name), name)
            case Return => body._return()
            case ReturnExpr(e) => body._return(translateExpr(e))
            case Transition(newState) => {
                val inv = body.invoke("transition")
                inv.arg(JExpr.lit(newState))
            }
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
                val jIf = body._if(jEx.eq(JExpr.lit(h.stateName)))
                translateBody(jIf._then(), h.body)

                var jPrev = jIf
                for (_case <- cases) {
                    jPrev = jPrev._elseif(jEx.eq(JExpr.lit(_case.stateName)))
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

    def translateBody(body : JBlock, statements : Seq[Statement]): Unit = {
        for (st <- statements) {
            translateStatement(body, st)
        }
    }

    def translateFuncDecl(decl : Func, newClass : JDefinedClass) : Unit = {
        val meth : JMethod = newClass.method(JMod.PRIVATE, model.VOID, decl.name)

        /* add args */
        for (arg <- decl.args) {
            meth.param(model.ref(arg.typ.name), arg.varName)
        }

        /* add body */
        val body = meth.body()
        translateBody(body, decl.body)
    }

    def translateTransDecl(decl : Transaction, newClass : JDefinedClass) : Unit = {
        val meth : JMethod = newClass.method(JMod.PUBLIC, model.VOID, decl.name)

        /* add args */
        for (arg <- decl.args) {
            meth.param(model.ref(arg.typ.name), arg.varName)
        }

        /* add body */
        val body = meth.body()
        translateBody(body, decl.body)
    }

    def translateStateDecl(decl : State, newClass : JDefinedClass) : Unit = {

    }
}

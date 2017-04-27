package edu.cmu.cs.obsidian.codegen

import java.io.BufferedWriter

import com.helger.jcodemodel.JExpr
import edu.cmu.cs.obsidian.parser._

import collection.JavaConverters._


/**
  * Created by mcoblenz on 4/27/17.
  */
class DafnyGen {
    def translateProgram(p: Program, outputFile: java.io.File): Unit = {
        val fileWriter = new java.io.FileWriter(outputFile)
        val stringWriter = new BufferedWriter(fileWriter)

        for (c: Contract <- p.contracts) {
            val dafnyClass = translateContract(c)

            // Debug
            println(dafnyClass)

            stringWriter.write(dafnyClass)
            stringWriter.newLine()
        }
        stringWriter.flush()
        fileWriter.flush()
    }

    private def indent(s: String, indents: Int): String = {
        var indentString = ""
        for (i <- 1 to indents) {
            indentString += "  "
        }
        val indentedList = s.split("\n").map((str: String) => indentString + str)
        String.join("\n", indentedList.toIterable.asJava)

    }

    def translateContract(c: Contract): String = {
        val translatedDeclarations = c.declarations.map(translateDeclaration)
        val classContents = String.join("\n", translatedDeclarations.toIterable.asJava)



        val classDecl = "class " + c.name + " {\n" + indent(classContents, 1) + "\n}"

        classDecl
    }

    def translateDeclaration(d: Declaration): String = {
        d match {
            case f@Field(typ, fieldName) => translateField(f)
            case t@Transaction(_, _, _, _, _) => translateTransaction(t)
            case _ => "" // TODO
        }
    }

    def translateType(t: Type): String = {
        val typeDecl = t match {
            case IntType() => "int"
            case BoolType() => "bool"
            case StringType() => "string"
            case NonPrimitiveType(m, name) => name
        }

        typeDecl
    }

    def translateField(f: Field): String = {
        val typeDecl = translateType(f.typ)
        "var " + f.fieldName + ": " + typeDecl
    }

    def translateTransaction(t: Transaction): String = {
        "method " + t.name + " (" + translateArgs(t.args) + ")" + "\n{" + indent(translateBody(t.body), 1) + "\n}"
    }

    def translateArgs(args: Seq[VariableDecl]): String = {
        def translateArg(arg: VariableDecl): String = {
            arg.varName + ": " + translateType(arg.typ)
        }

        String.join(", ", args.map(translateArg).toIterable.asJava)
    }

    def translateExpression(e: Expression): String = {
        e match {
            case Variable(x) => x
            case _ => "" // TODO: the rest of the cases
        }
    }

    def translateStatement(s: Statement): String = {
        s match {
            case VariableDecl(typ, name) =>
                val initializer = typ match {
                    case BoolType() => "false"
                    case _ => "null"
                }
                "var " + name + " := " + initializer
            case VariableDeclWithInit(typ, name, e) =>
                "var " + name + " := " + translateExpression(e)

            case Return => "return"

            case ReturnExpr(e) => "return" + translateExpression(e)

            case Transition(newState, updates) => ""
                /* We must (in this order):
                 * 1) define local variables that match the signature for the state constructor
                 * 2) execute the state constructor.
                 */
                /* construct a new instance of the inner contract */
                /*val decls =

                   // OLD CODE BELOW HERE
                val newStField = translationContext.states(newState).innerClassField
                body.assign(newStField, JExpr._new(translationContext.states(newState).innerClass))

                /* assign fields in the update construct */
                for ((f, e) <- updates) {
                    body.assign(newStField.ref(f.x), translateExpr(e, translationContext, localContext))
                }

                /* assign conserved fields (implicit to programmer) */
                body.invoke(conserveFieldsName).arg(translationContext.states(newState).enumVal)

                /* nullify old state inner class field */
                body.invoke(deleteOldStateName)

                /* change the enum to reflect the new state */
                body.assign(JExpr.ref(stateField), translationContext.getEnum(newState))
                */

            case Assignment(Variable(x), e) =>
                x + " := " + translateExpression(e)
            /* it's bad that this is a special case */
            case Assignment(Dereference(This(), field), e) => {
                /* we don't check the local context and just assume it's a field */
                field + " := " + translateExpression(e)
            }
            case Assignment(Dereference(eDeref, field), e) => ""
                // TODO: do we ever need this in the general case if all contracts are encapsulated?
            case Throw() => "" // TODO; not supported yet
            case If(e, s) =>
                "if (" + translateExpression(e) + ") {\n" +
                    indent(translateBody(s), 1) +
                    "\n}"
                /*
            case IfThenElse(e, s1, s2) =>
                val jIf = body._if(translateExpr(e, translationContext, localContext))
                translateBody(jIf._then(), s1, translationContext, localContext)
                translateBody(jIf._else(), s2, translationContext, localContext)

            case TryCatch(s1, s2) =>
                val jTry = body._try()
                val jCatch = jTry._catch(model.ref("RuntimeException"))
                translateBody(jTry.body(), s1, translationContext, localContext)
                translateBody(jCatch.body(), s2, translationContext, localContext)

            case Switch(e, cases) =>
                val h :: remainingCases = cases
                val jEx = translateExpr(e, translationContext, localContext)
                val eqState = (s: String) =>
                    // TODO
                    /* somewhat of a bad workaround, but the alternative involves knowing the
                     * type of the expression jEx: in general, this requires an analysis
                     * to link references to declarations */
                    JExpr.invoke(jEx, "getState").invoke("toString").invoke("equals").arg(JExpr.lit(s))
                val jIf = body._if(eqState(h.stateName))
                translateBody(jIf._then(), h.body, translationContext, localContext)

                var jPrev = jIf
                for (_case <- remainingCases) {
                    jPrev = jPrev._elseif(eqState(_case.stateName))
                    translateBody(jPrev._then(), _case.body, translationContext, localContext)
                }
            case LocalInvocation(methName, args) =>
                addArgs(translationContext.invokeTransactionOrFunction(methName),
                    args, translationContext, localContext)
            /* TODO : it's bad that this is a special case */
            case Invocation(This(), methName, args) =>
                addArgs(translationContext.invokeTransactionOrFunction(methName),
                    args, translationContext, localContext)

            case Invocation(e, methName, args) =>
                addArgs(body.invoke(translateExpr(e, translationContext, localContext), methName),
                    args, translationContext, localContext)

            /* all expressions can be statements but no other expressions have a reasonable meaning */
            case _ => ()
            */
        }
    }

    def translateBody(statements: Seq[Statement]): String = {
        val translatedStatements = statements.map(translateStatement)
        String.join(";\n", translatedStatements.toIterable.asJava)

    }
}

package edu.cmu.cs.obsidian.codegen

import java.io.BufferedWriter

import com.helger.jcodemodel.JExpr
import edu.cmu.cs.obsidian.parser._

import collection.JavaConverters._


/**
  * Created by mcoblenz on 4/27/17.
  */

class StateDeclaration (val contents: String);

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

        val allStatesAsDecls = c.declarations.filter((d: Declaration) => d.isInstanceOf[State])
        val allStates = allStatesAsDecls.map((d: Declaration) => d.asInstanceOf[State])
        val translatedDeclarationsAndStates = c.declarations.map(translateDeclaration(allStates)(c.name))
        val (translatedDeclarations, translatedStates) = translatedDeclarationsAndStates.unzip

        // translatedStates is a list of all the different states in the contract. Translate those all into one datatype declaration.
        val flattenedStates = translatedStates.flatten :+ new StateDeclaration("defaultState")

        val stateContents = String.join(" | ", flattenedStates.map((s: StateDeclaration) => s.contents).toIterable.asJava)
        val stateTypeName = c.name + "_states"
        val stateDeclaration = "datatype " + stateTypeName + " = " + indent(stateContents, 1)



        val stateField = "var state__: " + stateTypeName + "\n\n"

        val classConstructor = "constructor()\n" + "modifies this;\n" + "{\n" +
        indent("this.state__ := defaultState;", 1) + "\n}\n\n"

        val classContents = stateField + classConstructor + String.join("\n", translatedDeclarations.toIterable.asJava)



        val classDecl = "class " + c.name + " {\n"  +
            indent(classContents, 1) + "\n}"

        stateDeclaration + "\n\n" + classDecl
    }

    def translateDeclaration(allStates: Seq[State])(containingContractName: String)(d: Declaration) : (String, Seq[StateDeclaration]) = {
        d match {
            case f@Field(typ, fieldName) => (translateField(f), Nil)
            case t@Transaction(_, _, _, _, _) => (translateTransaction(allStates)(t), Nil)
            case s@State(_, _) => ("", List(translateState(s, containingContractName)))
            case _ => ("", Nil) // TODO
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

    def translateTransaction(allStates: Seq[State])(t: Transaction): String = {
        val ensuresExprs = t.ensures
        val ensuresStrings = ensuresExprs.map((e: Ensures) => "ensures " + (translateExpression(e.expr)) + ";\n")
        val ensuresClause = String.join("", ensuresStrings.toIterable.asJava)

        "method " + t.name + " (" + translateArgs(t.args) + ")\n" +
            "modifies this\n" +
            ensuresClause +
            "{" + indent(translateBody(allStates: Seq[State])(t.body), 1) + "\n}\n"
    }

    def translateArgs(args: Seq[VariableDecl]): String = {
        def translateArg(arg: VariableDecl): String = {
            arg.varName + ": " + translateType(arg.typ)
        }

        String.join(", ", args.map(translateArg).toIterable.asJava)
    }

    def translateExpression(e: Expression): String = {
        e match {
            case Variable(x) =>
                x.toString
            case NumLiteral(value) =>
                value.toString
            case StringLiteral(value) =>
                value.toString
            case TrueLiteral() =>
                "true"
            case FalseLiteral() =>
                "false"
            case This() =>
                "this"
            case Conjunction(e1, e2) =>
                translateExpression(e1) + " && " + translateExpression(e2)
            case Disjunction(e1, e2) =>
                translateExpression(e1) + " || " + translateExpression(e2)
            case LogicalNegation(e) =>
                "!" + translateExpression(e)
            case Add(e1, e2) =>
                translateExpression(e1) + " + " + translateExpression(e2)
            case Subtract(e1, e2) =>
                translateExpression(e1) + " - " + translateExpression(e2)
            case Divide(e1, e2) =>
                translateExpression(e1) + " / " + translateExpression(e2)
            case Multiply(e1, e2) =>
                translateExpression(e1) + " * " + translateExpression(e2)
            case Equals(e1, e2) =>
                translateExpression(e1) + " == " + translateExpression(e2)
            case GreaterThan(e1, e2) =>
                translateExpression(e1) + " > " + translateExpression(e2)
            case GreaterThanOrEquals(e1, e2) =>
                translateExpression(e1) + " >= " + translateExpression(e2)
            case LessThan(e1, e2) =>
                translateExpression(e1) + " < " + translateExpression(e2)
            case LessThanOrEquals(e1, e2) =>
                translateExpression(e1) + " <= " + translateExpression(e2)
            case NotEquals(e1, e2) =>
                translateExpression(e1) + " != " + translateExpression(e2)
            case Dereference(e, f) =>
                assert(false).toString // Not implemented.
            case LocalInvocation(name, args) =>
                name.toString + "(" + args.mkString(", ") + ")"
            case Invocation(recipient, name, args) =>
                recipient.toString + "." + name.toString + "(" + args.mkString(", ") + ")"
            case Construction(name, args) =>
                "new " + name.toString + "(" + args.mkString(", ") + ")"
            case _ => ""
        }
    }

    def translateStatement(allStates: Seq[State])(s: Statement): String = {
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

            case Transition(newState, updates: Seq[(Variable, Expression)]) =>
                // There's no state constructor supported yet. Instead, we need to set up the state using the updates.
                // They better be complete…
                // ->S1({s1 = 3}) maps to…
                // state__ := S1(3)

                val destState = allStates.find((s: State) => s.name.equals(newState))

                // Typechecker should have rejected this program otherwise.
                assert(destState.isDefined)
                val stateFieldDecls = destState.get.declarations.filter((d: Declaration) => d.isInstanceOf[Field])
                val stateFields = stateFieldDecls.map((d: Declaration) => d.asInstanceOf[Field])
                // The order of the fields matters. We have to reorder the input expressions according to the order in which
                // the fields were declared.

                def mapField (f: Field): String = {
                    // For each field in the state, find the binding in the update list.
                    val foundPair = updates.find((u : (Variable, Expression)) => u._1.x.equals(f.fieldName))
                    assert(foundPair.isDefined)
                    translateExpression(foundPair.get._2)
                }

                val initializers = stateFields.map(mapField)
                "state__ := " + newState + "(" + String.join(", ", initializers.toIterable.asJava) + ");"
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
                "if " + translateExpression(e) + " {\n" +
                    indent(translateBody(allStates)(s), 1) +
                    "\n}"

            case IfThenElse(e, s1, s2) =>
                "if " + translateExpression(e) + " {\n" +
                    indent(translateBody(allStates)(s1), 1) +
                    "\n}\nelse {\n" + indent(translateBody(allStates)(s2), 1) + "\n}"
/*
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
            */
            case LocalInvocation(methName, args) =>
                invokeTransactionOrFunction(methName, args)
            /* TODO : it's bad that this is a special case */
            case Invocation(This(), methName, args) =>
                invokeTransactionOrFunction(methName, args)
            /*
            case Invocation(e, methName, args) =>
                addArgs(body.invoke(translateExpr(e, translationContext, localContext), methName),
                    args, translationContext, localContext)

            /* all expressions can be statements but no other expressions have a reasonable meaning */
            case _ => ()
            */
        }
    }

    def invokeTransactionOrFunction(name: String, args: Seq[Expression]): String = {
        val argsString = String.join(",", args.map(translateExpression).asJava)
        name + "(" + argsString + ")"
    }

    def translateBody(allStates: Seq[State])(statements: Seq[Statement]): String = {
        val translatedStatements = statements.map(translateStatement(allStates))
        String.join(";\n", translatedStatements.toIterable.asJava)

    }

    def translateState(state: State, containingContractName: String): StateDeclaration = {
        // The fields in the declarations have to be unwrapped into cases in an algebraic datatype.

        def extractFieldFromDeclaration(d: Declaration) = {
            d match {
                case Field(typ, fieldName) => fieldName + ": " + translateType(typ)
                case _ => ""
            }
        }

        val fieldsForState = state.declarations.map(extractFieldFromDeclaration)

        val stateContents = state.name + "(" + String.join(",", fieldsForState.toIterable.asJava) + ")"



        new StateDeclaration(stateContents)
    }
}

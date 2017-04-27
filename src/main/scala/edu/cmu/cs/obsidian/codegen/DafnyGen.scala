package edu.cmu.cs.obsidian.codegen

import java.io.BufferedWriter

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

    def translateField(f: Field): String = {
        val typeDecl = f.typ match {
            case IntType() => "int"
            case BoolType() => "bool"
            case StringType() => "string"
            case NonPrimitiveType(m, name) => name
        }

        "var " + f.fieldName + ": " + typeDecl
    }

    def translateTransaction(t: Transaction): String = {
        "method " + t.name + " (" + translateArgs(t.args) + ")" + "\n{" + indent(translateBody(t.body), 1) + "\n}"
    }

    def translateArgs(args: Seq[VariableDecl]): String = {
        ""
    }

    def translateBody(statements: Seq[Statement]): String = {
        ""
    }
}

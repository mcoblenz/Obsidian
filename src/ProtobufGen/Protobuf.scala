package edu.cmu.cs.obsidian.protobuf

import java.io.{File, PrintWriter}

/**
  * Created by mcoblenz on 2/17/17.
  */

class Protobuf (var messages: Seq[ProtobufMessage]) {
    def build(outputFile: File): Unit = {
        outputFile.delete()
        outputFile.createNewFile()


        val translatedMessages: Seq[String] = messages.map((m) => m.build(1, 0)._1)
        val writer: PrintWriter = new PrintWriter(outputFile)

        writer.write("syntax = \"proto3\";\n\n")

        for (t <- translatedMessages) {
            writer.write(t)
            writer.write("\n")
        }
        writer.close();
    }
}

abstract class ProtobufDeclaration {
    def build(nextFieldIndex: Int, nestingLevel: Int) : (String, Int) // Returns a string protobuf declaration and the next field index.

    def spaces(nestingLevel: Int) : String = {
        val str : StringBuilder = new StringBuilder()

        for (i <- 1 to (nestingLevel * 4)) {
            str.append(" ")
        }

        str.mkString
    }
}

case class ProtobufMessage (decls: Seq[ProtobufDeclaration], messageName: String) extends ProtobufDeclaration {

    // nestingLevel specifies how many messages deep this message is so that we can do indentation correctly.
    def build(initialNextFieldIndex: Int, nestingLevel: Int) : (String, Int) = {
        var translatedMessages : List[String] = List.empty[String]
        var nextFieldIndex = initialNextFieldIndex
        for (d <- decls) {
            val buildResult = d.build(nextFieldIndex, nestingLevel + 1)
            nextFieldIndex = buildResult._2
            translatedMessages = buildResult._1 :: translatedMessages
        }

        val allMessages = translatedMessages.foldLeft("")((out, msg) => out + msg + "\n")
        (spaces(nestingLevel) + "message " + messageName + " {" + "\n" + allMessages + spaces(nestingLevel) + "}\n", nextFieldIndex)
    }
}
case class ProtobufField (fieldType: FieldType, fieldName: String) extends ProtobufDeclaration {
    def build(nextFieldIndex: Int, nestingLevel: Int) : (String, Int) = {
        (spaces(nestingLevel)  + fieldType.typeString() + " " + fieldName + " = " + nextFieldIndex + ";", nextFieldIndex + 1)
    }
}


abstract class FieldType {
    def typeString() : String
}

case class BoolType() extends FieldType {
    def typeString() = {
        "bool"
    }
}

case class IntType() extends FieldType { // mathematical integer
    def typeString() = {
        "bytes"
    }
}

case class StringType() extends FieldType {
    def typeString() = {
        "string"
    }
}

case class ObjectType(typeName: String) extends FieldType {
    def typeString() = {
        typeName
    }
}




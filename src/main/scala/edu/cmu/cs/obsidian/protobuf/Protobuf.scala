package edu.cmu.cs.obsidian.protobuf

import java.io.{File, PrintWriter}

/**
  * Created by mcoblenz on 2/17/17.
  */

class Protobuf (var messages: Seq[ProtobufDeclaration]) {
    final val packageName: String = "org.hyperledger.fabric.example"

    def build(outputFile: File, targetOuterClassName: String): Unit = {
        outputFile.delete()
        outputFile.createNewFile()


        val translatedMessages: Seq[String] = messages.map((m) => m.build(1, 0)._1)
        val writer: PrintWriter = new PrintWriter(outputFile)

        writer.write("syntax = \"proto3\";\n\n")
        writer.write("option java_outer_classname = \"" + targetOuterClassName + "\";\n\n")
        writer.write("option java_package = \"" + packageName + "\";\n\n")
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

case class ProtobufDeclarationPair (d1: ProtobufDeclaration, d2: ProtobufDeclaration) extends ProtobufDeclaration {
    def build(initialNextFieldIndex: Int, nestingLevel: Int) : (String, Int) = {
        (d1.build(initialNextFieldIndex, nestingLevel)._1 + "\n" + d2.build(initialNextFieldIndex, nestingLevel)._1, nestingLevel)
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

        val allMessages = translatedMessages.foldRight("")((msg, out) => out + msg + "\n\n")
        (spaces(nestingLevel) + "message " + messageName + " {" + "\n" + allMessages + spaces(nestingLevel) + "}", nextFieldIndex)
    }
}
case class ProtobufField (fieldType: FieldType, fieldName: String) extends ProtobufDeclaration {
    def build(nextFieldIndex: Int, nestingLevel: Int) : (String, Int) = {
        (spaces(nestingLevel)  + fieldType.typeString() + " " + fieldName + " = " + nextFieldIndex + ";", nextFieldIndex + 1)
    }
}

case class ProtobufOneOf(name: String, options: List[(FieldType, String)]) extends ProtobufDeclaration {
    def build(nextFieldIndex: Int, nestingLevel: Int) : (String, Int) = {
        val (specBody, nextNextFieldIndex) = options.foldLeft(("", nextFieldIndex))(
            (state: (String, Int), option: (FieldType, String)) => {
                val buildStr = state._1 + spaces(nestingLevel + 1) +
                    option._1.typeString() + " " + option._2 + " = " + state._2 + ";\n"
                val newFieldIndex = state._2 + 1
                (buildStr, newFieldIndex)
            })

        val spec = spaces(nestingLevel) + "oneof " + name + " {\n" + specBody + spaces(nestingLevel) + "}"
        (spec, nextNextFieldIndex)
    }
}

case class ProtobufEnum (name: String, values: List[String]) extends ProtobufDeclaration {
    def build(nextFieldIndex: Int, nestingLevel: Int) : (String, Int) = {
        val valuesSpec = values.foldLeft(("", 0))((state: (String, Int), value: String) =>
            (state._1 + spaces(nestingLevel+1) + "__" + value.toUpperCase(java.util.Locale.US) + " = " + state._2 + ";\n", state._2 + 1))

        val spec = spaces(nestingLevel) + "enum " + name + " {\n" + valuesSpec._1 + spaces(nestingLevel) + "}"

        (spec, nextFieldIndex)
    }
}


abstract class FieldType {
    def typeString() : String
}

case class EnumType(enumName: String) extends FieldType {
    def typeString() = enumName
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




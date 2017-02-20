package ProtobufGen

import java.io.File

import edu.cmu.cs.obsidian.parser.Contract

/**
  * Created by mcoblenz on 2/17/17.
  */

class Protobuf (var messages: Seq[ProtobufMessage]) {
    def build(outputDir: File): Unit = {
        outputDir.delete()
        outputDir.createNewFile()
    }
}

abstract class ProtobufDeclaration

case class ProtobufMessage (fields: Seq[ProtobufDeclaration]) extends ProtobufDeclaration
case class ProtobufField (fieldType: FieldType, fieldName: String) extends ProtobufDeclaration


abstract class FieldType
case class BoolType() extends FieldType
case class IntType() extends FieldType // mathematical integer





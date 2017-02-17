package ProtobufGen

import java.io.File

/**
  * Created by mcoblenz on 2/17/17.
  */

abstract class FieldType
case class BoolType() extends FieldType
case class IntType() extends FieldType // mathematical integer

class ProtobufField (
    val fieldType: FieldType,
    val fieldName: String
)

class ProtobufMessage (
    val fields: List[ProtobufField]
)

class Protobuf (var messages: List[ProtobufMessage]) {
    def build(outputDir : File): Unit = {
    }
}

package edu.cmu.cs.obsidian.protobuf

import java.io.File

import edu.cmu.cs.obsidian.parser._

class Unimplemented extends Exception {}

/**
  * Created by mcoblenz on 2/17/17.
  *
  * Unlike the JCodeModel class hierarchy, this class does code generation in a functional style.
  */
object ProtobufGen {

    // Programs translate to lists of messages (one per contract).
    def translateProgram(program: Program): Protobuf = {
        val protobuf = new Protobuf(Nil)

        val messages = program.contracts.map(translateContract)

        new Protobuf(messages)
    }


    // Contracts translate to messages.
    private def translateContract(aContract: Contract): ProtobufMessage = {
        // We only care about the fields. The actual code is irrelevant.
        val allDeclarations = aContract.declarations.map(translateDeclaration)

        val stateNames: List[String] = aContract.declarations.foldRight(Nil: List[String])((decl, states) => decl match
            {
                case State(name, _) => name::states
                case _ => states
            }
        )

        val decls: List[ProtobufDeclaration] = allDeclarations.foldRight(List[ProtobufDeclaration]())((optionField, accum) =>
            optionField match {
                case None => accum
                case Some(f: ProtobufDeclaration) => f :: accum
            }
        )

        if (stateNames.length > 0) {
            val oneOfOptions = stateNames.map((stateName: String) =>
                (ObjectType(stateName), "state" + stateName))
            val stateDecl = ProtobufOneOf("state", oneOfOptions)

            new ProtobufMessage(stateDecl::decls, aContract.name)
        }
        else {
            new ProtobufMessage(decls, aContract.name)
        }
    }

    private def translateDeclaration(declaration: Declaration): Option[ProtobufDeclaration] = {
        declaration match {
            case c@Constructor(_, _, _) => None // TODO
            case c@Contract(modifiers, name, decls) => Some(translateContract(c))
            case t@TypeDecl(_, _) => None // TODO
            case f@edu.cmu.cs.obsidian.parser.Field(_, _) => Some(translateFieldDecl(f))
            case f@Func(_,_,_, _) => None
            case t@Transaction(_,_,_,_) => None
            case s@State(_, _) => Some(translateStateDecl(s))
        }
    }

    private def translateFieldDecl(f: edu.cmu.cs.obsidian.parser.Field): ProtobufDeclaration = {
        f.typ match {
            case i@edu.cmu.cs.obsidian.parser.IntType() => ProtobufField(edu.cmu.cs.obsidian.protobuf.IntType(), f.fieldName)
            case b@edu.cmu.cs.obsidian.parser.BoolType() => ProtobufField(edu.cmu.cs.obsidian.protobuf.BoolType(), f.fieldName)
            case s@edu.cmu.cs.obsidian.parser.StringType() => ProtobufField(edu.cmu.cs.obsidian.protobuf.StringType(), f.fieldName)
            case n@edu.cmu.cs.obsidian.parser.NonPrimitiveType(modifiers, typeName) =>
                ProtobufField(edu.cmu.cs.obsidian.protobuf.ObjectType(typeName), f.fieldName)
        }
    }

    private def translateStateDecl(s: State): ProtobufDeclaration = {
        // We only care about the fields. The actual code is irrelevant.
        val allFields = s.declarations.map(translateDeclaration)

        val fields: List[ProtobufDeclaration] = allFields.foldLeft(List[ProtobufDeclaration]())((accum, optionField) =>
            optionField match {
                case None => accum
                case Some(f: ProtobufDeclaration) => f :: accum
            }
        )

        new ProtobufMessage(fields, s.name)
    }
}

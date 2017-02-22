package edu.cmu.cs.obsidian.protobufgen

import java.io.File

import edu.cmu.cs.obsidian.parser._

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
    private def translateContract (aContract: Contract): ProtobufMessage = {
        // We only care about the fields. The actual code is irrelevant.
        val allDeclarations = aContract.declarations.map(translateDeclaration)

        val decls : List[ProtobufDeclaration] = allDeclarations.foldLeft(List[ProtobufDeclaration]())((accum, optionField) =>
            optionField match {case None => accum
                               case Some(f : ProtobufDeclaration) => f :: accum
                              }
            )

        new ProtobufMessage(decls)
    }

    private def translateDeclaration(declaration: Declaration) : Option[ProtobufDeclaration] = {
        declaration match {
            case c@Constructor(_,_,_) => None // TODO
            case t@TypeDecl(_,_) => None // TODO
            case f@edu.cmu.cs.obsidian.parser.Field(_,_) => Some (translateFieldDecl(f))
            case f@Func(_,_,_) => None
            case t@Transaction(_,_,_) => None
            case s@State(_,_) => Some (translateStateDecl(s))
        }
    }

    private def translateFieldDecl(f : edu.cmu.cs.obsidian.parser.Field) : ProtobufDeclaration = {
        f.typ match {
            // TODO; BoolType here is bogus.
            case _ => new ProtobufField(BoolType(), f.fieldName)
        }

    }

    private def translateStateDecl(s: State) : ProtobufDeclaration = {
        // We only care about the fields. The actual code is irrelevant.
        val allFields = s.declarations.map(translateDeclaration)

        val fields : List[ProtobufDeclaration] = allFields.foldLeft(List[ProtobufDeclaration]())((accum, optionField) =>
            optionField match {case None => accum
            case Some(f : ProtobufDeclaration) => f :: accum
            }
        )

        new ProtobufMessage(fields)
    }

/*

    private def resolveType(name: String): JType = {
        name match {
            case "ether" => model.ref("Ether")
            case "int" => model.ref("BigInteger")
            case other => model.ref(other)
        }
    }
}

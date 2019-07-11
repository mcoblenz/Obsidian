package edu.cmu.cs.obsidian.protobuf

import java.io.File

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.util.Util
import edu.cmu.cs.obsidian.typecheck._

class Unimplemented extends Exception {}

/**
  * Created by mcoblenz on 2/17/17.
  *
  * Unlike the JCodeModel class hierarchy, this class does code generation in a functional style.
  */
object ProtobufGen {
    // Programs translate to lists of messages (one per contract).
    def translateProgram(program: Program, sourceFilename: String): Seq[(Protobuf, String)] = {
        val protobuf = new Protobuf(Nil)
        assert(program.imports.isEmpty, "Imports should be empty after processing.")

        val messages = program.contracts.map(translateContract)

        val result: Seq[(Protobuf, String)] = Seq((new Protobuf(messages), sourceFilename))
        result
    }


    def genericParams(aContract: Contract): List[ProtobufDeclaration] =
        // TODO GENERIC: factor out this string constant
        aContract.params.map(p => ProtobufField(edu.cmu.cs.obsidian.protobuf.StringType(), "__generic" + p.gVar.varName)).toList

    // Contracts translate to messages.
    private def translateContract(aContract: Contract): ProtobufDeclaration = {
        // We only care about the fields. The actual code is irrelevant.
        val allDeclarations = aContract.declarations.map(translateDeclaration)

        val stateNames: List[String] = aContract.declarations.foldRight(Nil: List[String])((decl, states) => decl match
            {
                case State(name, _, _) => name::states
                case _ => states
            }
        )

        val decls: List[ProtobufDeclaration] = allDeclarations.foldRight(List[ProtobufDeclaration]())((optionField, accum) =>
            optionField match {
                case None => accum
                case Some(f: ProtobufDeclaration) => f :: accum
            }
        )

        val declsWithGUID =
            ProtobufField(edu.cmu.cs.obsidian.protobuf.StringType(), "__guid") :: genericParams(aContract) ++ decls

        val contractMessage = if (stateNames.nonEmpty) {
            val oneOfOptions = stateNames.map((stateName: String) =>
                (ObjectType(stateName), "state" + stateName))
            val stateDecl = ProtobufOneOf("state", oneOfOptions)

            ProtobufMessage(stateDecl :: declsWithGUID, aContract.name)
        }
        else {
            ProtobufMessage(declsWithGUID, aContract.name)
        }

        val contractOrGUIDFields : List[(FieldType, String)] = List[(FieldType, String)]((ObjectType(aContract.name), "obj"),
                                                                                         (edu.cmu.cs.obsidian.protobuf.StringType(), "guid"))
        val contractOrGUIDMessage = new ProtobufMessage(Seq(new ProtobufOneOf("either", contractOrGUIDFields)),
                                                        aContract.name + "OrGUID")

        new ProtobufDeclarationPair(contractMessage, contractOrGUIDMessage)
    }

    private def translateDeclaration(declaration: Declaration): Option[ProtobufDeclaration] = {
        declaration match {
            case c: Constructor => None // TODO
            case c: ObsidianContractImpl => Some(translateContract(c))
            case c: JavaFFIContractImpl => None
            case t: TypeDecl => None // TODO
            case f: edu.cmu.cs.obsidian.parser.Field => Some(translateFieldDecl(f))
            case t: Transaction => None
            case s: State => Some(translateStateDecl(s))
        }
    }

    private def translateFieldDecl(f: edu.cmu.cs.obsidian.parser.Field): ProtobufDeclaration = {
        f.typ match {
            case i@edu.cmu.cs.obsidian.typecheck.IntType() => ProtobufField(edu.cmu.cs.obsidian.protobuf.IntType(), f.name)
            case b@edu.cmu.cs.obsidian.typecheck.BoolType() => ProtobufField(edu.cmu.cs.obsidian.protobuf.BoolType(), f.name)
            case s@edu.cmu.cs.obsidian.typecheck.StringType() => ProtobufField(edu.cmu.cs.obsidian.protobuf.StringType(), f.name)
                // TODO: get the right type for the state if this is type specifies typestate?
            case np: NonPrimitiveType =>
                ProtobufField(edu.cmu.cs.obsidian.protobuf.StringType(), f.name);
            case BottomType() => assert(false, "Bottom type should not occur at codegen time"); ProtobufField(edu.cmu.cs.obsidian.protobuf.BoolType(), "bogus")
            case UnitType() => assert(false, "Fields should not be of unit type."); ProtobufField(edu.cmu.cs.obsidian.protobuf.BoolType(), "bogus")
        }
    }

    private def translateStateDecl(s: State): ProtobufDeclaration = {
        // We only care about the fields. The actual code is irrelevant.
        val allFields = s.fields.map(translateDeclaration)

        val fields: List[ProtobufDeclaration] = allFields.foldLeft(List[ProtobufDeclaration]())((accum, optionField) =>
            optionField match {
                case None => accum
                case Some(f: ProtobufDeclaration) => f :: accum
            }
        )

        new ProtobufMessage(fields, s.name)
    }
}

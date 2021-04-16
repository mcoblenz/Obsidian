package edu.cmu.cs.obsidian.codegen

import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.codegen.Util._

import java.io.{FileReader, StringWriter}


// reminder: use abstract class if want to create a base class that requires constructor arguments
sealed trait YulAST

object LiteralKind extends Enumeration {
    type LiteralKind = Value
    val number: codegen.LiteralKind.Value = Value("int")
    val boolean: codegen.LiteralKind.Value = Value("bool")
    val string: codegen.LiteralKind.Value = Value("string")
}

trait Expression extends YulAST

trait YulStatement extends YulAST

// for each asm struct, create a case class
case class TypedName(name: String, ntype: String) extends YulAST

case class Case(value: Literal, body: Block) extends YulAST {
    override def toString: String = {
        s"case ${value.toString} ${brace(body.toString)}"
    }
}

case class Literal(kind: LiteralKind.LiteralKind, value: String, vtype: String) extends Expression {
    override def toString: String = {
        /* todo/iev: i'm not positive if these assertions are the best idea, but they might be
           a nice seatbelt. the constants i'm checking against may be wrong or incomplete right now,
           this needs to be tested. add more complex assertions?
        */
        val msg: String = "internal error: literal with inconsistent type string"
        kind match {
            case edu.cmu.cs.obsidian.codegen.LiteralKind.number => assert(vtype == "int", msg)
            case edu.cmu.cs.obsidian.codegen.LiteralKind.boolean => assert(vtype == "bool", msg)
            case edu.cmu.cs.obsidian.codegen.LiteralKind.string => assert(vtype == "string", msg)
        }
        // from the spec, "Unless it is the default type, the type of a literal has to be specified
        // after a colon", which seems like this should be what we want to do:
        //
        // s"${value}:${mapObsTypeToABI(vtype)}"
        //
        // however, as of 12 April 2021, this produces a ton of warnings from solc about "user
        // defined types are not yet supported"
        value
    }
}

case class Identifier(name: String) extends Expression {
    override def toString: String = {
        name
    }
}

case class FunctionCall(functionName: Identifier, arguments: Seq[Expression]) extends Expression {
    override def toString: String = {
        s"${functionName.toString}${paren(arguments.map(id => id.toString).mkString(", "))}"
    }
}

case class Assignment(variableNames: Seq[Identifier], value: Expression) extends YulStatement {
    override def toString: String = {
        s"let ${variableNames.map(id => id.name).mkString(", ")} := ${value.toString}"
    }
}

case class VariableDeclaration(variables: Seq[TypedName]) extends YulStatement {
    override def toString: String = {
        s"let ${variables.map(id => id.name + ":" + id.ntype).mkString(", ")}"
    }
}

case class FunctionDefinition(
                                 name: String,
                                 parameters: Seq[TypedName],
                                 returnVariables: Seq[TypedName],
                                 body: Block) extends YulStatement {
    override def toString: String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/function.mustache"), "function")
        val scope = new FuncScope(this)
        mustache.execute(new StringWriter(), scope).toString
    }
}

case class If(condition: Expression, body: Block) extends YulStatement {
    override def toString: String = {
        s"if ${condition.toString} ${brace(body.toString)}"
    }
}

case class Switch(expression: Expression, cases: Seq[Case]) extends YulStatement {
    override def toString: String = {
        s"switch ${expression.toString}" + "\n" + (if (cases.isEmpty) brace("") else cases.map(c => c.toString).mkString("\n")) + "\n"
    }
}

case class ForLoop(pre: Block, condition: Expression, post: Block, body: Block) extends YulStatement {
    override def toString: String = {
        s"for ${brace(pre.toString)} ${condition.toString} ${brace(post.toString)}" + "\n" +
            brace(body.toString)
    }
}

case class Break() extends YulStatement {
    override def toString: String = "break"
}

case class Continue() extends YulStatement {
    override def toString: String = "continue"
}

case class Leave() extends YulStatement {
    override def toString: String = "leave"
}

case class ExpressionStatement(expression: Expression) extends YulStatement {
    override def toString: String = {
        expression.toString
    }
}

case class Block(statements: Seq[YulStatement]) extends YulStatement {
    override def toString: String = {
        statements.map(s => s.toString).mkString(" ")
    }
}


/*
    Object = 'object' StringLiteral '{' Code ( Object | Data )* '}'
    Code = 'code' Block
    Data = 'data' StringLiteral ( HexLiteral | StringLiteral )
    HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')
    StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'
 */
case class Code(block: Block) extends YulAST

case class Data(name: String, hex: Seq[HexLiteral], str: Seq[StringLiteral]) extends YulAST

case class HexLiteral(content: String) extends YulAST

case class StringLiteral(content: String) extends YulAST

case class YulObject(name: String, code: Code, subObjects: Seq[YulObject], data: Seq[Data]) extends YulAST {
    def yulString(): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"), "example")
        val scope = new ObjScope(this)
        val raw: String = mustache.execute(new StringWriter(), scope).toString
        // mustache butchers certain characters; this fixes that. there may be other characters that
        // need to be added here
        raw.replaceAll("&amp;", "&").
            replaceAll("&gt;", ">").
            replaceAll("&#10;", "\n").
            replaceAll("&#61;", "=")
    }

    // ObjScope and FuncScope are designed to facilitate mustache templates, with the following rules
    // - Each val is a tag in the template.
    // - If you need to fill in a section with non-empty list, define a function (which takes no args)
    //   named by the section tag, let it return an array of object, the object name does not matter,
    //   but the it must contain a val named by the tag inside the section tag (the best way to understand
    //   this is to track down an example)
    // temporary function, not designed for a full recursive walk through of the object
    class ObjScope(obj: YulObject) {

        val mainContractName: String = obj.name
        val creationObject: String = mainContractName
        val runtimeObject: String = mainContractName + "_deployed"
        var runtimeFunctionArray: Array[Func] = Array[Func]()
        var deployFunctionArray: Array[Func] = Array[Func]()
        var dispatch = false
        var dispatchArray: Array[codegen.Case] = Array[codegen.Case]()
        var deployCall: Array[Call] = Array[Call]()
        var memoryInitRuntime: String = ""

        def dispatchEntry(f: FunctionDefinition): Seq[YulStatement] = {
            Seq(
                //    if callvalue() { revert(0, 0) }
                callvaluecheck,
                // abi_decode_tuple_(4, calldatasize())
                codegen.ExpressionStatement(FunctionCall(Identifier("abi_decode_tuple"), Seq(ilit(4), FunctionCall(Identifier("calldatasize"), Seq())))),
                //    fun_retrieve_24()
                codegen.ExpressionStatement(FunctionCall(Identifier(functionRename(f.name)), f.parameters.map(p => Identifier(p.name)))), //todo: second argument is highly speculative
                //    let memPos := allocate_memory(0)
                codegen.Assignment(Seq(Identifier("memPos")), FunctionCall(Identifier("allocate_memory"), Seq(ilit(0)))),
                //    let memEnd := abi_encode_tuple__to__fromStack(memPos)
                codegen.Assignment(Seq(Identifier("memEnd")), FunctionCall(Identifier("abi_encode_tuple_to_fromStack"), Seq(Identifier("memPos")))),
                //    return(memPos, sub(memEnd, memPos))
                codegen.ExpressionStatement(FunctionCall(Identifier("return"), Seq(Identifier("memPos"), FunctionCall(Identifier("sub"), Seq(Identifier("memEnd"), Identifier("memPos"))))))
            )
        }

        def deploy(): Array[Call] = deployCall

        def deployFunctions(): Array[Func] = deployFunctionArray

        for (s <- obj.code.block.statements) {
            s match {
                case f: FunctionDefinition =>
                    deployFunctionArray = deployFunctionArray :+ new Func(f.toString)
                case e: ExpressionStatement =>
                    e.expression match {
                        case f: FunctionCall => deployCall = deployCall :+ new Call(f.toString)
                        case _ =>
                            assert(assertion = false, "TODO: objscope not implemented for expression statement " + e.toString)
                            () // TODO unimplemented
                    }
                case _ =>
                    assert(assertion = false, "TODO: objscope not implemented for block statement " + s.toString)
                    () // TODO unimplemented
            }
        }

        def runtimeFunctions(): Array[Func] = runtimeFunctionArray

        for (sub <- obj.subObjects) { // TODO separate runtime object out as a module (make it verbose)
            for (s <- sub.code.block.statements) { // temporary fix due to issue above
                s match {
                    case f: FunctionDefinition =>
                        dispatch = true
                        runtimeFunctionArray = runtimeFunctionArray :+ new Func(f.toString)
                        dispatchArray = dispatchArray :+ codegen.Case(hexlit(hashOfFunctionDef(f)), Block(dispatchEntry(f)))
                    case e: ExpressionStatement =>
                        e.expression match {
                            case f: FunctionCall => memoryInitRuntime = f.toString
                            case _ =>
                                assert(assertion = false, "TODO: " + e.toString())
                                () // TODO unimplemented
                        }
                    case x =>
                        assert(assertion = false, s"subobject case for ${x.toString} unimplemented")
                        ()
                }
            }
        }

        def dispatchCase(): codegen.Switch = codegen.Switch(Identifier("selector"), dispatchArray.toSeq)

        def defaultReturn(): FunctionCall = FunctionCall(Identifier("return"), Seq(ilit(0), ilit(0)))

        class Func(val code: String) {}

        class Case(val hash: String) {}

        class Call(val call: String) {}
    }

}

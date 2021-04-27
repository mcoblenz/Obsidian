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
            case LiteralKind.number => assert(vtype == "int", msg)
            case LiteralKind.boolean => assert(vtype == "bool", msg)
            case LiteralKind.string => assert(vtype == "string", msg)
        }
        // from the spec, "Unless it is the default type, the type of a literal has to be specified
        // after a colon", which seems like this should be what we want to do:
        //
        // s"${value}:${mapObsTypeToABI(vtype)}"
        //
        // however, as of 12 April 2021, this produces a ton of warnings from solc about "user
        // defined types are not yet supported"
        kind match {
            case edu.cmu.cs.obsidian.codegen.LiteralKind.number => value
            case edu.cmu.cs.obsidian.codegen.LiteralKind.boolean => value
            case edu.cmu.cs.obsidian.codegen.LiteralKind.string => quote(value)
        }
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
        s"${variableNames.map(id => id.name).mkString(", ")} := ${value.toString}"
    }
}

/**
  * This class represents variable declarations in Yul. These take the form of one or more names,
  * optionally paired with types, and 0 or 1 expressions to which they should be bound. Some examples
  * are and how they would be represented by arguments to this class are shown informally below:
  * let x := 7                      ~~> <(x,none)>,some(7)
  * let x := add(y,3)               ~~> <(x,none)>,some(add(y,3))
  * // here, since x has no type it's assumed to be u256 and gets assigned 0
  * let x                           ~~> <(x,none)>,none
  * let v:uint32 := 0 : uint32      ~~> <(x,some(uint32)>,some(0)
  * let x,y = g()                   ~~> <(x,none),(y,none)>,some(g())
  * let v:uint256, t := f()         ~~> <(v,some(uint256),(t,none)>,some(f())
  *
  * @param variables the sequence of variables, paired with their optional types, to declare
  * @param value     the optional expression to which to bind the variables declared
  */
case class VariableDeclaration(variables: Seq[(Identifier, Option[String])], value: Option[Expression]) extends YulStatement {
    override def toString: String = {
        s"let ${
            variables.map(v => v._1.name + (v._2 match {
                case Some(t) => s" : $t"
                case None => ""
            })).mkString(", ")
        }" +
            (value match {
                case Some(e) => s" := ${e.toString}"
                case None => ""
            })
    }
}

case class FunctionDefinition(name: String,
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
        s"switch ${expression.toString}" + "\n" +
            (if (cases.isEmpty) brace("") else cases.map(c => c.toString).mkString("\n")) +
            "\n"
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

case class YulObject(name: String, code: Code, subobjects: Seq[YulObject], data: Seq[Data]) extends YulAST {
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
            replaceAll("&#61;", "=").
            replaceAll("&quot;", "\"")
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

        val freeMemPointer = 64 // 0x40: currently allocated memory size (aka. free memory pointer)
        val firstFreeMem = 128 //  0x80: first byte in memory not reserved for special usages
        // the free memory pointer points to 0x80 initially

        var memoryInitRuntime: Expression = ap("mstore", ilit(freeMemPointer), ilit(firstFreeMem))

        var memoryInit: Expression = memoryInitRuntime

        def callValueCheck(): YulStatement = callvaluecheck

        var deRetCnt = 0

        def nextDeRet(): String = {
            deRetCnt = deRetCnt + 1
            s"_dd_ret_${deRetCnt.toString}" //todo: better naming convention?
        }

        def dispatchEntry(f: FunctionDefinition): Seq[YulStatement] = {
            val temps: Seq[Identifier] = f.returnVariables.map(_ => Identifier(nextDeRet()))

            //todo: second argument to the application is highly speculative; check once you have more complex functions
            val call_to_f: Expression = ap(functionRename(f.name), f.parameters.map(p => Identifier(p.name)): _*)
            val call_f_and_maybe_assign: YulStatement =
                if (f.returnVariables.nonEmpty) {
                    codegen.Assignment(temps, call_to_f)
                } else {
                    ExpressionStatement(call_to_f)
                }

            val memEnd =
                if(f.returnVariables.nonEmpty) {
                    Identifier(memEnd)
                } else {
                    // todo this might not be right, i'm not sure how things get allocated on the stack
                    temps.last
                }

            Seq(
                //    if callvalue() { revert(0, 0) }
                callvaluecheck,
                // abi_decode_tuple_(4, calldatasize())
                codegen.ExpressionStatement(ap("abi_decode_tuple", ilit(4), ap("calldatasize"))),
                //    fun_retrieve_24()
                call_f_and_maybe_assign,
                //    let memPos := allocate_memory(0)
                assign1(Identifier("memPos"), ap("allocate_memory", ilit(0))),
                //    let memEnd := abi_encode_tuple__to__fromStack(memPos) c.f.
                //    let memEnd := abi_encode_tuple_t_uint256__to_t_uint256__fromStack(memPos , ret_0) // TODO iev picking up here
                assign1(Identifier("memEnd"), ap("abi_encode_tuple_to_fromStack", Identifier("memPos"))),
                //    return(memPos, sub(memEnd, memPos))
                codegen.ExpressionStatement(ap("return", Identifier("memPos"), ap("sub", Identifier("memEnd"), Identifier("memPos"))))
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

        for (sub <- obj.subobjects) { // TODO separate runtime object out as a module (make it verbose)
            for (s <- sub.code.block.statements) { // temporary fix due to issue above
                s match {
                    case f: FunctionDefinition =>
                        dispatch = true
                        runtimeFunctionArray = runtimeFunctionArray :+ new Func(f.toString)
                        dispatchArray = dispatchArray :+ codegen.Case(hexlit(hashOfFunctionDef(f)), Block(dispatchEntry(f)))
                    case e: ExpressionStatement =>
                        e.expression match {
                            case _: FunctionCall =>
                                //TODO what was this line doing?
                                //memoryInitRuntime = f.toString
                                ()
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

        val datasize: Expression = ap("datasize", stringlit(runtimeObject))

        def codeCopy(): Expression = ap("codecopy", ilit(0), ap("dataoffset", stringlit(runtimeObject)), datasize)

        def defaultReturn(): Expression = ap("return", ilit(0), datasize)

        class Func(val code: String) {}

        class Case(val hash: String) {}

        class Call(val call: String) {}
    }

}

package edu.cmu.cs.obsidian.codegen

import java.io.{FileReader, StringWriter}
import com.github.mustachejava.Mustache
import com.github.mustachejava.MustacheFactory
import com.github.mustachejava.DefaultMustacheFactory

// reminder: use abstract class if want to create a base class that requires constructor arguments
sealed trait YulAST

object LiteralKind extends Enumeration {
    type LiteralKind = Value
    val number, boolean, string = Value
}
trait Expression extends YulAST
trait YulStatement extends YulAST


// for each asm struct, create a case class
case class TypedName (name: String, ntype: String) extends YulAST
case class Case (value: Literal, body: Block) extends YulAST

case class Literal (kind: LiteralKind.LiteralKind, value: String, vtype: String) extends Expression
case class Identifier (name: String) extends Expression
case class FunctionCall (functionName: Identifier, arguments: Seq[Expression]) extends Expression {
    def yulFunctionCallString(): String = {
        var code = this.functionName.name+"("
        var isFirst = true
        for (arg <- this.arguments){
            val argStr =
                arg match {
                    case Literal(_,value, _)=> value
                    case _ => ""
                }
            if (isFirst){
                code = code + argStr
                isFirst = false
            }
            else {
                code = code + "," + argStr
            }
        }
        code + ")" + "\n"
    }
}

case class Assignment (variableNames: Seq[Identifier], value: Expression) extends YulStatement
case class VariableDeclaration (variables: Seq[TypedName]) extends YulStatement
case class FunctionDefinition (
                                  name: String,
                                  parameters: Seq[TypedName],
                                  returnVariables: Seq[TypedName],
                                  body: Block) extends YulStatement {
    def yulFunctionDefString(): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/function.mustache"),"function")
        val scope = new FuncScope(this)
        mustache.execute(new StringWriter(), scope).toString()
    }
}
case class If (condition: Expression, body: Block) extends YulStatement
case class Switch (expression: Expression, cases: Seq[Case]) extends YulStatement
case class ForLoop (pre: Block, condition: Expression, post: Block, body: Block) extends YulStatement
case class Break () extends YulStatement
case class Continue () extends YulStatement
case class Leave () extends YulStatement
case class ExpressionStatement (expression: Expression) extends YulStatement
case class Block (statements: Seq[YulStatement]) extends YulStatement


/*
    Object = 'object' StringLiteral '{' Code ( Object | Data )* '}'
    Code = 'code' Block
    Data = 'data' StringLiteral ( HexLiteral | StringLiteral )
    HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')
    StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'
 */
case class Code (block: Block) extends YulAST
case class Data (name: String, hex: Seq[HexLiteral], str: Seq[StringLiteral]) extends YulAST
case class HexLiteral (content: String) extends YulAST
case class StringLiteral (content: String) extends YulAST
case class YulObject (name: String, code: Code, subObjects: Seq[YulObject], data: Seq[Data]) extends YulAST {
    def yulString(): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"),"example")
        val scope = new ObjScope(this)
        val raw: String = mustache.execute(new StringWriter(), scope).toString()
        raw.replaceAll("&amp;","&").replaceAll("&gt;",">").replaceAll("&#10;", "\n")
    }

    // ObjScope and FuncScope are designed to facilitate mustache templates, with the following rules
    // - Each val is a tag in the template.
    // - If you need to fill in a section with non-empty list, define a function (which takes no args)
    //   named by the section tag, let it return an array of object, the object name does not matter,
    //   but the it must contain a val named by the tag inside the section tag (the best way to understand
    //   this is to track down an example)
    // temporary function, not designed for a full recursive walk through of the object
    class ObjScope(obj: YulObject) {
        class Func(val code: String){}
        class Case(val hash: String){}
        class Call(val call: String){}

        // TODO unimplemented; hardcode to uint256 for now
        def mapObsTypeToABI(ntype: String): String = {
            "uint256"
        }

        // TODO unimplemented; hardcode for now; bouncycastle library maybe helpful
        def keccak256(s: String): String = {
            "0x70a08231"
        }

        def hashFunction(f: FunctionDefinition): String = {
            var strRep: String = f.name + "("
            for (p <- f.parameters){
                strRep = strRep + mapObsTypeToABI(p.ntype)
            }
            strRep = strRep + ")"
            keccak256(strRep)
            // TODO truncate and keep the first 4 bytes
        }

        val mainContractName: String = obj.name
        val creationObject: String = mainContractName
        val runtimeObject: String = mainContractName + "_deployed"
        var runtimeFunctionArray: Array[Func] = Array[Func]()
        var deployFunctionArray: Array[Func] = Array[Func]()
        var dispatch = false
        var dispatchArray: Array[Case] = Array[Case]()
        var deployCall: Array[Call] = Array[Call]()
        var memoryInitRuntime: String = ""

        for (s <- obj.code.block.statements) {
            s match {
                case f: FunctionDefinition => deployFunctionArray = deployFunctionArray :+ new Func(f.yulFunctionDefString())
                case e: ExpressionStatement =>
                    e.expression match {
                        case f: FunctionCall => deployCall = deployCall :+ new Call(f.yulFunctionCallString())
                        case _ =>
                            assert(false, "TODO")
                            () // TODO unimplemented
                    }
                case _ =>
                    assert(false, "TODO")
                    () // TODO unimplemented
            }
        }

        for (sub <- obj.subObjects) { // TODO separate runtime object out as a module (make it verbose)
            for (s <- sub.code.block.statements) { // temporary fix due to issue above
                s match {
                    case f: FunctionDefinition => {
                        dispatch = true
                        val code = f.yulFunctionDefString()
                        runtimeFunctionArray = runtimeFunctionArray :+ new Func(code)
                        dispatchArray = dispatchArray :+ new Case(hashFunction(f))
                    }
                    case e: ExpressionStatement =>
                        e.expression match {
                            case f: FunctionCall => memoryInitRuntime = f.yulFunctionCallString()
                            case _ =>
                                assert(false, "TODO")
                                () // TODO unimplemented
                        }
                    case _ => ()
                }
            }
        }
        def deploy(): Array[Call] = deployCall
        def deployFunctions(): Array[Func] = deployFunctionArray
        def runtimeFunctions(): Array[Func] = runtimeFunctionArray
        def dispatchCase(): Array[Case] = dispatchArray

    }

    // TODO need to fix indentation of the output
    class FuncScope(f: FunctionDefinition) {
        class Param(val name: String){}
        class Body(val code: String){}

        val functionName: String = f.name
        val arg0: String = if (f.parameters.nonEmpty) {f.parameters.head.name} else {""}
        var argRest: Array[Param] = Array[Param]()
        if (f.parameters.length > 1){
            var first  = true
            for (p <- f.parameters){
                if (first) {
                    first = false
                }
                else {
                    argRest = argRest :+ new Param(p.name)
                }
            }
        }
        // construct body
        var codeBody: Array[Body] = Array[Body]()
        for (s <- f.body.statements){
            s match {
                case ExpressionStatement(e) =>
                    e match {
                        case func: FunctionCall =>
                            codeBody = codeBody :+ new Body(func.yulFunctionCallString())
                    }
                case _ => ()
            }
        }
        // TODO assume only one return variable for now
        var hasRetVal = false
        var retParams = ""
        if (f.returnVariables.nonEmpty){
            hasRetVal = true
            retParams = f.returnVariables.apply(0).name
        }
        def params(): Array[Param] = argRest
        def body(): Array[Body] = codeBody
    }
}





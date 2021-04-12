package edu.cmu.cs.obsidian.codegen

import java.io.{FileReader, StringWriter}
import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.codegen.Util._

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
case class TypedName (name: String, ntype: String) extends YulAST
case class Case (value: Literal, body: Block) extends YulAST {
    override def toString: String = {
        s"case ${value.toString} ${brace(body.toString)}"
    }
}

case class Literal (kind: LiteralKind.LiteralKind, value: String, vtype: String) extends Expression {
    override def toString: String = {
        /* todo/iev: i'm not positive if these assertions are the best idea, but they might be
           a nice seatbelt. the constants i'm checking against may be wrong or incomplete right now,
           this needs to be tested. add more complex assertions?
        */
        val msg: String = "internal error: literal with inconsistent type string"
        kind match {
            case edu.cmu.cs.obsidian.codegen.LiteralKind.number => assert(vtype=="int",msg)
            case edu.cmu.cs.obsidian.codegen.LiteralKind.boolean => assert(vtype=="bool",msg)
            case edu.cmu.cs.obsidian.codegen.LiteralKind.string => assert(vtype=="string",msg)
        }
        // from the spec, "Unless it is the default type, the type of a literal has to be specified after a colon"
        s"${value}:${vtype}"
    }
}
case class Identifier (name: String) extends Expression {
    override def toString: String = {
        name
    }
}

case class FunctionCall (functionName: Identifier, arguments: Seq[Expression]) extends Expression {
    override def toString: String = {
        //iev: this assert replicates previous behaviour, but i'm not sure if that was right
        assert(arguments.exists(arg => arg match { case Literal(_,_,_) => true case _ => false }),
                "internal error: function call with non-literal argument")
        s"${functionName.toString} ${paren(arguments.map(id=>id.toString).mkString(", "))}"
    }
}

case class Assignment (variableNames: Seq[Identifier], value: Expression) extends YulStatement {
    override def toString: String = {
        s"let ${variableNames.map(id => id.name).mkString(", ")} := ${value.toString}"
    }
}
case class VariableDeclaration (variables: Seq[TypedName]) extends YulStatement {
    override def toString: String = {
        s"let ${variables.map(id => id.name+":"+id.ntype).mkString(", ")}"
    }
}
case class FunctionDefinition (
                                  name: String,
                                  parameters: Seq[TypedName],
                                  returnVariables: Seq[TypedName],
                                  body: Block) extends YulStatement {
    override def toString: String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/function.mustache"),"function")
        val scope = new FuncScope(this)
        mustache.execute(new StringWriter(), scope).toString
    }
}

case class If (condition: Expression, body: Block) extends YulStatement{
    override def toString: String = {
        s"if ${condition.toString} ${brace(body.toString)}"
    }
}

case class Switch (expression: Expression, cases: Seq[Case]) extends YulStatement{
    override def toString: String = {
        s"switch ${expression.toString}" + "\n" + cases.map(c => c.toString).mkString("\n") + "\n"
    }
}

case class ForLoop (pre: Block, condition: Expression, post: Block, body: Block) extends YulStatement {
    override def toString: String = {
        s"for ${brace(pre.toString)} ${condition.toString} ${brace(post.toString)}" + "\n" +
          brace(body.toString)
    }
}
case class Break () extends YulStatement {
    override def toString: String = "break"
}
case class Continue () extends YulStatement {
    override def toString: String = "continue"
}
case class Leave () extends YulStatement {
    override def toString: String = "leave"
}
case class ExpressionStatement (expression: Expression) extends YulStatement{
    override def toString: String = {
        expression.toString
    }
}
case class Block (statements: Seq[YulStatement]) extends YulStatement {
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
case class Code (block: Block) extends YulAST
case class Data (name: String, hex: Seq[HexLiteral], str: Seq[StringLiteral]) extends YulAST
case class HexLiteral (content: String) extends YulAST
case class StringLiteral (content: String) extends YulAST
case class YulObject (name: String, code: Code, subObjects: Seq[YulObject], data: Seq[Data]) extends YulAST {
    def yulString(): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"),"example")
        val scope = new ObjScope(this)
        val raw: String = mustache.execute(new StringWriter(), scope).toString
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

        for (sub <- obj.subObjects) { // TODO separate runtime object out as a module (make it verbose)
            for (s <- sub.code.block.statements) { // temporary fix due to issue above
                s match {
                    case f: FunctionDefinition =>
                        dispatch = true
                        runtimeFunctionArray = runtimeFunctionArray :+ new Func(f.toString)
                        dispatchArray = dispatchArray :+ new Case(hashOfFunctionDef(f))
                    case e: ExpressionStatement =>
                        e.expression match {
                            case f: FunctionCall => memoryInitRuntime = f.toString
                            case _ =>
                                assert(assertion = false, "TODO: " + e.toString())
                                () // TODO unimplemented
                        }
                    case _ =>
                        assert(false)
                        ()
                }
            }
        }
        def deploy(): Array[Call] = deployCall
        def deployFunctions(): Array[Func] = deployFunctionArray
        def runtimeFunctions(): Array[Func] = runtimeFunctionArray
        def dispatchCase(): Array[Case] = dispatchArray
        def defaultReturn(): FunctionCall = FunctionCall(Identifier("return"),Seq(ilit(0),ilit(0)))
    }

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
        var codeBody: Array[Body] = f.body.statements.map(s => new Body(s.toString)).toArray

        // TODO assume only one return variable for now
        var hasRetVal = false
        var retParams = ""
        if (f.returnVariables.nonEmpty){
            hasRetVal = true
            retParams = f.returnVariables.head.name
        }
        def params(): Array[Param] = argRest
        def body(): Array[Body] = codeBody
    }
}

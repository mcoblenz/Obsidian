package edu.cmu.cs.obsidian.codegen

import java.io.{FileReader, StringWriter}
import com.github.mustachejava.Mustache
import com.github.mustachejava.MustacheFactory
import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.codegen.{FunctionCall, FunctionDefinition, Literal, YulObject}

object YulStringGenerator {

    def yulString(obj: YulObject): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"),"example")
        val scope = new ObjScope(obj)
        val raw: String = mustache.execute(new StringWriter(), scope).toString()
        raw.replaceAll("&amp;","&").replaceAll("&gt;",">").replaceAll("&#10;", "\n")
    }

    def yulFunctionDefString(f: FunctionDefinition): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/function.mustache"),"function")
        val scope = new FuncScope(f)
        mustache.execute(new StringWriter(), scope).toString()
    }

    def yulFunctionCallString(f: FunctionCall): String = {
        var code = f.functionName.name+"("
        var isFirst = true
        for (arg <- f.arguments){
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
            case f: FunctionDefinition => deployFunctionArray = deployFunctionArray :+ new Func(YulStringGenerator.yulFunctionDefString(f))
            case e: ExpressionStatement =>
                e.expression match {
                    case f: FunctionCall => deployCall = deployCall :+ new Call(YulStringGenerator.yulFunctionCallString(f))
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
                    val code = YulStringGenerator.yulFunctionDefString(f)
                    runtimeFunctionArray = runtimeFunctionArray :+ new Func(code)
                    dispatchArray = dispatchArray :+ new Case(hashFunction(f))
                }
                case e: ExpressionStatement =>
                    e.expression match {
                        case f: FunctionCall => memoryInitRuntime = YulStringGenerator.yulFunctionCallString(f)
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
                        codeBody = codeBody :+ new Body(YulStringGenerator.yulFunctionCallString(func))
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

package edu.cmu.cs.obsidian.codegen

import java.io.{FileReader, StringWriter}
import com.github.mustachejava.Mustache
import com.github.mustachejava.MustacheFactory
import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.codegen.{FuncScope, FunctionCall, FunctionDefinition, Literal, ObjScope, YulObject}

object yulString {

    // todo: does this code get called?
    def yulString(obj: YulObject): String = {
        assert(false)
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"),"example")
        val scope = new ObjScope(obj)
        val raw: String = mustache.execute(new StringWriter(), scope).toString()
        raw.replaceAll("&amp;","&").replaceAll("&gt;",">").replaceAll("&#10;", "\n")
    }

    // todo: does this code get called?
    def yulFunctionDefString(f: FunctionDefinition): String = {
        assert(false)
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/function.mustache"),"function")
        val scope = new FuncScope(f)
        mustache.execute(new StringWriter(), scope).toString()
    }

    // todo: does this code get called?
    def yulFunctionCallString(f: FunctionCall): String = {
        assert(false)
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
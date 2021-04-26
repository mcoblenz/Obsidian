package edu.cmu.cs.obsidian.codegen

class FuncScope(f: FunctionDefinition) {
    class Param(val name: String) {}

    class Body(val code: String) {}

    val functionName: String = f.name
    val (arg0, argRest) = f.parameters match {
        case hd +: tl => (hd, tl.map(p => new Param(p.name)).toArray)
        case _ => ("", Array[Param]())
    }

    var hasRetVal: Boolean = f.returnVariables.nonEmpty
    var retParams: String = f.returnVariables.map(v => v.name).mkString(", ")

    def params(): Array[Param] = argRest

    def body(): Array[Body] = f.body.statements.map(s => new Body(s.toString)).toArray
}

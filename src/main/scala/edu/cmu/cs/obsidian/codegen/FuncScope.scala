package edu.cmu.cs.obsidian.codegen

class FuncScope(f: FunctionDefinition) {
    class Param(val name: String){}
    class Body(val code: String){}

    val functionName: String = f.name
    val (arg0, argRest) = f.parameters match {
        case hd +: tl => (hd, tl.map(p => new Param(p.name)).toArray)
        case _ => ("", Array[Param]())
    }

    // TODO assume only one return variable for now
    var retParams: String = if (f.returnVariables.nonEmpty) { f.returnVariables.head.name } else { "" }
    def params(): Array[Param] = argRest
    def body(): Array[Body] = f.body.statements.map(s => new Body(s.toString)).toArray
}

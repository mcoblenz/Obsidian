package edu.cmu.cs.obsidian.codegen

class FuncScope(f: FunctionDefinition) {
    class Param(val name: String){}
    class Body(val code: String){}

    val functionName: String = f.name
    val arg0: String = if (f.parameters.nonEmpty) { f.parameters.head.name } else { "" }
    var argRest: Array[Param] =
        if (f.parameters.nonEmpty) {
            f.parameters.drop(1).map(p => new Param(p.name)).toArray
        } else {
            Array[Param]()
        }

    // TODO assume only one return variable for now
    var hasRetVal: Boolean = f.returnVariables.nonEmpty
    var retParams: String = if (hasRetVal) { f.returnVariables.head.name } else { "" }
    def params(): Array[Param] = argRest
    def body(): Array[Body] = f.body.statements.map(s => new Body(s.toString)).toArray
}

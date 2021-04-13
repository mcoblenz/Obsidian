package edu.cmu.cs.obsidian.codegen

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
    var codeBody : Array[Body] = f.body.statements.map(s => new Body(s.toString)).toArray

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


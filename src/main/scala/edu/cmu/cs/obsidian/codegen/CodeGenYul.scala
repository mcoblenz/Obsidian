package edu.cmu.cs.obsidian.codegen

import java.io.{File, FileReader, FileWriter, StringWriter}
import java.nio.file.{Files, Path, Paths}
import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.Main.{findMainContract, findMainContractName}
import edu.cmu.cs.obsidian.codegen.Code
import edu.cmu.cs.obsidian.codegen.LiteralKind.number
import edu.cmu.cs.obsidian.typecheck.ContractType

import scala.collection.immutable.Map

object CodeGenYul extends CodeGenerator {

    // TODO improve this temporary symbol table
    var tempSymbolTable: Map[String, Int] = Map() // map from field identifiers to index in storage
    var tempTableIdx = 0 // counter indicating the next available slot in the table
    var stateIdx = -1    // whether or not there is a state
    var stateEnumMapping: Map[String, Int] = Map() // map from state name to an enum value
    var stateEnumCounter = 0  // counter indicating the next value to assign since we don't knon the total num of states

    def gen(filename: String, srcDir: Path, outputPath: Path, protoDir: Path,
            options: CompilerOptions, checkedTable: SymbolTable, transformedTable: SymbolTable): Boolean = {
        // extract ast and find main contract
        val ast = checkedTable.ast
        val mainContractOption = findMainContract(ast)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val mainName = findMainContractName(ast)
        // prepare finalOutputPath
        val finalOutputPath = options.outputPath match {
            case Some(p) =>
                Paths.get(p).resolve(mainName)
            case None =>
                Paths.get(mainName)
        }
        // translate from obsidian AST to yul AST
        val translated_obj = translateProgram(ast)
        // generate yul string from yul AST
        val s = translated_obj.yulString()
        // write string to output file
        // currently it's created in the Obsidian directory; this may need to be changed, based on desired destination
        Files.createDirectories(finalOutputPath)
        val writer = new FileWriter(new File(finalOutputPath.toString(), translated_obj.name + ".yul"))
        writer.write(s)
        writer.flush()
        true
    }

    def translateProgram(program: Program): YulObject = {
        // translate main contract
        val mainContractOption = findMainContract(program)
        if (mainContractOption.isEmpty) {
            throw new RuntimeException("No main contract found")
        }
        val mainContract = mainContractOption.get
        val main_contract_ast = mainContract match {
            case obsContract: ObsidianContractImpl => translateContract(obsContract)
            case _ => throw new RuntimeException("Java contract not supported in yul translation")
        }

        // TODO ignore imports, data for now
        // translate other contracts (if any) and add them to the subObjects
        var new_subObjects: Seq[YulObject] = main_contract_ast.subObjects
        for (c <- program.contracts) {
            c match {
                case obsContract: ObsidianContractImpl =>
                    if (!c.modifiers.contains(IsMain())) { // if not main contract
                        // this is a top level contract object
                        // note: interfaces are not translated;
                        // TODO detect an extra contract named "Contract", skip that as a temporary fix
                        if (c.name != ContractType.topContractName) {
                            new_subObjects = main_contract_ast.subObjects :+ translateContract(obsContract)
                        }
                    }
                case _: JavaFFIContractImpl =>
                    throw new RuntimeException("Java contract not supported in yul translation")
            }
        }

        YulObject(main_contract_ast.name, main_contract_ast.code, new_subObjects, main_contract_ast.data)
    }

    def translateContract(contract: ObsidianContractImpl): YulObject = {
        var subObjects: Seq[YulObject] = Seq()
        var statement_seq_deploy: Seq[YulStatement] = Seq()
        var statement_seq_runtime: Seq[YulStatement] = Seq()

        // memory init
        val freeMemPointer = 64 // 0x40: currently allocated memory size (aka. free memory pointer)
        val firstFreeMem = 128 //  0x80: first byte in memory not reserved for special usages
        // the free memory pointer points to 0x80 initially
        val initExpr = FunctionCall(
            Identifier("mstore"),
            Seq(Literal(LiteralKind.number, freeMemPointer.toString(), "int"),Literal(LiteralKind.number, firstFreeMem.toString(), "int")))
        statement_seq_deploy = statement_seq_deploy :+ ExpressionStatement(initExpr)
        statement_seq_runtime = statement_seq_runtime :+ ExpressionStatement(initExpr)

        // callValueCheck: TODO unimplemented
        // it checks for the wei sent together with the current call and revert if that's non zero.
        // if callvalue() { revert(0, 0) }

        // translate declarations
        for (d <- contract.declarations) {
            val (deploy_seq, runtime_seq) = translateDeclaration(d)
            statement_seq_deploy = statement_seq_deploy ++ deploy_seq
            statement_seq_runtime = statement_seq_runtime ++ runtime_seq
        }

        // create runtime object
        val runtime_name = contract.name + "_deployed"
        val runtime_obj = YulObject(runtime_name, Code(Block(statement_seq_runtime)), Seq(), Seq())
        subObjects = runtime_obj +: subObjects

        YulObject(contract.name, Code(Block(statement_seq_deploy)), subObjects, Seq())
    }

    // return statements that go to deploy object, and statements that go to runtime object
    def translateDeclaration(declaration: Declaration): (Seq[YulStatement], Seq[YulStatement]) = {
        declaration match {
            case f: Field => (Seq(), translateField(f))
            case t: Transaction =>
                (Seq(), translateTransaction(t))
            case s: State =>
                (Seq(), translateState(s))
            case c: ObsidianContractImpl =>
                assert(false, "TODO")
                (Seq(), Seq())
            case c: JavaFFIContractImpl =>
                assert(assertion = false, "Java contracts not supported in Yul translation")
                (Seq(), Seq())
            case c: Constructor =>
                (translateConstructor(c), Seq())
            case t: TypeDecl =>
                assert(false, "TODO")
                (Seq(), Seq())
            // This should never be hit.
            case _ =>
                assert(false, "Translating unexpected declaration: " + declaration)
                (Seq(), Seq())
        }
    }

    def translateField(field: Field): Seq[YulStatement] = {
        // Reserve a slot in the storage by assigning a index in the symbol table
        // since field declaration has not yet be assigned, there is no need to do sstore
        tempSymbolTable += field.name -> tempTableIdx
        tempTableIdx += 1
        Seq()
    }

    def translateState(s: State): Seq[YulStatement] = {
        if (stateIdx == -1){
            stateIdx = tempTableIdx
            tempTableIdx += 1
        }
        // add state name to enum value mapping
        stateEnumMapping += s.name -> stateEnumCounter
        stateEnumCounter += 1

        Seq()
    }

    def translateConstructor(constructor: Constructor): Seq[YulStatement] = {
        val extractTypeName: (VariableDeclWithSpec) => TypedName = (v => TypedName(v.varName, mapObsTypeToABI(v.typIn.toString())) )
        val parameters: Seq[TypedName] = (constructor.args).map[TypedName](extractTypeName)
        var body: Seq[YulStatement] =Seq()
        for (s <- constructor.body){
            body = body ++ translateStatement(s)
        }
        val deployExpr = FunctionCall(
            Identifier("constructor_"+constructor.name), // TODO change how to find constructor function name after adding randomized suffix/prefix
            Seq()) // TODO constructor params not implemented

        Seq(ExpressionStatement(deployExpr),
            FunctionDefinition(
                "constructor_"+constructor.name, // TODO rename transaction name (by adding prefix/suffix)
                parameters,
                Seq(),
                Block(body)))
    }

    // TODO unimplemented; hardcode to uint256 for now
    def mapObsTypeToABI(ntype: String) = {
        "uint256"
    }

    def translateTransaction(transaction: Transaction): Seq[YulStatement] = {
        val f: (VariableDeclWithSpec) => TypedName = v => TypedName(v.varName, mapObsTypeToABI(v.typIn.toString()))
        val parameters: Seq[TypedName] = transaction.args.map[TypedName](f)

        val ret: Seq[TypedName] =
            if (transaction.retType.isEmpty) {
                Seq()
            } else { // TODO hard code return variable name now, need a special naming convention to avoid collisions
                Seq(TypedName("retValTempName", mapObsTypeToABI(transaction.retType.get.toString())) )
            }

        var body: Seq[YulStatement] = Seq()
        for (s <- transaction.body){
            body = body ++ translateStatement(s)
        }

        Seq(FunctionDefinition(
            transaction.name, // TODO rename transaction name (by adding prefix/suffix)
            parameters,
            ret,
            Block(body)))
    }

    def translateStatement(s: Statement): Seq[YulStatement] = {
        s match {
            case Return() =>
                Seq()
            case ReturnExpr(e) =>
                translateExpr(e)
            case Assignment(assignTo, e) =>
                assignTo match {
                    // TODO only support int/int256 now
                    case ReferenceIdentifier(x) =>
                        val idx = tempSymbolTable(x)
                        val value = e match {
                            case NumLiteral(v) => v
                            case _ =>
                                assert(false, "TODO")
                                0
                        }
                        val expr = FunctionCall(Identifier("sstore"), Seq(Literal(LiteralKind.number, idx.toString(), "int"),Literal(LiteralKind.number, value.toString(), "int")))
                        Seq(ExpressionStatement(expr))
                    case _ =>
                        assert(false, "TODO")
                        Seq()
                }

            case _ =>
                assert(false, "TODO")
                Seq()

        }
    }

    def translateExpr(e: Expression): Seq[YulStatement] = {
        e match {
            case ReferenceIdentifier(x) =>
                val idx = tempSymbolTable(x)
                val expr = FunctionCall(Identifier("sload"), Seq(Literal(LiteralKind.number, idx.toString(), "int")))
                Seq(ExpressionStatement(expr))
            case NumLiteral(n) =>
                // we compile to int, which is s256 in yul, first asserting to make sure that the
                // literal in the obsidian source is small enough.
                val top = 0x7FFFFFFF
                assert(n < top, "numeric literals larger than " + top + "are not supported")
                Seq(ExpressionStatement(Literal(LiteralKind.number,n.toString(),"int")))
            case _ =>
                assert(false, "TODO: translation of " + e.toString() + " is not implemented")
                Seq() // TODO unimplemented
        }
    }
}

// Yulstring
// document scope class relation to mustache
// temporary function, not designed for a full recursive walk through of the object
class ObjScope(obj: YulObject) {
    class Func(val code: String){}
    class Case(val hash: String){}
    class Call(val call: String){}

    // TODO unimplemented; hardcode to uint256 for now
    def mapObsTypeToABI(ntype: String): String = {
        "uint256"
    }

    // TODO unimplemented; hardcode for now; bouncycastle library may be helpful
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
            case f: FunctionDefinition => deployFunctionArray = deployFunctionArray :+ new Func(yulString.yulFunctionDefString(f))
            case e: ExpressionStatement =>
                e.expression match {
                    case f: FunctionCall => deployCall = deployCall :+ new Call(yulString.yulFunctionCallString(f))
                    case _ => () // TODO unimplemented
                }
            case _ => () // TODO unimplemented
        }
    }

    for (sub <- obj.subObjects) { // TODO separate runtime object out as a module
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
                    case litn : Literal =>
                        codeBody = codeBody :+ new Body(litn.value.toString())
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

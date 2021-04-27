package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.codegen.LiteralKind.LiteralKind

import java.io.{File, FileWriter}
import java.nio.file.{Files, Path, Paths}
// note: some constructor names collide with edu.cmu.cs.obsidian.codegen.
// in those places we use the fully qualified name
import edu.cmu.cs.obsidian.Main.{findMainContract, findMainContractName}
import edu.cmu.cs.obsidian.codegen.Util._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck.ContractType

import scala.collection.immutable.Map

object CodeGenYul extends CodeGenerator {

    // TODO improve this temporary symbol table
    var tempSymbolTable: Map[String, Int] = Map() // map from field identifiers to index in storage
    var tempTableIdx: Int = 0 // counter indicating the next available slot in the table
    var stateIdx: Int = -1 // whether or not there is a state
    var stateEnumMapping: Map[String, Int] = Map() // map from state name to an enum value
    var stateEnumCounter: Int = 0 // counter indicating the next value to assign since we don't know the total num of states

    // we generate new temporary variables with a little bit of global state; i am making the
    // implicit assumption that nothing except nextTemp will modify the contents of tempCnt, even
    // though that is not enforced statically.
    var tempCnt: Int = 0
    var retCnt: Int = 0

    def nextTemp(): Identifier = {
        tempCnt = tempCnt + 1
        Identifier(name = s"_tmp_${tempCnt.toString}") //todo: better naming convention?
    }

    // todo this is getting redundant, find a better way
    def nextRet(): String = {
        tempCnt = tempCnt + 1
        s"_ret_${tempCnt.toString}" //todo: better naming convention?
    }


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
        val writer = new FileWriter(new File(finalOutputPath.toString, translated_obj.name + ".yul"))
        writer.write(s)
        writer.flush()
        true
    }

    def translateProgram(program: Program): YulObject = {
        // translate main contract, or fail if none is found or only a java contract is present
        val main_contract_ast: YulObject =
            findMainContract(program) match {
                case Some(p) => p match {
                    case c@ObsidianContractImpl(_, _, _, _, _, _, _, _) => translateContract(c)
                    case JavaFFIContractImpl(_, _, _, _, _) =>
                        throw new RuntimeException("Java contract not supported in yul translation")
                }
                case None => throw new RuntimeException("No main contract found")
            }

        // TODO ignore imports, data for now
        // translate other contracts (if any) and add them to the subObjects
        var new_subObjects: Seq[YulObject] = main_contract_ast.subobjects
        for (c <- program.contracts) {
            c match {
                case obsContract: ObsidianContractImpl =>
                    if (!c.modifiers.contains(IsMain())) { // if not main contract
                        // this is a top level contract object
                        // note: interfaces are not translated;
                        // TODO detect an extra contract named "Contract", skip that as a temporary fix
                        if (c.name != ContractType.topContractName) {
                            new_subObjects = main_contract_ast.subobjects :+ translateContract(obsContract)
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
                assert(assertion = false, "TODO")
                (Seq(), Seq())
            case _: JavaFFIContractImpl =>
                assert(assertion = false, "Java contracts not supported in Yul translation")
                (Seq(), Seq())
            case c: Constructor =>
                (translateConstructor(c), Seq())
            case t: TypeDecl =>
                assert(assertion = false, "TODO")
                (Seq(), Seq())
            // This should never be hit.
            case _ =>
                assert(assertion = false, "Translating unexpected declaration: " + declaration)
                (Seq(), Seq())
        }
    }

    def translateField(f: Field): Seq[YulStatement] = {
        // Reserve a slot in the storage by assigning a index in the symbol table
        // since field declaration has not yet be assigned, there is no need to do sstore
        tempSymbolTable += f.name -> tempTableIdx
        tempTableIdx += 1
        Seq() // TODO: do we really mean to always return the empty sequence?
    }

    def translateState(s: State): Seq[YulStatement] = {
        if (stateIdx == -1) {
            stateIdx = tempTableIdx
            tempTableIdx += 1
        }
        // add state name to enum value mapping
        stateEnumMapping += s.name -> stateEnumCounter
        stateEnumCounter += 1

        Seq() // TODO: do we really mean to always return the empty sequence?
    }

    def translateConstructor(constructor: Constructor): Seq[YulStatement] = {
        val new_name: String = "constructor_" + constructor.name
        val deployExpr = FunctionCall(
            Identifier(new_name), // TODO change how to find constructor function name after adding randomized suffix/prefix
            Seq()) // TODO constructor params not implemented

        Seq(ExpressionStatement(deployExpr),
            FunctionDefinition(
                new_name, // TODO rename transaction name (by adding prefix/suffix) iev: this seems to be done already
                constructor.args.map(v => TypedName(v.varName, mapObsTypeToABI(v.typIn.toString))),
                Seq(), //todo/iev: why is this always empty?
                Block(constructor.body.flatMap((s: Statement) => translateStatement(s, None))))) //todo iev flatmap may be a bug to hide something wrong; None means that constructors don't return. is that true?
    }

    def translateTransaction(transaction: Transaction): Seq[YulStatement] = {
        var id: Option[String] = None
        val ret: Seq[TypedName] =
            transaction.retType match {
                case Some(t) =>
                    id = Some(nextRet())
                    Seq(TypedName(id.get, mapObsTypeToABI(t.toString)))
                case None => Seq()
            }

        Seq(FunctionDefinition(
            transaction.name, // TODO rename transaction name (by adding prefix/suffix)
            transaction.args.map(v => TypedName(v.varName, mapObsTypeToABI(v.typIn.toString))),
            ret,
            Block(transaction.body.flatMap((s: Statement) => translateStatement(s, id))))) //todo iev temp vars: likely a hack to work around something wrong
    }

    /**
      * translate an obsidian statement into the corresponding sequence of yul statements
      *
      * @param s      the statement to be translated
      * @param retVar the name of the variable to use for returning for the current scope, if there is one
      * @return
      */
    def translateStatement(s: Statement, retVar: Option[String]): Seq[YulStatement] = {
        s match {
            case Return() =>
                Seq(Leave())
            case ReturnExpr(e) =>
                retVar match {
                    case Some(retVarName) =>
                        val temp_id = nextTemp()
                        val e_yul = translateExpr(temp_id, e)
                        e_yul ++ Seq(edu.cmu.cs.obsidian.codegen.Assignment(Seq(Identifier(retVarName)), temp_id), Leave())
                    case None => assert(assertion = false, "error: returning an expression from a transaction without a return type")
                        Seq()
                }
            case Assignment(assignTo, e) =>
                assignTo match {
                    case ReferenceIdentifier(x) =>
                        val kind: LiteralKind = e match {
                            case NumLiteral(_) => LiteralKind.number
                            case TrueLiteral() => LiteralKind.boolean
                            case FalseLiteral() => LiteralKind.boolean
                            case StringLiteral(_) => LiteralKind.string
                            case _ =>
                                assert(assertion = false, s"unimplemented assignment case ${assignTo.toString}")
                                LiteralKind.number
                        }
                        Seq(ExpressionStatement(FunctionCall(Identifier("sstore"),
                            Seq(ilit(tempSymbolTable(x)), Literal(kind, e.toString, kind.toString))))) //todo: this is likely wrong for strings
                    case e =>
                        assert(assertion = false, "TODO: translate assignment case" + e.toString)
                        Seq()
                }
            case IfThenElse(scrutinee, pos, neg) =>
                val id = nextTemp()
                val scrutinee_yul: Seq[YulStatement] = translateExpr(id, scrutinee)
                val pos_yul: Seq[YulStatement] = pos.flatMap(s => translateStatement(s, retVar)) // todo iev be careful here this might be wrong
                val neg_yul: Seq[YulStatement] = neg.flatMap(s => translateStatement(s, retVar))
                scrutinee_yul ++ Seq(edu.cmu.cs.obsidian.codegen.Switch(id, Seq(Case(true_lit, Block(pos_yul)),
                    Case(false_lit, Block(neg_yul)))))
            case e: Expression => translateExpr(nextTemp(), e)
            case VariableDecl(typ, varName) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case VariableDeclWithInit(typ, varName, e) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case VariableDeclWithSpec(typIn, typOut, varName) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case Transition(newStateName, updates, thisPermission) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case Revert(maybeExpr) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case If(eCond, s) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case IfInState(e, ePerm, typeState, s1, s2) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case TryCatch(s1, s2) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case Switch(e, cases) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case StaticAssert(expr, typeState) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
        }
    }

    // helper function for a common calling pattern below. todo: there may be a slicker way to do
    //  this with https://docs.scala-lang.org/tour/mixin-class-composition.html in the future
    //  once all the cases are written and work
    def call(s: String, retvar: Identifier, es: Expression*): Seq[YulStatement] = {
        // for each expression, make a new temporary variable and translate the expression
        val es_trans: Seq[(Seq[YulStatement], Identifier)] = es.map(e => {
            val id = nextTemp()
            (translateExpr(id, e), id)
        })

        // flatten the resultant sequences and do them first, then make the call to the function using the Ids
            es_trans.map(x => edu.cmu.cs.obsidian.codegen.VariableDeclaration(Seq((x._2, None)), None)) ++
            es_trans.flatMap(x => x._1) ++
            assign1(retvar, ap(s, es_trans.map(x => x._2): _*))
    }

    def geq_leq(s: String, retvar: Identifier, e1: Expression, e2: Expression): Seq[YulStatement] = {
        // this doesn't fit the pattern of binary_call or a more general version that
        // takes  (Identifier, Identifier) => Expression, because what you want to do
        // is build another Obsidian Expression but with the Yul Identifiers for the
        // temp vars in it, which is incoherent.
        //
        // todo: maybe there's a more elegant way to do this with less repeated code
        val e1id = nextTemp()
        val e2id = nextTemp()
        Seq(VariableDeclaration(Seq((e1id,None)),None), VariableDeclaration(Seq((e2id,None)),None)) ++
        translateExpr(e1id, e1) ++
            translateExpr(e2id, e2) ++
            Seq(edu.cmu.cs.obsidian.codegen.Assignment(Seq(retvar), ap("or", ap(s, e1id, e2id), ap("eq", e1id, e2id))))
    }

    def translateExpr(retvar: Identifier, e: Expression): Seq[YulStatement] = {
        e match {
            case e: AtomicExpression =>
                e match {
                    case ReferenceIdentifier(x) =>
                        assign1(retvar, FunctionCall(Identifier("sload"), Seq(ilit(tempSymbolTable(x)))))
                    case NumLiteral(n) =>
                        assign1(retvar, ilit(n))
                    case StringLiteral(value) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case TrueLiteral() =>
                        assign1(retvar, true_lit)
                    case FalseLiteral() =>
                        assign1(retvar, false_lit)
                    case This() =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Parent() =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: UnaryExpression =>
                e match {
                    case LogicalNegation(e) => call("not", retvar, e) // todo "bitwise “not” of x (every bit of x is negated)", which may be wrong
                    case Negate(e) => translateExpr(retvar, Subtract(NumLiteral(0), e))
                    case Dereference(_, _) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Disown(_) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: BinaryExpression =>
                e match {
                    case Conjunction(e1, e2) => call("and", retvar, e1, e2)
                    case Disjunction(e1, e2) => call("or", retvar, e1, e2)
                    case Add(e1, e2) => call("add", retvar, e1, e2)
                    case StringConcat(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Subtract(e1, e2) => call("sub", retvar, e1, e2)
                    case Divide(e1, e2) => call("sdiv", retvar, e1, e2) // todo div is for unsigneds; i believe we have signed ints?
                    case Multiply(e1, e2) => call("mul", retvar, e1, e2)
                    case Mod(e1, e2) => call("smod", retvar, e1, e2) // todo as with div
                    case Equals(e1, e2) => call("eq", retvar, e1, e2)
                    case GreaterThan(e1, e2) => call("sgt", retvar, e1, e2) // todo as with div
                    case GreaterThanOrEquals(e1, e2) => geq_leq("sgt", retvar, e1, e2)
                    case LessThan(e1, e2) => call("slt", retvar, e1, e2) //todo as with div
                    case LessThanOrEquals(e1, e2) => geq_leq("slt", retvar, e1, e2)
                    case NotEquals(e1, e2) => translateExpr(retvar, LogicalNegation(Equals(e1, e2)))
                }
            case e@LocalInvocation(name, genericParams, params, args) => // todo: why are the middle two args not used?
                val (seqs, ids: Seq[edu.cmu.cs.obsidian.codegen.Expression]) =
                    args.map(p => {
                        val id = nextTemp()
                        (translateExpr(id, p), ExpressionStatement(id))
                    }).unzip
                seqs.flatten :+ ExpressionStatement(FunctionCall(Identifier(name), ids))
            case Invocation(recipient, genericParams, params, name, args, isFFIInvocation) =>
                assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                Seq()
            case Construction(contractType, args, isFFIInvocation) =>
                assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                Seq()
            case StateInitializer(stateName, fieldName) =>
                assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                Seq()
        }
    }
}
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

    def nextTemp(): Identifier = {
        tempCnt = tempCnt + 1
        Identifier(name = s"_tmp_${tempCnt.toString}") //todo: better naming convention?
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

        // memory init
        val freeMemPointer = 64 // 0x40: currently allocated memory size (aka. free memory pointer)
        val firstFreeMem = 128 //  0x80: first byte in memory not reserved for special usages
        // the free memory pointer points to 0x80 initially
        val initExpr = FunctionCall(Identifier("mstore"), Seq(ilit(freeMemPointer), ilit(firstFreeMem)))
        statement_seq_deploy = statement_seq_deploy :+ ExpressionStatement(initExpr)
        statement_seq_runtime = statement_seq_runtime :+ ExpressionStatement(initExpr) //todo add `:+ ExpressionStatement(callvaluecheck)` here, fix what breaks

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
                Block(constructor.body.flatMap((s: Statement) => translateStatement(s))))) //todo iev temp vars: likely a hack to work around something wrong
    }

    def translateTransaction(transaction: Transaction): Seq[YulStatement] = {
        val ret: Seq[TypedName] =
            if (transaction.retType.isEmpty) {
                Seq()
            } else { // TODO hard code return variable name now, need a special naming convention to avoid collisions
                Seq(TypedName("retValTempName", mapObsTypeToABI(transaction.retType.get.toString)))
            }

        Seq(FunctionDefinition(
            transaction.name, // TODO rename transaction name (by adding prefix/suffix)
            transaction.args.map(v => TypedName(v.varName, mapObsTypeToABI(v.typIn.toString))),
            ret,
            Block(transaction.body.flatMap((s: Statement) => translateStatement(s))))) //todo iev temp vars: likely a hack to work around something wrong
    }

    def translateStatement(s: Statement): Seq[YulStatement] = {
        s match {
            case Return() =>
                Seq(Leave())
            case ReturnExpr(e) =>
                // we need to allocate some space in some form of memory and put e there
                assert(assertion = false, "TODO: returning a value not implemented")
                Seq()
            case Assignment(assignTo, e) =>
                assignTo match {
                    case ReferenceIdentifier(x) =>
                        val kind: LiteralKind = e match {
                            case NumLiteral(_) => LiteralKind.number
                            case TrueLiteral() => LiteralKind.boolean
                            case FalseLiteral() => LiteralKind.boolean
                            case l =>
                                assert(assertion = false, "TODO: unimplemented translate assignment case: " + l.toString)
                                LiteralKind.number
                        }
                        Seq(ExpressionStatement(FunctionCall(Identifier("sstore"),
                            Seq(ilit(tempSymbolTable(x)), Literal(kind, e.toString, kind.toString)))))
                    case e =>
                        assert(assertion = false, "TODO: translate assignment case" + e.toString)
                        Seq()
                }
            case IfThenElse(scrutinee, pos, neg) =>
                val id = nextTemp()
                val scrutinee_yul: Seq[YulStatement] = translateExpr(id, scrutinee)
                val pos_yul: Seq[YulStatement] = pos.flatMap(s => translateStatement(s)) // todo iev be careful here this might be wrong
                val neg_yul: Seq[YulStatement] = neg.flatMap(s => translateStatement(s))
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

    def translateExpr(retvar: Identifier, e: Expression): Seq[YulStatement] = {
        e match {
            case e: AtomicExpression =>
                e match {
                    case ReferenceIdentifier(x) =>
                        store_then_ret(retvar, FunctionCall(Identifier("sload"), Seq(ilit(tempSymbolTable(x)))))
                    case NumLiteral(n) =>
                        store_then_ret(retvar, ilit(n))
                    case StringLiteral(value) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case TrueLiteral() =>
                        store_then_ret(retvar, true_lit)
                    case FalseLiteral() =>
                        store_then_ret(retvar, false_lit)
                    case This() =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Parent() =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: UnaryExpression =>
                e match {
                    case LogicalNegation(e) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Negate(e) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Dereference(e, f) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Disown(e) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: BinaryExpression =>
                e match {
                    case Conjunction(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Disjunction(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Add(e1, e2) =>
                        val e1_id = nextTemp()
                        val e2_id = nextTemp()
                        translateExpr(e1_id, e1) ++ translateExpr(e2_id, e2) ++ store_then_ret(retvar, binary("add", e1_id, e2_id))
                    case StringConcat(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Subtract(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Divide(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Multiply(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Mod(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Equals(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case GreaterThan(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case GreaterThanOrEquals(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case LessThan(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case LessThanOrEquals(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case NotEquals(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e@LocalInvocation(name, genericParams, params, args) =>
                //val expr = FunctionCall(Identifier(name),args.map(x => translateExpr(e) match)) // todo iev working here
                Seq()
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
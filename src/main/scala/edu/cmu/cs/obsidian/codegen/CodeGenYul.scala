package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.CompilerOptions
import edu.cmu.cs.obsidian.parser.ContractTable

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

    var tempTableIdx: Int = 0 // counter indicating the next available slot in the table
    var stateIdx: Int = -1 // whether or not there is a state
    var stateEnumMapping: Map[String, Int] = Map() // map from state name to an enum value
    var stateEnumCounter: Int = 0 // counter indicating the next value to assign since we don't know the total num of states


    // we generate new temporary variables with a little bit of global state; i am making the
    // implicit assumption that nothing except nextTemp will modify the contents of tempCnt, even
    // though that is not enforced statically.
    //
    // todo: this is getting redundant; better way to manage different vars we create?
    var tempCnt: Int = 0
    var retCnt: Int = 0

    def nextTemp(): Identifier = {
        tempCnt = tempCnt + 1
        Identifier(name = s"_tmp_${tempCnt.toString}") //todo: better naming convention?
    }

    // todo this is getting redundant, find a better way
    def nextRet(): String = {
        retCnt = retCnt + 1
        s"_ret_${retCnt.toString}" //todo: better naming convention?
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
        val translated_obj = translateProgram(ast, checkedTable)
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

    def translateProgram(program: Program, checkedTable: SymbolTable): YulObject = {
        // translate main contract, or fail if none is found or only a java contract is present
        val main_contract_ast: YulObject =
            findMainContract(program) match {
                case Some(p) => p match {
                    case c@ObsidianContractImpl(_, _, _, _, _, _, _, _) => translateContract(c, checkedTable)
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
                            new_subObjects = main_contract_ast.subobjects :+ translateContract(obsContract, checkedTable)
                        }
                    }
                case _: JavaFFIContractImpl =>
                    throw new RuntimeException("Java contract not supported in yul translation")
            }
        }

        YulObject(main_contract_ast.name, main_contract_ast.code, new_subObjects, main_contract_ast.data)
    }

    def translateContract(contract: ObsidianContractImpl, checkedTable: SymbolTable): YulObject = {
        var subObjects: Seq[YulObject] = Seq()
        var statement_seq_deploy: Seq[YulStatement] = Seq()
        var statement_seq_runtime: Seq[YulStatement] = Seq()

        // translate declarations
        for (d <- contract.declarations) {
            val (deploy_seq, runtime_seq) = translateDeclaration(d, contract.name, checkedTable)
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
    def translateDeclaration(declaration: Declaration, contractName: String, checkedTable: SymbolTable): (Seq[YulStatement], Seq[YulStatement]) = {
        declaration match {
            case f: Field => (Seq(), translateField(f))
            case t: Transaction =>
                (Seq(), translateTransaction(t, contractName, checkedTable))
            case s: State =>
                (Seq(), translateState(s))
            case c: ObsidianContractImpl =>
                assert(assertion = false, "TODO")
                (Seq(), Seq())
            case _: JavaFFIContractImpl =>
                assert(assertion = false, "Java contracts not supported in Yul translation")
                (Seq(), Seq())
            case c: Constructor =>
                (translateConstructor(c, contractName, checkedTable), Seq())
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

    def translateConstructor(constructor: Constructor, contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        val new_name: String = "constructor_" + constructor.name
        val deployExpr = FunctionCall(
            Identifier(new_name), // TODO change how to find constructor function name after adding randomized suffix/prefix
            Seq()) // TODO constructor params not implemented

        Seq(ExpressionStatement(deployExpr),
            FunctionDefinition(
                new_name, // TODO rename transaction name (by adding prefix/suffix) iev: this seems to be done already
                constructor.args.map(v => TypedName(v.varName, mapObsTypeToABI(v.typIn.toString))),
                Seq(), //todo/iev: why is this always empty?
                Block(constructor.body.flatMap((s: Statement) => translateStatement(s, None, contractName, checkedTable))))) //todo iev flatmap may be a bug to hide something wrong; None means that constructors don't return. is that true?
    }

    def translateTransaction(transaction: Transaction, contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        var id: Option[String] = None
        val ret: Seq[TypedName] = {
            transaction.retType match {
                case Some(t) =>
                    id = Some(nextRet())
                    Seq(TypedName(id.get, mapObsTypeToABI(t.toString)))
                case None => Seq()
            }
        }

        Seq(FunctionDefinition(
            transaction.name, // TODO rename transaction name (by adding prefix/suffix)
            transaction.args.map(v => TypedName(v.varName, mapObsTypeToABI(v.typIn.toString))),
            ret,
            Block(transaction.body.flatMap((s: Statement) => translateStatement(s, id, contractName, checkedTable))))) //todo iev temp vars: likely a hack to work around something wrong
    }

    /**
      * translate an obsidian statement into the corresponding sequence of yul statements
      *
      * @param s      the statement to be translated
      * @param retVar the name of the variable to use for returning for the current scope, if there is one
      * @return
      */
    def translateStatement(s: Statement, retVar: Option[String], contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        //todo: why is retVar an option and why is it a string not an identifier?
        s match {
            case Return() =>
                Seq(Leave())
            case ReturnExpr(e) =>
                retVar match {
                    case Some(retVarName) =>
                        val temp_id = nextTemp()
                        val e_yul = translateExpr(temp_id, e, contractName, checkedTable)
                        decl_0exp(temp_id) +:
                            e_yul :+
                            assign1(Identifier(retVarName), temp_id) :+
                            Leave()
                    case None => assert(assertion = false, "error: returning an expression from a transaction without a return type")
                        Seq()
                }
            case Assignment(assignTo, e) =>
                assignTo match {
                    case ReferenceIdentifier(x) =>
                        // todo: this assumes that all identifiers are either fields or stack variables.
                        //  it also likely does not work correctly with shadowing.
                        val id = nextTemp()
                        val e_yul = translateExpr(id, e, contractName, checkedTable)
                        decl_0exp(id) +:
                            e_yul :+
                            (if(checkedTable.contractLookup(contractName).allFields.exists(f => f.name.equals(x))) {
                                //todo: compute offsets
                                ExpressionStatement(apply("sstore",hexlit(keccak256(contractName+x)),id))
                            } else {
                                assign1(Identifier(x), id)
                            })
                    case _ =>
                        assert(assertion = false, "trying to assign to non-assignable: " + e.toString)
                        Seq()
                }
            case IfThenElse(scrutinee, pos, neg) =>
                // generate a temp to store the last assignment used in either block
                val id_last = nextTemp()
                // generate a temp for the scrutinee
                val id_scrutinee: Identifier = nextTemp()

                // translate the scrutinee
                val scrutinee_yul: Seq[YulStatement] = translateExpr(id_scrutinee, scrutinee, contractName, checkedTable)

                // translate each block and generate an extra assignment for the last statement
                val pos_yul: Seq[YulStatement] = pos.flatMap(s => translateStatement(s, retVar, contractName, checkedTable))
                val neg_yul: Seq[YulStatement] = neg.flatMap(s => translateStatement(s, retVar, contractName, checkedTable))

                // put the pieces together into a switch statement, preceded by the evaluation of the scrutinee
                decl_0exp(id_last) +:
                    decl_0exp(id_scrutinee) +:
                    scrutinee_yul :+
                    edu.cmu.cs.obsidian.codegen.Switch(id_scrutinee,
                        Seq(Case(boollit(true), Block(pos_yul)),
                            Case(boollit(false), Block(neg_yul))))

            case e: Expression =>
                // todo: tighten up this logic, there's repeated code here
                retVar match {
                    case Some(value) => translateExpr(Identifier(value), e, contractName, checkedTable)
                    case None =>
                        val id = nextTemp()
                        decl_0exp(id) +: translateExpr(id, e, contractName, checkedTable)
                }
            case VariableDecl(typ, varName) =>
                Seq(decl_0exp_t(Identifier(varName), typ))
            case VariableDeclWithInit(typ, varName, e) =>
                val id = nextTemp()
                val e_yul = translateExpr(id, e, contractName, checkedTable)
                decl_0exp(id) +:
                    e_yul :+
                    decl_0exp_t_init(Identifier(varName), typ, id)
            case VariableDeclWithSpec(typIn, typOut, varName) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case Transition(newStateName, updates, thisPermission) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case Revert(maybeExpr) =>
                assert(assertion = false, s"TODO: translateStatement unimplemented for ${s.toString}")
                Seq()
            case If(scrutinee, s) =>
                val id_scrutinee: Identifier = nextTemp()
                val scrutinee_yul: Seq[YulStatement] = translateExpr(id_scrutinee, scrutinee, contractName, checkedTable)
                val s_yul: Seq[YulStatement] =
                    s.flatMap(s => {
                        val id_s: Identifier = nextTemp()
                        decl_0exp(id_s) +: translateStatement(s, Some(id_s.name), contractName, checkedTable)
                        //todo: this also does not assign afterwards; likely the same bug as fixed in IfThenElse
                    })

                decl_0exp(id_scrutinee) +:
                    scrutinee_yul :+
                    edu.cmu.cs.obsidian.codegen.If(id_scrutinee, Block(s_yul))
            // todo: also no assignment here
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
    def call(s: String, retvar: Identifier, contractName: String, checkedTable: SymbolTable, es: Expression*): Seq[YulStatement] = {
        // for each expression, make a new temporary variable and translate the expression
        val es_trans: Seq[(Seq[YulStatement], Identifier)] = es.map(e => {
            val id = nextTemp()
            (translateExpr(id, e, contractName, checkedTable), id)
        })

        // flatten the resultant sequences and do them first, then make the call to the function using the Ids
        es_trans.map(x => decl_0exp(x._2)) ++
            es_trans.flatMap(x => x._1) :+
            assign1(retvar, apply(s, es_trans.map(x => x._2): _*))
    }

    def geq_leq(s: String, retvar: Identifier, e1: Expression, e2: Expression, contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        // this doesn't fit the pattern of binary_call or a more general version that
        // takes  (Identifier, Identifier) => Expression, because what you want to do
        // is build another Obsidian Expression but with the Yul Identifiers for the
        // temp vars in it, which is incoherent.
        //
        // todo: maybe there's a more elegant way to do this with less repeated code
        val e1id = nextTemp()
        val e2id = nextTemp()
        Seq(decl_0exp(e1id), decl_0exp(e2id)) ++
            translateExpr(e1id, e1, contractName, checkedTable) ++
            translateExpr(e2id, e2, contractName, checkedTable) :+
            assign1(retvar, apply("or", apply(s, e1id, e2id), apply("eq", e1id, e2id)))
    }


    /**
      * Given the type of a contract and a table, compute the size that we need to allocate for it in memory.
      * TODO: as a simplifying assumption, for now this always returns 256.
      *
      * @param t the contract type of interest
      * @param symTab the symbol table to look in
      * @return the size of memory needed for the contract
      */
    def contractSize(t : ContractType, symTab: SymbolTable) : Int = {
        256
    }

    def translateExpr(retvar: Identifier, e: Expression, contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        e match {
            case e: AtomicExpression =>
                e match {
                    case ReferenceIdentifier(x) =>
                        // todo: this assumes that all indentifiers are either fields or stack variables;
                        //  we store nothing in memory. this is also very likely not doing the right thing
                        //  with name shadowing

                        // todo: this also assumes that everything is a u256 and does no type-directed
                        //  cleaning in the way that solc does
                        if(checkedTable.contractLookup(contractName).allFields.exists(f => f.name.equals(x))) {
                            val store_id = nextTemp()
                            //todo: compute offsets
                            Seq(decl_1exp(store_id,apply("sload",hexlit(keccak256(contractName+x)))),
                                assign1(retvar, store_id))
                        } else {
                            Seq(assign1(retvar, Identifier(x)))
                        }
                    case NumLiteral(n) =>
                        Seq(assign1(retvar, intlit(n)))
                    case StringLiteral(value) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case TrueLiteral() =>
                        Seq(assign1(retvar, boollit(true)))
                    case FalseLiteral() =>
                        Seq(assign1(retvar, boollit(false)))
                    case This() =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Parent() =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: UnaryExpression =>
                e match {
                    case LogicalNegation(e) => translateStatement(IfThenElse(e, Seq(FalseLiteral()), Seq(TrueLiteral())), Some(retvar.name), contractName, checkedTable)
                    case Negate(e) =>
                        translateExpr(retvar, Subtract(NumLiteral(0), e), contractName, checkedTable)
                    case Dereference(_, _) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Disown(_) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: BinaryExpression =>
                e match {
                    case Conjunction(e1, e2) => call("and", retvar, contractName, checkedTable, e1, e2)
                    case Disjunction(e1, e2) => call("or", retvar, contractName, checkedTable, e1, e2)
                    case Add(e1, e2) => call("add", retvar, contractName, checkedTable, e1, e2)
                    case StringConcat(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Subtract(e1, e2) => call("sub", retvar, contractName, checkedTable, e1, e2)
                    case Divide(e1, e2) => call("sdiv", retvar, contractName, checkedTable, e1, e2) // todo div is for unsigned; i believe we have signed ints?
                    case Multiply(e1, e2) => call("mul", retvar, contractName, checkedTable, e1, e2)
                    case Mod(e1, e2) => call("smod", retvar, contractName, checkedTable, e1, e2) // todo as with div
                    case Equals(e1, e2) => call("eq", retvar, contractName, checkedTable, e1, e2)
                    case GreaterThan(e1, e2) => call("sgt", retvar, contractName, checkedTable, e1, e2) // todo as with div
                    case GreaterThanOrEquals(e1, e2) => geq_leq("sgt", retvar, e1, e2, contractName, checkedTable)
                    case LessThan(e1, e2) => call("slt", retvar, contractName, checkedTable, e1, e2) //todo as with div
                    case LessThanOrEquals(e1, e2) => geq_leq("slt", retvar, e1, e2, contractName, checkedTable)
                    case NotEquals(e1, e2) => translateExpr(retvar, LogicalNegation(Equals(e1, e2)), contractName, checkedTable)
                }
            case e@LocalInvocation(name, genericParams, params, args) => // todo: why are the middle two args not used?
                // look up the name of the function in the table, get its return type, and then compute
                // how wide of a tuple that return type is. (currently this is always either 0 or 1)
                val width = checkedTable.contractLookup(contractName).lookupTransaction(name) match {
                    case Some(trans) =>
                        trans.retType match {
                            case Some(typ) => obsTypeToWidth(typ)
                            case None => 0
                        }
                    case None =>
                        assert(assertion = false, "encountered a function name without knowing how many things it returns")
                        -1
                }

                // todo: some of this logic may be repeated in the dispatch table

                // todo: the code here is set up to mostly work in the world in which obsidian has tuples,
                //  which it does not. i wrote it before i knew that. the assert below is one place that it breaks;
                //  to fix it, i need to refactor this object so that i pass around a vector of temporary variables
                //  to assign returns to rather than just one (i think). this is OK for now, but technical debt that
                //  we'll have to address if we ever add tuples to obsidian.

                // for each argument expression, produce a new temp variable and translate it to a
                // sequence of yul statements ending in an assignment to that variable.
                val (seqs, ids) = {
                    args.map(p => {
                        val id: Identifier = nextTemp()
                        (translateExpr(id, p, contractName, checkedTable), id)
                    }).unzip
                }

                // the result is the recursive translation and the expression either using the temp
                // here or not.

                // todo: this does not work with non-void functions that are called without binding
                //  their results, ie "f()" if f returns an int
                seqs.flatten ++ (width match {
                    case 0 => Seq(ExpressionStatement(FunctionCall(Identifier(name), ids)))
                    case 1 =>
                        val id: Identifier = nextTemp()
                        Seq(decl_1exp(id, FunctionCall(Identifier(name), ids)), assign1(retvar, id))
                    case _ => assert(assertion = false, "obsidian currently does not support tuples; this shouldn't happen."); Seq()
                })

            case Invocation(recipient, genericParams, params, name, args, isFFIInvocation) =>

                val id_recipient = nextTemp()
                val recipient_yul = translateExpr(id_recipient, recipient, contractName, checkedTable)

                val id_fnselc = nextTemp()
                val id_mstore_in = nextTemp() // todo: check this with args / rets
                val id_call = nextTemp()
                val id_mstore_out = nextTemp()

                // todo this may be busted; test it
                // check the return type of the function being called; the yul emitted by solc
                //   deals with this by just checking the result of call, i think.
//                val store_return = checkedTable.contractLookup(recipient.toString).lookupTransaction(name) match {
//                    case Some(value) => value.retType match {
//                        case Some(value) =>
//                            val abi_type = mapObsTypeToABI(value.baseTypeName)
//
//                        case None => None
//                    }
//                    case None => None
//                }


                (decl_0exp(id_recipient) +: recipient_yul) ++
                    Seq(
                        // todo these three lines i'm skipping because they propagate the result of the
                        //  create() call from construct. i can get that from the recipient name here.
                        // let var_ic_27_address := expr_31_address// skip
                        // let _4_address := var_ic_27_address // skip
                        // let expr_33_address := _4_address // skip
                        // let expr_35_address := convert_t_contract$_IntContainer_$20_to_t_address(expr_33_address)
                        // let expr_35_functionSelector := 0xb8e010de // skipping this, i'll just inline it below
                        // if iszero(extcodesize(expr_35_address)) { revert_error_0cc013b6b3b6beabea4e3a74a6d380f0df81852ca99887912475e1f66b2a2c20() }
                        revertIf(apply("iszero",apply("extcodesize", id_recipient))),

                        //// storage for arguments and returned data
                        // let _5 := allocate_unbounded()
                        // mstore(_5, shift_left_224(expr_35_functionSelector))
                        decl_1exp(id_fnselc, hexlit(hashOfFunctionName(name, params.map(t => mapObsTypeToABI(t.baseTypeName))))),
                        decl_1exp(id_mstore_in, apply("allocate_unbounded")),
                        ExpressionStatement(apply("mstore", id_mstore_in, apply("shl", intlit(224), id_fnselc))), // todo: 224 is a magic number

                        // let _6 := abi_encode_tuple__to__fromStack(add(_5, 4) )
                        // todo: this seems to just add 4 and i have no idea why right now; it's probably type-directed.
                        decl_1exp(id_mstore_out, apply("add", id_mstore_in, intlit(4))),

                        // let _7 := call(gas(), expr_35_address,  0,  _5, sub(_6, _5), _5, 0)
                        decl_1exp(id_call,
                            apply("call",
                                apply("gas"), // all the gas we have right now
                                id_recipient, // address of the contract being called
                                intlit(0),    // amount of money being passed
                                id_mstore_in, // todo: check this
                                apply("sub",id_mstore_out, id_mstore_in), // todo: check this
                                id_mstore_in, // check this
                                intlit(0)) // todo: update when we support returns; needs to be the size of the thing returned.
                        ),

                        // if iszero(_7) { revert_forward_1() }
                        revertForwardIfZero(id_call),

                        // if there's a return, it needs to be mloaded into the retvar. maybe just load it no matter what? and then if
                        // the source doesn't use it then nothing subsequent will check that temp.

                        // if _7 {
                        //    // update freeMemoryPointer according to dynamic return size
                        //    finalize_allocation(_5, returndatasize())
                        //
                        //    // decode return parameters from external try-call into retVars
                        //    abi_decode_tuple__fromMemory(_5, add(_5, returndatasize()))
                        // }

                        edu.cmu.cs.obsidian.codegen.If(id_call, Block(
                            Seq(
                                ExpressionStatement(apply("finalize_allocation", id_mstore_in, apply("returndatasize"))),
                                assign1(id_call, apply("mload", intlit(0))), // todo this has to be wrong
                                assign1(retvar, id_call) // todo: maybe just doing it up here instead of always? is that right?
                            )
                        ))
                        //assign1(retvar, id_call) // todo: this is wrong; it means that i'm always assigning to the return var, even if the context doesn't make that the right thing to do. eg. both set() and return(get()) assign to the retvar. i've solved this problem before, i just need to remember how.
                )

            case Construction(contractType, args, isFFIInvocation) =>
                // todo: currently we ignore the arguments to the constructor

                val max_addr = "0xffffffffffffffff"
                val id_alloc = nextTemp()
                val id_newbound = nextTemp()
                val id_addr = nextTemp()

                // check if either the new bound is bigger than the max address or less than the previous allocated range
                val addr_check = apply("or", apply("gt", id_newbound, stringlit(max_addr)),
                                             apply("lt", id_newbound, id_alloc))

                // size of the structure we're allocating
                val struct_size = apply("datasize",stringlit(contractType.contractName))

                // size of the structure we're allocating
                val struct_offset = apply("dataoffset",stringlit(contractType.contractName))
                Seq(
                    // let _2 := allocate_unbounded()
                    decl_1exp(id_alloc, apply("allocate_unbounded")),
                    // let _3 := add(_2, datasize("IntContainer_22"))
                    decl_1exp(id_newbound, struct_size),
                    // if or(gt(_3, 0xffffffffffffffff), lt(_3, _2)) { panic_error_0x41() }
                    edu.cmu.cs.obsidian.codegen.If(addr_check, Block(Seq(ExpressionStatement(apply("panic_error_0x41"))))),
                    // datacopy(_2, dataoffset("IntContainer_22"), datasize("IntContainer_22"))
                    ExpressionStatement(apply("datacopy",id_alloc,struct_offset,struct_size)),
                    // _3 := abi_encode_tuple__to__fromStack(_3) // todo this adds 0, so i'm going to ignore it for now?
                    // let expr_33_address := create(0, _2, sub(_3, _2))
                    decl_1exp(id_addr, apply("create", intlit(0), id_alloc, apply("sub", id_newbound, id_alloc))),
                    // if iszero(expr_33_address) { revert_forward_1() }
                    revertForwardIfZero(id_addr),
                    assign1(retvar, id_addr)
                )
            case StateInitializer(stateName, fieldName) =>
                assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                Seq()
        }
    }
}
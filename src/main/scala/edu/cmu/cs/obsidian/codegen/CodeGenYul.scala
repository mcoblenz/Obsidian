package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.CompilerOptions

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
                        //todo: easy optimization is to look at e; if it happens to be a literal we can save a temp.
                        val id = nextTemp()
                        val e_yul = translateExpr(id, e, contractName, checkedTable)
                        decl_0exp(id) +:
                            e_yul :+
                            assign1(Identifier(x), id)
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

                // todo: this may be useful elsewhere, too

                /**
                  * translate a statement into a new temporary variable along with its declaration
                  *
                  * @param s the statement to be translated
                  * @return a pair of the new variable and the sequence of yulstatements resulting
                  *         from the translation; inductively that sequence will end in an assignment
                  *         to the declared variable.
                  */
                def trans_store(s: Statement): (Identifier, Seq[YulStatement]) = {
                    val id_s: Identifier = nextTemp()
                    (id_s, decl_0exp(id_s) +: translateStatement(s, Some(id_s.name), contractName, checkedTable))
                }

                // translate each block and generate an extra assignment for the last statement
                val pos_yul: Seq[(Identifier, Seq[YulStatement])] = pos.map(trans_store)
                val pos_assign = assign1(id_last, pos_yul.last._1)

                val neg_yul: Seq[(Identifier, Seq[YulStatement])] = neg.map(trans_store)
                val neg_assign = assign1(id_last, neg_yul.last._1)

                // assign back from which ever last statement gets run, or not depending on the inductive requirements
                val assign_back = retVar match {
                    case Some(value) => Seq(assign1(Identifier(value), id_last))
                    case None => Seq()
                }

                // put the pieces together into a switch statement, preceeded by the evaluation of the
                // scrutinee
                (decl_0exp(id_last) +:
                    decl_0exp(id_scrutinee) +:
                    scrutinee_yul :+
                    edu.cmu.cs.obsidian.codegen.Switch(id_scrutinee,
                        Seq(Case(boollit(true), Block(pos_yul.flatMap(x => x._2) :+ pos_assign)),
                            Case(boollit(false), Block(neg_yul.flatMap(x => x._2) :+ neg_assign))))) ++
                    assign_back
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

    def translateExpr(retvar: Identifier, e: Expression, contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        e match {
            case e: AtomicExpression =>
                e match {
                    case ReferenceIdentifier(x) =>
                        Seq(assign1(retvar, Identifier(x))) //todo: in general this will need to know to look in memory / storage / stack
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
                    case Negate(e) => translateExpr(retvar, Subtract(NumLiteral(0), e), contractName, checkedTable)
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
                // which it does not. i wrote it before i knew that. the assert below is one place that it breaks;
                // to fix it, i need to refactor this object so that i pass around a vector of temporary variables
                // to assign returns to rather than just one (i think). this is OK for now, but technical debt that
                // we'll have to address if we ever add tuples to obsidian.

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
                //
                // todo: this does not work with non-void functions that are called without binding their results, ie "f()" if f returns an int
                seqs.flatten ++ (width match {
                    case 0 => Seq(ExpressionStatement(FunctionCall(Identifier(name), ids)))
                    case 1 =>
                        val id: Identifier = nextTemp()
                        Seq(decl_1exp(id, FunctionCall(Identifier(name), ids)), assign1(retvar, id))
                    case _ => assert(assertion = false, "obsidian currently does not support tuples; this shouldn't happen."); Seq()
                })

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
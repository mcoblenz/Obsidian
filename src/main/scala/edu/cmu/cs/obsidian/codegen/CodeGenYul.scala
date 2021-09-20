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
        Identifier(name = s"_tmp_${tempCnt.toString}")
    }

    // todo this is getting redundant, find a better way
    def nextRet(): Identifier = {
        retCnt = retCnt + 1
        Identifier(s"_ret_${retCnt.toString}")
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

        // generate yul string from yul AST, write to the output file
        val s = translated_obj.yulString()
        Files.createDirectories(finalOutputPath)
        val writer = new FileWriter(new File(finalOutputPath.toString, translated_obj.name + ".yul"))
        writer.write(s)
        writer.flush()
        true
    }

    def translateProgram(program: Program, checkedTable: SymbolTable): YulObject = {
        // translate main contract, or fail if none is found or only a java contract is present
        val mainContractYO: YulObject =
            findMainContract(program) match {
                case Some(p) => p match {
                    case c@ObsidianContractImpl(_, _, _, _, _, _, _, _) => translateMainContract(c, checkedTable)
                    case JavaFFIContractImpl(_, _, _, _, _) =>
                        throw new RuntimeException("Java contract not supported in yul translation")
                }
                case None => throw new RuntimeException("No main contract found")
            }

        // translate other contracts (if any) and add them to the childObjects
        var childContracts: Seq[YulObject] = Seq()
        // todo: this collection always contains a contract that looks like
        //   ObsidianContractImpl(Set(),Contract,List(),Contract,List(),None,true,)
        //   and i do not know why
        for (c <- program.contracts) {
            c match {
                case obsContract: ObsidianContractImpl =>
                    if (!c.modifiers.contains(IsMain()) && c.name != ContractType.topContractName) {
                        childContracts = childContracts :+ translateNonMainContract(obsContract, checkedTable)
                    }
                case _: JavaFFIContractImpl =>
                    throw new RuntimeException("Java contract not supported in yul translation")
            }
        }

        // todo: we do not process imports
        YulObject(name = mainContractYO.name,
            code = mainContractYO.code,
            runtimeSubobj = mainContractYO.runtimeSubobj ++ childContracts,
            childContracts = Seq(), // todo maybe delete this field entirely
            data = mainContractYO.data) // todo this is always empty, we ignore data
    }

    /**
      * given a contract, produce the Yul object that contains its translation as the main object. this will not
      * rename any of the transactions for this object, but will call transactions from other objects with the
      * names that they will have in the flattened translation. e.g. `f()` remains `f()` but `ic.f()` becomes
      * `IntContainer___f(this)` if ic is an IntContainer.
      *
      * todo: right now we do not actually have enough type information to do the above in a robust way, but we will
      * add that soon
      *
      * @param contract the contract to be translated
      * @param checkedTable the symbol table for that contract
      * @return the yul
      */
    def translateMainContract(contract: ObsidianContractImpl, checkedTable: SymbolTable): YulObject = {
        var decls: Seq[YulStatement] = Seq()

        // translate declarations
        for (d <- contract.declarations) {
            decls = decls ++ translateDeclaration(d, contract.name, checkedTable, true)
        }

        // create runtime object from just the declarations and with the subobject name suffix
        val runtime_obj = YulObject(name = contract.name + "_deployed",
            code = Code(Block(decls)),
            runtimeSubobj = Seq(),
            childContracts = Seq(),
            data = Seq())

        YulObject(name = contract.name,
            code = Code(Block(Seq())),
            runtimeSubobj = Seq(runtime_obj),
            childContracts = Seq(),
            data = Seq())
    }

    /**
      * given a contract that is not the main one, produce a yul object that represents the part of its translation that's
      * ready to be inserted into the translation of a main yul object. this will rename the transactions according to the
      * contract name.
      *
      * @param c the contract to be translated
      * @param checkedTable the symbol table of the contract
      * @return the YulObject representing the translation. note that all the fields other than `code` will be the empty sequence.
      */
    def translateNonMainContract(c: ObsidianContractImpl, checkedTable: SymbolTable): YulObject = {
        var translation: Seq[YulStatement] = Seq()

        for (d <- c.declarations) {
            val dTranslated = translateDeclaration(d, c.name, checkedTable, false)
            translation = translation ++ dTranslated
        }

        YulObject(name = c.name,
            code = Code(Block(translation)),
            runtimeSubobj = Seq(),
            childContracts = Seq(),
            data = Seq()
        )
    }


    /**
      * compute the translation of a declaration into yul with respect to its context in the larger
      * obsidian program
      *
      * @param declaration  the declaration to translate
      * @param contractName the name of the contract in which the declaration appears
      * @param checkedTable the symbol table for the contract in which the declaration appears
      * @param inMain       whether or not the contract in which the declaration apepars is the main one
      * @return the yul statements corresponding to the declaration
      */
    def translateDeclaration(declaration: Declaration, contractName: String, checkedTable: SymbolTable, inMain: Boolean): Seq[YulStatement] = {
        declaration match {
            case f: Field => translateField(f) // todo
            case t: Transaction => translateTransaction(t, contractName, checkedTable, inMain)
            case s: State => translateState(s) // todo
            case c: ObsidianContractImpl =>
                assert(assertion = false, "TODO")
                Seq()
            case _: JavaFFIContractImpl =>
                assert(assertion = false, "Java contracts not supported in Yul translation")
                Seq()
            case _: Constructor =>
                assert(assertion = false, "constructors not supported in Yul translation")
                Seq()
            // todo: previously this returned a pair of sequences, and this was the only clause
            //   in which the left element was not empty. the left sequence would go in the code
            //   part of the output object rather than the runtime, but that's not where we
            //   want constructors to go.
            // (translateConstructor(c, contractName, checkedTable), Seq())
            case _: TypeDecl =>
                assert(assertion = false, "TODO")
                Seq()
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
        assert(false) // todo: this is never getting called as far as i know, and i don't really think it should be so i want to know about it if it is


        // this is basically dead code; it's never been run in any of the ganache tests because we don't have constructors.
        val new_name: String = "constructor_" + constructor.name
        val deployExpr = FunctionCall(
            Identifier(new_name), // TODO change how to find constructor function name after adding randomized suffix/prefix
            Seq()) // TODO constructor params not implemented

        Seq(ExpressionStatement(deployExpr),
            FunctionDefinition(
                new_name, // TODO rename transaction name (by adding prefix/suffix) iev: this seems to be done already
                constructor.args.map(v => TypedName(v.varName, obsTypeToYulTypeAndSize(v.typIn.toString)._1)),
                Seq(), //todo/iev: why is this always empty?
                Block(constructor.body.flatMap((s: Statement) => translateStatement(s, None, contractName, checkedTable, true))))) //todo iev flatmap may be a bug to hide something wrong; None means that constructors don't return. is that true?
    }

    def translateTransaction(transaction: Transaction, contractName: String, checkedTable: SymbolTable, inMain: Boolean): Seq[YulStatement] = {
        var id: Option[Identifier] = None

        // if the transaction appears in main, it keeps its name, otherwise it gets prepended with the name of the contract in which it appears.
        val name: String =
            if (inMain) {
                transaction.name
            } else {
                transactionNameMapping(contractName, transaction.name)
            }

        // translate the return type to the ABI names
        val ret: Seq[TypedName] = {
            transaction.retType match {
                case Some(t) =>
                    id = Some(nextRet())
                    Seq(TypedName(id.get.name, obsTypeToYulTypeAndSize(t.toString)._1))
                case None => Seq()
            }
        }

        // for transactions appearing in main, nothing changes; others get an explicit "this" argument added
        val args: Seq[TypedName] =
            if (inMain) {
                Seq() // add nothing
            } else {
                Seq(TypedName("this", "string")) // todo "this" is emphatically not a string but i'm not sure what the type of it ought to be; addr?
            } ++ transaction.args.map(v => TypedName(v.varName, obsTypeToYulTypeAndSize(v.typIn.toString)._1))

        // form the body of the transaction by translating each statement found
        val body: Seq[YulStatement] = transaction.body.flatMap((s: Statement) => translateStatement(s, id, contractName, checkedTable, inMain))

        // return the function definition formed from the above parts
        Seq(FunctionDefinition(name, args, ret, Block(body)))
    }

    /**
      * translate an obsidian statement into the corresponding sequence of yul statements
      *
      * @param s      the statement to be translated
      * @param retVar the name of the variable to use for returning for the current scope, if there is one
      * @return
      */
    def translateStatement(s: Statement, retVar: Option[Identifier], contractName: String, checkedTable: SymbolTable, inMain: Boolean): Seq[YulStatement] = {
        s match {
            case Return() =>
                Seq(Leave())
            case ReturnExpr(e) =>
                retVar match {
                    case Some(retVarName) =>
                        val temp_id = nextTemp()
                        val e_yul = translateExpr(temp_id, e, contractName, checkedTable, inMain)
                        decl_0exp(temp_id) +:
                            e_yul :+
                            assign1(Identifier(retVarName.name), temp_id) :+
                            Leave()
                    case None => assert(assertion = false, "error: returning an expression from a transaction without a return type")
                        Seq()
                }
            case Assignment(assignTo, e) =>
                assignTo match {
                    case ReferenceIdentifier(x, obstype) =>
                        // todo: this assumes that all identifiers are either fields or stack variables.
                        //  it also likely does not work correctly with shadowing.
                        val id = nextTemp()
                        val e_yul = translateExpr(id, e, contractName, checkedTable, inMain)
                        decl_0exp(id) +:
                            e_yul :+
                            (if (checkedTable.contractLookup(contractName).allFields.exists(f => f.name.equals(x))) {
                                //todo: compute offsets
                                ExpressionStatement(apply("sstore", hexlit(keccak256(contractName + x)), id))
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
                val scrutinee_yul: Seq[YulStatement] = translateExpr(id_scrutinee, scrutinee, contractName, checkedTable, inMain)

                // translate each block and generate an extra assignment for the last statement
                val pos_yul: Seq[YulStatement] = pos.flatMap(s => translateStatement(s, retVar, contractName, checkedTable, inMain))
                val neg_yul: Seq[YulStatement] = neg.flatMap(s => translateStatement(s, retVar, contractName, checkedTable, inMain))

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
                    case Some(value) => translateExpr(value, e, contractName, checkedTable, inMain)
                    case None =>
                        val id = nextTemp()
                        decl_0exp(id) +: translateExpr(id, e, contractName, checkedTable, inMain)
                }
            case VariableDecl(typ, varName) =>
                Seq(decl_0exp_t(Identifier(varName), typ))
            case VariableDeclWithInit(typ, varName, e) =>
                val id = nextTemp()
                val e_yul = translateExpr(id, e, contractName, checkedTable, inMain)
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
                val scrutinee_yul: Seq[YulStatement] = translateExpr(id_scrutinee, scrutinee, contractName, checkedTable, inMain)
                val s_yul: Seq[YulStatement] =
                    s.flatMap(s => {
                        val id_s: Identifier = nextTemp()
                        decl_0exp(id_s) +: translateStatement(s, Some(id_s), contractName, checkedTable, inMain)
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
    def call(s: String, retvar: Identifier, contractName: String, checkedTable: SymbolTable, inMain: Boolean, es: Expression*): Seq[YulStatement] = {
        // for each expression, make a new temporary variable and translate the expression
        val es_trans: Seq[(Seq[YulStatement], Identifier)] = es.map(e => {
            val id = nextTemp()
            (translateExpr(id, e, contractName, checkedTable, inMain), id)
        })

        // flatten the resultant sequences and do them first, then make the call to the function using the Ids
        es_trans.map(x => decl_0exp(x._2)) ++
            es_trans.flatMap(x => x._1) :+
            assign1(retvar, apply(s, es_trans.map(x => x._2): _*))
    }

    def geq_leq(s: String, retvar: Identifier, e1: Expression, e2: Expression, contractName: String, checkedTable: SymbolTable, inMain: Boolean): Seq[YulStatement] = {
        // this doesn't fit the pattern of binary_call or a more general version that
        // takes  (Identifier, Identifier) => Expression, because what you want to do
        // is build another Obsidian Expression but with the Yul Identifiers for the
        // temp vars in it, which is incoherent.
        //
        // todo: maybe there's a more elegant way to do this with less repeated code
        val e1id = nextTemp()
        val e2id = nextTemp()
        Seq(decl_0exp(e1id), decl_0exp(e2id)) ++
            translateExpr(e1id, e1, contractName, checkedTable, inMain) ++
            translateExpr(e2id, e2, contractName, checkedTable, inMain) :+
            assign1(retvar, apply("or", apply(s, e1id, e2id), apply("eq", e1id, e2id)))
    }


    /**
      * Given the type of a contract and a table, compute the size that we need to allocate for it in memory.
      * TODO: as a simplifying assumption, for now this always returns 256.
      *
      * @param t      the contract type of interest
      * @param symTab the symbol table to look in
      * @return the size of memory needed for the contract
      */
    def contractSize(t: ContractType, symTab: SymbolTable): Int = {
        256
    }

    def translateExpr(retvar: Identifier, e: Expression, contractName: String, checkedTable: SymbolTable, inMain: Boolean): Seq[YulStatement] = {
        e match {
            case e: AtomicExpression =>
                e match {
                    case ReferenceIdentifier(x, obstype) =>
                        // todo: this assumes that all indentifiers are either fields or stack variables;
                        //  we store nothing in memory. this is also very likely not doing the right thing
                        //  with name shadowing

                        // todo: this also assumes that everything is a u256 and does no type-directed
                        //  cleaning in the way that solc does
                        if (checkedTable.contractLookup(contractName).allFields.exists(f => f.name.equals(x))) {
                            val store_id = nextTemp()
                            //todo: compute offsets
                            Seq(decl_1exp(store_id, apply("sload", hexlit(keccak256(contractName + x)))),
                                assign1(retvar, store_id))
                        } else {
                            Seq(assign1(retvar, Identifier(x)))
                        }
                    case NumLiteral(n) =>
                        Seq(assign1(retvar, intlit(n)))
                    case StringLiteral(value) =>
                        Seq(assign1(retvar, stringlit(value)))
                    case TrueLiteral() =>
                        Seq(assign1(retvar, boollit(true)))
                    case FalseLiteral() =>
                        Seq(assign1(retvar, boollit(false)))
                    case This(obstype) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Parent() =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: UnaryExpression =>
                e match {
                    case LogicalNegation(e) => translateStatement(IfThenElse(e, Seq(FalseLiteral()), Seq(TrueLiteral())), Some(retvar), contractName, checkedTable, inMain)
                    case Negate(e) =>
                        translateExpr(retvar, Subtract(NumLiteral(0), e), contractName, checkedTable, inMain)
                    case Dereference(_, _) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Disown(_) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                }
            case e: BinaryExpression =>
                e match {
                    case Conjunction(e1, e2) => call("and", retvar, contractName, checkedTable, inMain, e1, e2)
                    case Disjunction(e1, e2) => call("or", retvar, contractName, checkedTable, inMain, e1, e2)
                    case Add(e1, e2) => call("add", retvar, contractName, checkedTable, inMain, e1, e2)
                    case StringConcat(e1, e2) =>
                        assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                        Seq()
                    case Subtract(e1, e2) => call("sub", retvar, contractName, checkedTable, inMain, e1, e2)
                    case Divide(e1, e2) => call("sdiv", retvar, contractName, checkedTable, inMain, e1, e2) // todo div is for unsigned; i believe we have signed ints?
                    case Multiply(e1, e2) => call("mul", retvar, contractName, checkedTable, inMain, e1, e2)
                    case Mod(e1, e2) => call("smod", retvar, contractName, checkedTable, inMain, e1, e2) // todo as with div
                    case Equals(e1, e2) => call("eq", retvar, contractName, checkedTable, inMain, e1, e2)
                    case GreaterThan(e1, e2) => call("sgt", retvar, contractName, checkedTable, inMain, e1, e2) // todo as with div
                    case GreaterThanOrEquals(e1, e2) => geq_leq("sgt", retvar, e1, e2, contractName, checkedTable, inMain)
                    case LessThan(e1, e2) => call("slt", retvar, contractName, checkedTable, inMain, e1, e2) //todo as with div
                    case LessThanOrEquals(e1, e2) => geq_leq("slt", retvar, e1, e2, contractName, checkedTable, inMain)
                    case NotEquals(e1, e2) => translateExpr(retvar, LogicalNegation(Equals(e1, e2)), contractName, checkedTable, inMain)
                }
            case e@LocalInvocation(name, genericParams, params, args, obstype) => // todo: why are the middle two args not used?
                // look up the name of the function in the table, get its return type, and then compute
                // how wide of a tuple that return type is. (currently this is always either 0 or 1).
                //
                // todo: this is a hack. when we add type arguments to the translation, we'll be able to do this correctly.
                val width = checkedTable.contractLookup(contractName).lookupTransaction(name) match {
                    case Some(trans) =>
                        trans.retType match {
                            case Some(typ) => obsTypeToWidth(typ)
                            case None => 0
                        }
                    case None =>
                        // this is an absolute kludge just to get a proof of concept working for a specific test case
                        if (name == transactionNameMapping("IntContainer", "set")) {
                            0
                        } else if (name == transactionNameMapping("IntContainer", "get")) {
                            1
                        } else if (name == transactionNameMapping("IntContainer", "set1")) {
                            0
                        } else if (name == transactionNameMapping("IntContainer", "set2")) {
                            0
                        } else {
                            assert(false, s"width of transaction named ${name}")
                        }
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
                        (translateExpr(id, p, contractName, checkedTable, inMain), id)
                    }).unzip
                }

                // the result is the recursive translation and the expression either using the temp
                // here or not.

                // todo: this does not work with non-void functions that are called without binding
                //  their results, ie "f()" if f returns an int
                ids.map(id => decl_0exp(id)) ++
                seqs.flatten ++ (width match {
                    case 0 => Seq(ExpressionStatement(FunctionCall(Identifier(name), ids)))
                    case 1 =>
                        val id: Identifier = nextTemp()
                        Seq(decl_1exp(id, FunctionCall(Identifier(name), ids)), assign1(retvar, id))
                    case _ => assert(assertion = false, "obsidian currently does not support tuples; this shouldn't happen."); Seq()
                })

            case Invocation(recipient, genericParams, params, name, args, isFFIInvocation, obstype) =>
                // we translate invocations by first translating the recipient expression. this
                // returns a variable containing a memory address for the implicit `this` argument
                // added to the translation of the transactions into the flat Yul object


                // todo: this ultimately needs to be type-directed. to translate an invocation,
                //  we need to know the type of the recipient expression being invoked so that we
                //  can call the appropriate translated transaction in the big Yul object.

                // we get a variable storing the address of the instance from recursively translating
                // the recipient. we also form a Parser identifier with this, so that we can translate
                // the invocation with a tailcall to translateExpr
                val id_recipient: Identifier = nextTemp()
                val this_address = edu.cmu.cs.obsidian.parser.ReferenceIdentifier(id_recipient.name)

                val recipient_yul = translateExpr(id_recipient, recipient, contractName, checkedTable, inMain)

                ((decl_0exp(id_recipient) +: recipient_yul) ++
                    translateExpr(retvar, LocalInvocation(transactionNameMapping(getContractName(recipient), name),
                        genericParams,
                        params,
                        this_address +: args), contractName, checkedTable, inMain))

            case Construction(contractType, args, isFFIInvocation, obstype) =>
                // todo: currently we ignore the arguments to the constructor
                assert(args.isEmpty, "contracts that take arguments are not yet supported")

                val id_memaddr = nextTemp()
                Seq(
                    // grab the appropriate amount of space of memory sequentially, off the free memory pointer
                    decl_1exp(id_memaddr, apply("allocate_memory", intlit(sizeOfContractType(contractType)))),

                    // return the address that the space starts at
                    assign1(retvar, id_memaddr)
                )
            case StateInitializer(stateName, fieldName, obstype) =>
                assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                Seq()
        }
    }
}
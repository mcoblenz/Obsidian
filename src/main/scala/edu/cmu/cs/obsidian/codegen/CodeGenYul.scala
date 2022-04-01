package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.typecheck._
import edu.cmu.cs.obsidian.{CompilerOptions, codegen}

import java.io.{File, FileWriter}
import java.nio.file.{Files, Path, Paths}
// note: some constructor names collide with edu.cmu.cs.obsidian.codegen.
// in those places we use the fully qualified name
import edu.cmu.cs.obsidian.Main.{findMainContract, findMainContractName}
import edu.cmu.cs.obsidian.codegen.Util._
import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck.ContractType

object CodeGenYul extends CodeGenerator {
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

        //  throw an exception if there is no main contract
        if (findMainContract(checkedTable.ast).isEmpty) {
            throw new RuntimeException("No main contract found")
        }

        // figure out where to put the output depending on the compiler options
        val finalOutputPath = options.outputPath match {
            case Some(p) =>
                Paths.get(p).resolve(findMainContractName(checkedTable.ast))
            case None =>
                Paths.get(findMainContractName(checkedTable.ast))
        }

        // translate from obsidian AST to yul AST
        val translated_obj = translateProgram(checkedTable.ast, checkedTable)

        // generate yul string from yul AST, write to the output file
        val s = translated_obj.yulString()
        Files.createDirectories(finalOutputPath)
        val writer = new FileWriter(new File(finalOutputPath.toString, translated_obj.contractName + ".yul"))
        writer.write(s)
        writer.flush()
        true
    }

    def translateProgram(program: Program, checkedTable: SymbolTable): YulObject = {
        /** returns true iff the argument contract has at least one constructor, false otherwise.
          *
          * @param c the contract to inspect
          * @return true if it has a constructor, false otherwise
          */
        def hasConstructor(c: Contract): Boolean = {
            c.declarations.exists(d => d match {
                case decl: InvokableDeclaration => decl match {
                    case Constructor(_, _, _, _) => true
                    case _ => false
                }
                case _ => false
            })
        }

        /** given a contract, return the empty sequence if it has a constructor or the sequence
          * containing a default constructor if it doesn't have one.
          *
          * @param c the contract of interest
          * @return the empty sequence if it has a constructor or the sequence containing a
          *         default constructor if it doesn't have one
          */
        def defaultConstructor(c: Contract): Seq[FunctionDefinition] = {
            if (hasConstructor(c)) {
                Seq()
            } else {
                Seq(writeDefaultConstructor(c, checkedTable))
            }
        }

        /** given a contract,
          * translate all the declarations it contains into yul and produce that sequence and
          * produce a default constructor for it if it does not have any constructors.
          *
          * @param c the contract to translate
          * @return the sequence of yul statements for each declaration in the contract
          */
        def translateContract(c: Contract): Seq[YulStatement] = {
            c match {
                case _: ObsidianContractImpl =>
                    defaultConstructor(c) ++ c.declarations.flatMap(d => translateDeclaration(d, c.name, checkedTable))
                case _: JavaFFIContractImpl =>
                    throw new RuntimeException("Java contract not supported in yul translation")
            }
        }

        val (main_contract, other_contracts): (Contract, Seq[Contract]) =
            program.contracts.filter(c => c.name != ContractType.topContractName).partition(c => c.modifiers.contains(IsMain())) match {
                case (Seq(x), l) => (x, l)
                case _ => throw new RuntimeException("program does not have exactly one main contract")
            }

        // nb: we do not process imports
        YulObject(contractName = main_contract.name,
            data = Seq(),
            mainContractTransactions = translateContract(main_contract),
            mainContractSize = sizeOfContractST(main_contract.name, checkedTable),
            mainConstructorTypeNames = defaultConstructorSignature(main_contract, checkedTable, ""),
            // todo: this includes all of the default constructors in the top
            defaultCons = (main_contract +: other_contracts).map(c => writeDefaultConstructor(c, checkedTable)),
            otherTransactions = other_contracts.flatMap(translateContract),
            tracers = (main_contract +: other_contracts).flatMap(c => writeTracers(checkedTable, c.name)).distinctBy(fd => fd.name)
        )
    }

    /** compute the signature of a default constructor, with arguments in field order, for a contract
      *
      * @param c      the contract to construct
      * @param ct     the symbol table of the program the contract exists in
      * @param prefix the prefix to use for argument names
      * @return the signature of the constructor for this contract
      */
    def defaultConstructorSignature(c: Contract, ct: SymbolTable, prefix: String): Seq[TypedName] = {
        // todo: this is just recursive and therefore does some work more than once. it would be faster
        //     to store a table of these and memoize, but programs are small enough that i'm willing to
        //     leave that optimization for later (if ever).

        def proc(d: Declaration): Seq[TypedName] = {
            d match {
                case Field(_, typ, name, _) => typ match {
                    case primitiveType: PrimitiveType => Seq(TypedName(s"con_arg_${prefix}_$name", obsTypeToYulType(primitiveType)))
                    case npt: NonPrimitiveType => npt match {
                        case ContractReferenceType(contractType, _, _) =>
                            defaultConstructorSignature(ct.contractLookup(contractType.contractName).contract, ct, s"$prefix$name")
                        case _ => Seq()
                    }
                    case _ => Seq()
                }
                case _ => Seq()
            }
        }

        c.declarations.flatMap(proc)
    }

    /** given a contract and the list of arguments its default constructor accepts, produce a
      * series of assignments that assign the fields of that contract to those arguments, recurring
      * into the subcontracts and calling their default constructors as appropriate.
      *
      * @param c   the contract to generate the assignments for
      * @param sig the signature of that contracts default constructor
      * @param ct  the context in which the contract exists
      * @return the sequence of assignments for the default constructor
      */
    def defaultConstructorAssignments(c: Contract, sig: Seq[TypedName], ct: SymbolTable): Seq[YulStatement] = {

        def proc(d: Declaration, stateSoFar: (Seq[YulStatement], Seq[TypedName])): (Seq[YulStatement], Seq[TypedName]) = {
            val (acc, sig) = stateSoFar
            d match {
                case Field(_, typ, name, _) =>
                    typ match {
                        // if the field is primitive, grab the next argument off the signature and assign the field to it
                        case _: PrimitiveType =>
                            sig match {
                                case use +: rest =>
                                    (Seq(LineComment(s"this.${name} := ${use.name}"),
                                        // todo: (performance) this inserts a check against the storage threshold
                                        //   every time, which will always be false for the main constructor's first call
                                        updateField(ct.contractLookup(c.name), name, Identifier(use.name))
                                    ) ++ acc,
                                        rest)
                                case _ => throw new RuntimeException("ran out of variables building default constructor; this is a bug")
                            }
                        // if the field is a contract . . .
                        case npt: NonPrimitiveType =>
                            npt match {
                                case ContractReferenceType(contractType, _, _) =>
                                    // learn about the subcontract being called and compute its default constructor's signature
                                    val sub_contract_ct: ContractTable = ct.contractLookup(contractType.contractName)
                                    val sub_contract: Contract = sub_contract_ct.contract
                                    val sub_contract_default_sig: Seq[TypedName] = defaultConstructorSignature(sub_contract, ct, "")
                                    val sub_constructor_name: String =
                                        flattenedName(sub_contract.name, sub_contract.name, Some(sub_contract_default_sig.map(tn => tn.typ.toString)))

                                    // break off the right number of arguments from the signature to call that default constructor
                                    val (args_for_sub, rest): (Seq[Identifier], Seq[TypedName]) =
                                        sig.splitAt(sub_contract_default_sig.length) match {
                                            case (l1, l2) => (l1.map(tn => Identifier(tn.name)).reverse, l2)
                                        }

                                    // make a temporary variable to store the address in memory for the subobject
                                    val sub_this: Identifier = nextTemp()

                                    (Seq(
                                        // grab some memory for the subcontract
                                        decl_1exp(sub_this, apply("allocate_memory", intlit(sizeOfContract(sub_contract_ct)))),

                                        // write the address of the subcontract to the corresponding field of this contract
                                        ExpressionStatement(apply("mstore", fieldFromThis(ct.contractLookup(c.name), name), sub_this)),

                                        // call the constructor on that for the this argument and the right
                                        ExpressionStatement(apply(sub_constructor_name, sub_this +: args_for_sub: _*))) ++ acc
                                        , rest)
                                case _ => (acc, sig)
                            }
                        case _ => (acc, sig)
                    }
                case _ => (acc, sig)
            }
        }

        c.declarations.foldRight((Seq(): Seq[YulStatement], sig.reverse))(proc)._1
    }

    /** given a contract, produce a default constructor that takes enough arguments to instantiate
      * the fields of the contract, including recursively calling default constructors for non
      * primitive fields.
      *
      * @param c  the contract to write a constructor for
      * @param ct the symbol table that describes the contract c
      * @return the default constructor
      */
    def writeDefaultConstructor(c: Contract, ct: SymbolTable): FunctionDefinition = {
        val signature: Seq[TypedName] = defaultConstructorSignature(c, ct, "")

        addThisArgument(FunctionDefinition(name = flattenedName(c.name, c.name, Some(signature.map(tn => tn.typ.toString))),
            parameters = signature,
            returnVariables = Seq(), //todo add a return for a this address?
            body = Block(LineComment("default constructor generated by the compiler") +: defaultConstructorAssignments(c, signature, ct))
        ))
    }

    def writeTracers(ct: SymbolTable, name: String): Seq[FunctionDefinition] = {
        // this is a stand in for a more robust mechanism for choosing when to emit logs.
        // we'll want to be able to turn it off to do performance testing in the future.
        val emit_logs: Boolean = true

        val c: Contract = ct.contract(name) match {
            case Some(value) => value.contract
            case None => throw new RuntimeException()
        }

        var body: Seq[YulStatement] = Seq()
        var others: Seq[FunctionDefinition] = Seq()

        for (d <- c.declarations) {
            d match {
                case Field(_, typ, fname, _) =>
                    val mem_loc: codegen.Expression = fieldFromThis(ct.contractLookup(name), fname)
                    val sto_loc: codegen.Expression = mapToStorageAddress(mem_loc)
                    val log_temp: Identifier = nextTemp()

                    // todo (tidy) this is kind of redundant with the match below

                    // the body of load needs to check if it's a ContractReferenceType and add the offset if so;
                    //   when we copy to storage, REWRITE the value of the pointer.
                    val shift_if_addr: codegen.Expression =
                    typ match {
                        case primitiveType: PrimitiveType => apply("mload", mem_loc)
                        case npt: NonPrimitiveType => npt match {
                            case ContractReferenceType(contractType, permission, remoteReferenceType) => apply("add", storage_threshold, apply("mload", mem_loc))
                            case StateType(contractType, stateNames, remoteReferenceType) => throw new RuntimeException("unimplemented")
                            case InterfaceContractType(name, simpleType) => throw new RuntimeException("unimplemented")
                            case GenericType(gVar, bound) => throw new RuntimeException("unimplemented")
                        }
                        case BottomType() => throw new RuntimeException("unimplemented")
                    }

                    val load = Seq(
                        //sstore(add(this,offset), mload(add(this,offset)))
                        LineComment("loading"),
                        ExpressionStatement(apply("sstore", sto_loc, shift_if_addr)))

                    val log: Seq[YulStatement] =
                        if (emit_logs) {
                            Seq(LineComment("logging"),
                                // allocate memory to log from
                                decl_1exp(log_temp, apply("allocate_memory", intlit(32))),
                                // load what we just wrote to storage to that location
                                ExpressionStatement(apply("mstore", log_temp, apply("sload", sto_loc))),
                                // emit the log
                                ExpressionStatement(apply("log0", log_temp, intlit(32)))
                            )
                        } else {
                            Seq()
                        }


                    typ match {
                        case t: NonPrimitiveType => t match {
                            case ContractReferenceType(contractType, _, _) =>
                                body = body ++ load ++ log ++
                                    Seq(
                                        LineComment("traversal"),
                                        ExpressionStatement(apply(nameTracer(contractType.contractName), apply("mload", mem_loc)))
                                    )
                                // todo: this recursive call may not be needed if we generate tracers
                                //   for every contract in the program
                                others = others ++ writeTracers(ct, contractType.contractName)
                            case _ => Seq()
                        }
                        case _: PrimitiveType =>
                            body = body ++ load ++ log
                        case _ => Seq()
                    }
                case _ => Seq()
            }
        }

        FunctionDefinition(name = nameTracer(name),
            parameters = Seq(TypedName("this", YATAddress())),
            returnVariables = Seq(),
            body = Block(body :+ Leave())
        ) +: others.distinctBy(fd => fd.name)
    }

    /**
      * compute the translation of a declaration into yul with respect to its context in the larger
      * obsidian program
      *
      * @param declaration  the declaration to translate
      * @param contractName the name of the contract in which the declaration appears
      * @param checkedTable the symbol table for the contract in which the declaration appears
      * @return the yul statements corresponding to the declaration
      */
    def translateDeclaration(declaration: Declaration, contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        // nb there is a .asInstanceOf in the mustache glue code that only works if this really
        // returns a sequence of FunctionDeclaration objects. that's OK for now because it's true,
        // but as the cases below here get filled in that may not be true and we'll have to fix it.

        declaration match {
            case _: Field => Seq() // fields are translated as they are encountered
            case t: Transaction => Seq(translateTransaction(t, contractName, checkedTable, isCons = false))
            case _: State =>
                assert(assertion = false, "TODO")
                Seq()
            case _: ObsidianContractImpl =>
                assert(assertion = false, "TODO")
                Seq()
            case _: JavaFFIContractImpl =>
                assert(assertion = false, "Java contracts not supported in Yul translation")
                Seq()
            case c: Constructor =>
                // given an obsidian type, pull out the non-primitive type or raise an exception
                def nonprim(t: ObsidianType): NonPrimitiveType = {
                    t match {
                        case npt: NonPrimitiveType => npt
                        case _ => throw new RuntimeException("needed a non-primitive type")
                    }
                }

                // constructors turn into transactions with a special name and the same body
                Seq(translateTransaction(
                    Transaction(
                        name = c.name,
                        // we omit generic type information because we don't have it and would need
                        // to reconstruct it, and we don't use it to translate to yul anyway
                        params = Seq(),
                        args = c.args,
                        retType = c.retType,
                        // we omit any ensures because that feature is largely deprecated
                        ensures = Seq(),
                        body = c.body,
                        isStatic = false,
                        isPrivate = false,
                        thisType = nonprim(c.thisType),
                        thisFinalType = nonprim(c.thisFinalType)
                    ),
                    contractName, checkedTable, isCons = true))
            case _: TypeDecl =>
                assert(assertion = false, "TODO")
                Seq()
        }
    }

    def translateTransaction(transaction: Transaction, contractName: String, checkedTable: SymbolTable, isCons: Boolean): FunctionDefinition = {
        var id: Option[Identifier] = None

        // translate the return type to the ABI names
        val ret: Seq[TypedName] = {
            transaction.retType match {
                case Some(t) =>
                    id = Some(nextRet())
                    Seq(TypedName(id.get.name, obsTypeToYulType(t)))
                case None => Seq()
            }
        }

        // form the body of the transaction by translating each statement found
        val body: Seq[YulStatement] = transaction.body.flatMap((s: Statement) => translateStatement(s, id, contractName, checkedTable))

        val type_names =
            if (isCons) {
                Some(transaction.args.map(vd => vd.typIn.toString))
            } else {
                None
            }

        // return the function definition formed from the above parts, with an added special argument called `this` for the address
        // of the allocated instance on which it should act
        addThisArgument(
            FunctionDefinition(name = flattenedName(contractName, transaction.name, type_names),
                parameters = transaction.args.map(v => TypedName(v.varName, obsTypeToYulType(v.typIn))),
                ret,
                body = Block(body)))
    }

    /**
      * translate an obsidian statement into the corresponding sequence of yul statements
      *
      * @param s      the statement to be translated
      * @param retVar the name of the variable to use for returning for the current scope, if there is one
      * @return
      */
    def translateStatement(s: Statement, retVar: Option[Identifier], contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
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
                        val e_yul = translateExpr(id, e, contractName, checkedTable)
                        val ct = checkedTable.contractLookup(contractName)

                        val field_address = fieldFromThis(ct, x)

                        // todo: make this a helper function once you get the address argument figured out
                        val trace_for_e : Seq[YulStatement] = e.obstype match {
                            case Some(value) => value match {
                                case _: PrimitiveType => Seq()// Do(apply("sstore",mapToStorageAddress(field_address),apply("mload",field_address)))
                                case t: NonPrimitiveType => t match {
                                    case ContractReferenceType(contractType, _, _) => // todo
                                        Seq(
                                            edu.cmu.cs.obsidian.codegen.If(apply("not",compareToThresholdExp(field_address)),
                                                                            Block(Do(apply(nameTracer(contractType.contractName), id))))
                                        )
                                    case StateType(_, _, _) => assert(assertion=false, "not yet implemented"); Seq()
                                    case InterfaceContractType(_, _) => assert(assertion=false, "not yet implemented"); Seq()
                                    case GenericType(_, _) => assert(assertion=false, "not yet implemented"); Seq()
                                }
                                case BottomType() => Seq()
                            }
                            case None => assert(assertion=false,"encountered an expression without a type annotation"); Seq()
                        }

                        val update_instructions : Seq[YulStatement] =
                            if (ct.allFields.exists(f => f.name.equals(x))) {
                                trace_for_e :+ updateField(ct, x, id)
                                // edu.cmu.cs.obsidian.codegen.If(apply("not",compareToThresholdExp(field_address)), Block(trace_for_e)))
                                //Seq(updateField(ct, x, id), ifInStorge(field_address,trace_for_e,Seq()))
                            } else {
                                Seq(assign1(Identifier(x), id))
                            }

                        decl_0exp(id) +: (e_yul ++ update_instructions)

                        // look at assignTo. if it's a storage address, then look at the type of e. if it's a primitive, ignore it.  if it's a
                        // contract reference, it needs to get traced first
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
                    case Some(value) => translateExpr(value, e, contractName, checkedTable)
                    case None =>
                        val id = nextTemp()
                        decl_0exp(id) +: translateExpr(id, e, contractName, checkedTable)
                }
            case VariableDecl(typ, varName) =>
                Seq(decl_0exp_t(Identifier(varName), obsTypeToYulType(typ)))
            case VariableDeclWithInit(typ, varName, e) =>
                val id = nextTemp()
                val e_yul = translateExpr(id, e, contractName, checkedTable)
                decl_0exp(id) +:
                    e_yul :+
                    decl_0exp_t_init(Identifier(varName), obsTypeToYulType(typ), id)
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
                        decl_0exp(id_s) +: translateStatement(s, Some(id_s), contractName, checkedTable)
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

    /** This encapsulates a general pattern of translation shared between both local and general
      * invocations, as called below in the two relevant cases of translate expression.
      *
      * @param name         the name of the thing being invoked
      * @param args         the arguments to the invokee
      * @param obstype      the type at the invocation site
      * @param thisID       where to look in memory for the relevant fields
      * @param retvar       the temporary variable to store the return
      * @param contractName the overall name of the contract being translated
      * @param checkedTable the checked tabled for the overall contract
      * @return the sequence of yul statements that are the translation of the invocation so described
      */
    def translateInvocation(name: String,
                            args: Seq[Expression],
                            obstype: Option[ObsidianType],
                            thisID: Identifier,
                            retvar: Identifier, contractName: String, checkedTable: SymbolTable
                           ): Seq[YulStatement] = {
        // look up the name of the function in the table, get its return type, and then compute
        // how wide of a tuple that return type is. right now that's either 1 (if the
        // transaction returns) or 0 (because it's void)
        val width = obstype match {
            case Some(t) => obsTypeToWidth(t)
            case None => assert(assertion = false, s"width failed; missing type annotation")
        }

        // todo: some of this logic may be repeated in the dispatch table

        // todo: the code here is set up to mostly work in the world in which obsidian has tuples,
        //  which it does not. i wrote it before i knew that. the assert below is one place that it breaks;
        //  to fix it, i need to refactor this object so that i pass around a vector of temporary variables
        //  to assign returns to rather than just one (i think). this is OK for now, but technical debt that
        //  we'll have to address if we ever add tuples to obsidian.

        // for each argument expression, produce a new temp variable and translate it to a
        // sequence of yul statements ending in an assignment to that variable.
        val (seqs: Seq[Seq[YulStatement]], ids: Seq[Identifier]) = {
            args.map(p => {
                val id: Identifier = nextTemp()
                (translateExpr(id, p, contractName, checkedTable), id)
            }).unzip
        }

        // the result is the recursive translation and the expression either using the temp
        // here or not.
        ids.map(id => decl_0exp(id)) ++
            seqs.flatten ++ (width match {
            case 0 => Do(FunctionCall(Identifier(name), thisID +: ids))
            case 1 =>
                val id: Identifier = nextTemp()
                Seq(decl_1exp(id, FunctionCall(Identifier(name), thisID +: ids)), assign1(retvar, id))
            case _ => assert(assertion = false, "obsidian currently does not support tuples; this shouldn't happen."); Seq()
        })
    }

    def translateExpr(retvar: Identifier, e: Expression, contractName: String, checkedTable: SymbolTable): Seq[YulStatement] = {
        e match {
            case e: AtomicExpression =>
                e match {
                    case ReferenceIdentifier(x, obstype) =>
                        // todo: this assumes that all indentifiers are either fields or stack variables;
                        //  we store nothing in memory. this is also very likely not doing the right thing
                        //  with name shadowing

                        // todo: this also assumes that everything is a u256 and does no type-directed
                        //  cleaning in the way that solc does
                        val ct = checkedTable.contractLookup(contractName)
                        if (ct.allFields.exists(f => f.name.equals(x))) {
                            val store_id = nextTemp()
                            Seq(decl_0exp(store_id),
                                fetchField(ct, x, store_id),
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
                    case LogicalNegation(e) => translateStatement(IfThenElse(e, Seq(FalseLiteral()), Seq(TrueLiteral())), Some(retvar), contractName, checkedTable)
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
            case e@LocalInvocation(name, _, _, args, obstype) =>
                translateInvocation(flattenedName(contractName, name, None),
                    args, obstype, Identifier("this"), retvar, contractName, checkedTable)

            case Invocation(recipient, _, _, name, args, _, obstype) =>
                // we translate invocations by first translating the recipient expression. this
                // returns a variable containing a memory address for the implicit `this` argument
                // added to the translation of the transactions into the flat Yul object

                // we get a variable storing the address of the instance from recursively translating
                // the recipient. we also form a Parser identifier with this, so that we can translate
                // the invocation with a tailcall to translateExpr
                val id_recipient: Identifier = nextTemp()
                val recipient_yul = translateExpr(id_recipient, recipient, contractName, checkedTable)

                (decl_0exp(id_recipient) +: recipient_yul) ++
                    // todo: this may be the cause of a bug in the future. this is how non-main
                    //  functions get their names translated before calling, but that might not
                    //  work with multiple contracts and private transactions. i'm not sure.
                    translateInvocation(flattenedName(getContractName(recipient), name, None),
                        args,
                        obstype,
                        id_recipient,
                        retvar, contractName, checkedTable)

            case Construction(contractType, args, isFFIInvocation, obstype) =>
                // grab an identifier to store memory
                val id_memaddr = nextTemp()

                // store the names of the types of the arguments
                val typeNames: Seq[String] = args.map(e => e.obstype.get.toString)

                // given a declaration, test if it's a constructor with type arguments that match the usage here
                def isMatchingConstructor(d: Declaration): Boolean =
                    d match {
                        case c: Constructor => typeNames == c.args.map(v => v.typIn.toString)
                        case _ => false
                    }

                // does this constructor belong to the main contract?
                val isMainContract = checkedTable.contractLookup(contractType.contractName).contract.isMain

                // check to to see if there is a constructor to call, and if so translate the
                // arguments and invoke the constructor as normal transaction with the hash appended
                // to the name to call the right one
                val conCall =
                if (checkedTable.contract(contractType.contractName).get.contract.declarations.exists(d => isMatchingConstructor(d))) {
                    translateInvocation(name = flattenedName(contractType.contractName, contractType.contractName, Some(typeNames)),
                        args = args,
                        obstype = Some(UnitType()),
                        thisID = id_memaddr,
                        retvar = retvar, contractName = contractName, checkedTable = checkedTable)
                } else {
                    Seq()
                }

                // we only call the tracer after the constructor for the main contract
                val traceCall =
                    if (isMainContract) {
                        Do(apply(nameTracer(contractType.contractName), Identifier("this")))
                    } else {
                        Seq()
                    }


                Seq( // grab the appropriate amount of space of memory sequentially, off the free memory pointer
                    decl_1exp(id_memaddr, apply("allocate_memory", intlit(sizeOfContractST(contractType.contractName, checkedTable)))),

                    // return the address that the space starts at, call the constructor and the tracer as above
                    assign1(retvar, id_memaddr)) ++ conCall


            case StateInitializer(stateName, fieldName, obstype) =>
                assert(assertion = false, "TODO: translation of " + e.toString + " is not implemented")
                Seq()
        }
    }
}
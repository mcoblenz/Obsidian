package edu.cmu.cs.obsidian.codegen

import CodeGen._
import edu.cmu.cs.obsidian.parser._
import com.helger.jcodemodel._
import edu.cmu.cs.obsidian.parser
import edu.cmu.cs.obsidian.parser.Parser.Identifier
import edu.cmu.cs.obsidian.util._
import edu.cmu.cs.obsidian.typecheck._

import scala.collection.{mutable, _}
import collection.JavaConverters._


trait Target {
    val generateDebugOutput: Boolean
}

// We have to keep track of which contract is the main client contract because some of the imported contracts may be main contracts for server processes.
case class Client(mainContract: Contract, generateDebugOutput: Boolean = false) extends Target
case class Server(mainContract: Contract, generateDebugOutput: Boolean = false) extends Target

class CodeGen (val target: Target, table: SymbolTable) {

    private val model: JCodeModel = new JCodeModel()

    /* we must keep track of the transactions in main so that we can match on
     * them in the "invoke" function. */
    private val mainTransactions: mutable.Set[Transaction] =
        new mutable.HashSet[Transaction]()

    /* naming conventions for various generated Java constructs */
    private final val stateField: String = "__state"
    private final val getStateMeth: String = "getState"
    private final val guidFieldName: String = "__guid"
    private final val modifiedFieldName: String = "__modified"
    private final val loadedFieldName: String = "__loaded"
    private final val serializationParamName: String = "__st"
    private final val constructorReturnsOwnedFieldName: String = "__constructorReturnsOwned"
    private final val constructorReturnsOwnedReferenceMethName = "constructorReturnsOwnedReference"

    private def stateEnumNameForClassName(className: String): String = {
        "State_" + className
    }

    private def fieldGetMethodName(fieldName: String): String = {
        "__getField__" + fieldName
    }

    private def fieldSetMethodName(fieldName: String): String = {
        "__setField__" + fieldName
    }

    private def innerClassName(stName: String): String = {
        "State_" + stName
    }

    private def innerClassFieldName(stName: String): String = {
        "__state" + stName
    }

    private def isInsideInvocationFlag(): JFieldRef = {
        JExpr.ref("__isInsideInvocation")
    }

    private def invokeGetState(receiver: IJExpression, loadSerialization: Boolean): JInvocation = {
        if (loadSerialization) {
            JExpr.invoke(receiver, getStateMeth).arg(JExpr.ref(serializationParamName))
        } else {
            JExpr.invoke(receiver, getStateMeth).arg(JExpr._null())
        }
    }

    private def transactionGetMethodName(txName: String, stOption: Option[String]): String = {
        stOption match {
            case Some(stName) => txName + "__" + "state" + stName
            case None => txName
        }
    }

    /* This method dynamically checks type state and copies the value of conserved fields
     * (i.e. defined in both states) from one state to another */
    private final val conserveFieldsName = "__conserveFields"
    /* based on the result of [getStateMeth], this nulls out the appropriate state field
     * so that the old state can be garbage-collected after a transition */
    private final val deleteOldStateName = "__oldStateToNull"

    final val packageName: String = "org.hyperledger.fabric.example"

    def populateProtobufOuterClassNames(contract: Contract,
                                        protobufOuterClassName: String,
                                        contractNameResolutionMap: Map[Contract, String],
                                        protobufOuterClassNames: mutable.HashMap[String, String]): Unit = {
        protobufOuterClassNames += (contractNameResolutionMap(contract) -> (packageName + "." + protobufOuterClassName))

        for (d <- contract.declarations if d.isInstanceOf[ObsidianContractImpl]) {
            val innerContract = d.asInstanceOf[ObsidianContractImpl]
            populateProtobufOuterClassNames(innerContract, protobufOuterClassName, contractNameResolutionMap, protobufOuterClassNames)
        }
    }

    def translateProgram(program: Program,
                         protobufOuterClassName: String): JCodeModel = {
        // Put all generated code in the same package.
        val programPackage: JPackage = model._package(packageName)
        translateProgramInPackage(program, protobufOuterClassName, programPackage)
    }

    private def translateProgramInPackage(program: Program,
                                          protobufOuterClassName: String,
                                          programPackage: JPackage): JCodeModel = {
        val contractNameResolutionMap: Map[Contract, String] = TranslationContext.contractNameResolutionMapForProgram(program)
        val protobufOuterClassNames = mutable.HashMap.empty[String, String]

        assert(program.imports.isEmpty, "Imports should be empty after processing.")

        /* again match on c to check if it is a ObsidianContract or a javaFFIContract
         */
        for (c <- program.contracts) {
            c match {
                case obsContract: ObsidianContractImpl => populateProtobufOuterClassNames(obsContract, protobufOuterClassName, contractNameResolutionMap, protobufOuterClassNames)
                case javaContract: JavaFFIContractImpl =>
            }
        }

        /* match on c to check which type of contract it is */
        for (c <- program.contracts) {
            // TODO : generate code for interfaces (issue #117)

            c match {

                case c: ObsidianContractImpl => {
                    if (!c.isInterface) {
                        val newClass: JDefinedClass = programPackage._class(c.name)
                        val translationContext = makeTranslationContext(c, newClass, contractNameResolutionMap, protobufOuterClassNames, false)
                        translateOuterContract(c, programPackage, protobufOuterClassName, contractNameResolutionMap, protobufOuterClassNames, translationContext, newClass)

                        if (c.isImport) {
                            translateStubContract(c, programPackage, translationContext)
                        }
                    }
                }

                case c: JavaFFIContractImpl =>
            }

        }
        model
    }


    /* [true] iff the contract [c] has a constructor that takes no args */
    private def hasEmptyConstructor(c: Contract): Boolean = {
        for (d <- c.declarations) {
            d match {
                case con: Constructor => if (con.args.isEmpty) return true
                case _ => ()
            }
        }
        return false
    }

    private def classNameForStub(contractName: String) = {
        contractName + "__Stub__"
    }

    private def translateStubContract(contract: Contract,
                                      programPackage: JPackage,
                                      translationContext: TranslationContext): Unit = {
        var contractClass = programPackage._class(JMod.PUBLIC, classNameForStub(contract.name))
        contractClass._extends(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientStub"))
        val constructor = contractClass.constructor(JMod.PUBLIC)
        val connectionManager = constructor.param(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientConnectionManager"), "connectionManager")
        val uuid = constructor.param(classOf[String], "uuid")

        val superConstructorInvocation = constructor.body().invoke("super")
        superConstructorInvocation.arg(connectionManager)
        superConstructorInvocation.arg(uuid)

        generateStateEnumAndFields(contract, contractClass, true)


        val txNames: mutable.Set[String] = new mutable.HashSet[String]()

        for (decl <- contract.declarations) {
            translateStubDeclaration(decl, contractClass, None, txNames, translationContext)
        }
    }

    private def translateStubDeclaration(decl: Declaration,
                                         inClass: JDefinedClass,
                                         stOption: Option[State],
                                         txNames: mutable.Set[String],
                                         translationContext: TranslationContext): Unit = {

        decl match {
            case _: TypeDecl => assert(false, "unsupported"); // TODO
            case f: Field => translateStubField(f, inClass)
            case c: Constructor => // Constructors aren't translated because stubs are only for remote instances.
            case t: Transaction => if (!txNames.contains(t.name)) translateStubTransaction(t, inClass, stOption, translationContext)
                txNames.add(t.name)
            case s: State => translateStubState(s, inClass, txNames, translationContext)
            case c: Contract => translateStubContract(c,
                inClass.getPackage(), translationContext)
        }
    }


    private def translateStubField(decl: Field, newClass: JDefinedClass): Unit = {
        /*
            For now, we don't need any fields. We might change that in the future.

        // In a stub, every non-primitive field is also a stub, so we need to make the field of appropriate type for that.
        // Primitive fields will not map to anything; instead, their accesses will translate to remote calls.
        val fieldType = decl.typ
        fieldType match {
                case np: NonPrimitiveType =>
                    val remoteFieldType = np.remoteType
                    newClass.field(JMod.PRIVATE, resolveType(remoteFieldType), decl.name)
                case _ => ()
        }
        */
    }


    private def marshallExprWithFullObjects(unmarshalledExpr: IJExpression, typ: ObsidianType): IJExpression = {
        val marshalledArg = typ match {
            case IntType() => unmarshalledExpr.invoke("toString");
            case BoolType() =>
                val encoder = model.directClass("java.util.Base64").staticInvoke("getEncoder")
                val bytes = JExpr.cond(unmarshalledExpr, JExpr.ref("TRUE_ARRAY"), JExpr.ref("FALSE_ARRAY"))
                encoder.invoke("encodeToString").arg(bytes)
            case StringType() =>
                unmarshalledExpr
            case _ =>
                val encoder = model.directClass("java.util.Base64").staticInvoke("getEncoder")
                val bytes = unmarshalledExpr.invoke("__wrappedArchiveBytes")
                encoder.invoke("encodeToString").arg(bytes)
        }

        marshalledArg
    }

    // Returns a pair of an error-checking block option and the resulting expression.
    private def unmarshallExprExpectingUUIDObjects(marshalledExpr: IJExpression, typ: ObsidianType, errorBlock: JBlock): IJExpression = {
        typ match {
            case IntType() =>
                val stringClass = model.ref("java.lang.String")
                val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                val stringRepresentation = JExpr._new(stringClass).arg(marshalledExpr).arg(charset)
                val newInt = JExpr._new(model.parseType("java.math.BigInteger"))
                newInt.arg(stringRepresentation)
                newInt
            case BoolType() =>
                val ifLengthIncorrect = errorBlock._if(marshalledExpr.ref("length").eq(JExpr.lit(1)).not())
                val exception = JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientTransactionFailedException"))
                exception.arg("Invalid length of data array for boolean expression.")
                val _ = ifLengthIncorrect._then()._throw(exception)
                marshalledExpr.component(JExpr.lit(0)).eq0()
            case StringType() =>
                val stringClass = model.ref("java.lang.String")
                val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                JExpr._new(stringClass).arg(marshalledExpr).arg(charset)
            // this case encompasses [AstContractType] and [AstStateType]
            case _ =>
                val targetClass = resolveType(typ, table).asInstanceOf[AbstractJClass]
                val constructorInvocation = JExpr._new(targetClass)
                constructorInvocation.arg(JExpr.ref("connectionManager"))

                val stringClass = model.ref("java.lang.String")
                val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                val guidString = JExpr._new(stringClass).arg(marshalledExpr).arg(charset)

                constructorInvocation.arg(guidString)
                constructorInvocation
        }
    }

    private def unmarshallExprExpectingFullObjects(translationContext: TranslationContext,
                                                   body: JBlock,
                                                   marshalledExpr: IJExpression,
                                                   initialTyp: ObsidianType,
                                                   finalTyp: ObsidianType,
                                                   errorBlock: JBlock,
                                                   paramIndex: Integer): IJExpression = {
        initialTyp match {
            case IntType() =>
                val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                val stringType = model.ref("java.lang.String")
                val intAsString = JExpr._new(stringType).arg(marshalledExpr)
                intAsString.arg(charset)
                val intType = resolveType(initialTyp, table)
                val decl = body.decl(intType, "unmarshalledInt" + paramIndex, JExpr._new(intType).arg(intAsString))
                val test = body._if(decl.eq(JExpr._null()))
                val exception = JExpr._new(model.directClass("edu.cmu.cs.obsidian.chaincode.BadArgumentException"))
                exception.arg(intAsString)
                test._then()._throw(exception)
                decl
            case BoolType() =>
                val ifLengthIncorrect = errorBlock._if(marshalledExpr.ref("length").eq(JExpr.lit(1)).not())
                val exception = JExpr._new(model.directClass("edu.cmu.cs.obsidian.chaincode.BadArgumentException"))
                exception.arg(marshalledExpr)
                val _ = ifLengthIncorrect._then()._throw(exception)
                marshalledExpr.component(JExpr.lit(0)).eq0()
            case StringType() =>
                val stringClass = model.ref("java.lang.String")
                val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                JExpr._new(stringClass).arg(marshalledExpr).arg(charset)
            // this case encompasses [AstContractType] and [AstStateType]
            case np: NonPrimitiveType =>
                val contract = resolveNonPrimitiveTypeToContract(np, translationContext, translationContext.contract)
                assert(contract.isDefined)
                val protobufClassName = translationContext.getProtobufClassName(contract.get)
                val archiveType = model.directClass(protobufClassName + "OrGUID")

                val decodedArg = body.decl(model.parseType("byte[]"),
                    "decoded" + paramIndex,
                    model.directClass("java.util.Base64").staticInvoke("getDecoder").invoke("decode").arg(marshalledExpr)
                )


                val archive = body.decl(archiveType, "archive" + paramIndex, archiveType.staticInvoke("parseFrom").arg(decodedArg))

                val enumGetter = "getEitherCase"
                val guidEnumName = protobufClassName + "OrGUID" + "." + "EitherCase" + "." + "GUID"

                val unarchivedObjDecl = body.decl(resolveType(initialTyp, table), "unarchivedObj" + paramIndex)
                // If we have a GUID…
                val hasGUID = archive.invoke(enumGetter).invoke("equals").arg(JExpr.direct(guidEnumName))

                val cond = body._if(hasGUID)

                val stub = JExpr.ref(serializationParamName).invoke("getStub")
                val guid = archive.invoke("getGuid")
                val loadInvocation = JExpr.ref(serializationParamName).invoke("loadContractWithGUID").arg(stub).arg(guid)
                loadInvocation.arg(np.isOwned)
                loadInvocation.arg(finalTyp.isOwned)
                cond._then().assign(unarchivedObjDecl, JExpr.cast(resolveType(initialTyp, table), loadInvocation))

                // If we have an object…
                val targetClass = resolveType(initialTyp, table).asInstanceOf[AbstractJClass]
                val classInstance = JExpr._new(targetClass)

                cond._else().assign(unarchivedObjDecl, classInstance)
                cond._else().invoke(unarchivedObjDecl, "initFromArchive").arg(archive.invoke("getObj")).arg(JExpr.ref(serializationParamName))
                unarchivedObjDecl
            case BottomType() =>
                assert(false)
                JExpr._null()
            case UnitType() =>
                assert(false)
                JExpr._null()
        }
    }


    private def translateStubTransaction(transaction: Transaction,
                                         newClass: JDefinedClass,
                                         stOption: Option[State],
                                         translationContext: TranslationContext): JMethod = {
        val stName = stOption match {
            case Some(st) => Some(st.name)
            case None => None
        }

        //val txName = transactionGetMethodName(transaction.name, stName)

        val obsidianRetType = transaction.retType match {
            case Some(typ) =>
                typ match {
                    case np: NonPrimitiveType => Some(np.remoteType)
                    case _ => Some(typ)
                }
            case None => None
        }

        val javaRetType = obsidianRetType match {
            case Some(retType) => resolveType(retType, table)
            case None => model.VOID
        }

        val meth: JMethod = newClass.method(JMod.PUBLIC, javaRetType, transaction.name)
        meth._throws(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException"))


        var argExpressions: List[IJExpression] = Nil
        /* add args */
        for (arg <- transaction.args) {
            argExpressions = argExpressions :+ meth.param(resolveType(arg.typIn, table), arg.varName)
        }
        /* add SerializationState as argument */
        argExpressions = argExpressions :+ meth.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)

        /* add body */

        // argsArray = new Object[size]
        val body = meth.body()
        val objectArrayType = newClass.owner().ref("java.util.ArrayList").narrow(newClass.owner().ref("String"))
        val newArrayExpr = JExpr._new(objectArrayType)
        newArrayExpr.arg(JExpr.lit(argExpressions.length))
        val argArray = body.decl(objectArrayType, "argArray", newArrayExpr)


        var i = 0;
        // last argument is the serialization state
        for (i <- 0 until argExpressions.length - 1) {
            val unmarshalledArg = argExpressions(i)

            val marshalledArg = marshallExprWithFullObjects(unmarshalledArg, transaction.args(i).typIn)
            val setInvocation = body.invoke(argArray, "add")
            setInvocation.arg(marshalledArg)
        }

        //connectionManager.doTransaction(transaction.name, args)
        val tryBlock = body._try()

        val doTransactionInvocation = JExpr.invoke(JExpr.ref("connectionManager"), "doTransaction")
        doTransactionInvocation.arg(transaction.name)
        doTransactionInvocation.arg(argArray)
        doTransactionInvocation.arg(JExpr.invoke("__getGUID")) // pass UUID so server knows what object to invoke the transaction on
        doTransactionInvocation.arg(transaction.retType.isDefined)

        if (transaction.retType.isDefined) {
            // return result
            val marshalledResultDecl = tryBlock.body().decl(newClass.owner().ref("byte[]"), "marshalledResult", doTransactionInvocation)

            val errorBlock = new JBlock()

            val deserializedArg = unmarshallExprExpectingUUIDObjects(marshalledResultDecl, obsidianRetType.get, errorBlock)

            if (!errorBlock.isEmpty) {
                tryBlock.body().add(errorBlock)
            }

            val resultDecl = tryBlock.body().decl(javaRetType, "result", deserializedArg)
            tryBlock.body()._return(resultDecl)
        }
        else {
            tryBlock.body().add(doTransactionInvocation)
        }
        val ioExceptionCatchBlock = tryBlock._catch(model.directClass("java.io.IOException"))
        ioExceptionCatchBlock.body()._throw(JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException")))

        val failedCatchBlock = tryBlock._catch(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientTransactionFailedException"))
        failedCatchBlock.body()._throw(JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException")))

        val bugCatchBlock = tryBlock._catch(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientTransactionBugException"))
        bugCatchBlock.body()._throw(JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException")))

        meth
    }

    private def translateStubState(s: State, inClass: JDefinedClass, txNames: mutable.Set[String], translationContext: TranslationContext): Unit = {
        for (decl <- s.fields) {
            translateStubDeclaration(decl, inClass, Some(s), txNames, translationContext)
        }
    }

    /* the make_X_Info functions setup necessary helper methods for a state-specific declaration
     * (i.e. defined in some states but not in the entire contract) of type X */
    def makeFieldInfo(newClass: JDefinedClass, stateLookup: Map[String, StateContext])
                     (name: String, declSeq: Seq[(State, Field)]): FieldInfo = {
        val fieldType = resolveType(declSeq.head._2.typ, table)

        /* setup get method */
        val getMeth = newClass.method(JMod.PRIVATE, fieldType, fieldGetMethodName(name))
        val getBody = getMeth.body()
        for ((st, f) <- declSeq) {
            // dynamically check the state
            getBody._if(invokeGetState(JExpr._this(), true).eq(stateLookup(st.name).enumVal))
                   ._then()
                   ._return(stateLookup(st.name).innerClassField.ref(f.name))
        }
        // exhaustive return to keep the compiler happy
        getBody._return(JExpr._null())


        /* setup set method */
        val setMeth = newClass.method(JMod.PRIVATE, model.VOID, fieldSetMethodName(name))
        val setBody = setMeth.body()
        val newValue = setMeth.param(fieldType, "newValue")
        for ((st, f) <- declSeq) {
            // dynamically check the state
            setBody._if(invokeGetState(JExpr._this(), true).eq(stateLookup(st.name).enumVal))
                ._then()
                .assign(stateLookup(st.name).innerClassField.ref(f.name), newValue)
        }

        StateSpecificFieldInfo(declSeq, getMeth, setMeth)
    }

    def makeTransactionInfo(newClass: JDefinedClass, stateLookup: Map[String, StateContext])
                           (name: String, declSeq: Seq[(State, Transaction)]): TransactionInfo = {
        val txExample = declSeq.head._2
        val stExample = declSeq.head._1

        val txExampleName = transactionGetMethodName(txExample.name, Some(stExample.name))

        val (hasReturn, retType) = txExample.retType match {
            case Some(typ) => (true, resolveType(typ, table))
            case None => (false, model.VOID)
        }

        val meth = newClass.method(JMod.PUBLIC, retType, txExampleName)
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ReentrancyException"))


      /* add the appropriate args to the method and collect them in a list */
        val jArgs = txExample.args.map( (arg: VariableDeclWithSpec) => meth.param(resolveType(arg.typIn, table), arg.varName) )

        val body = meth.body()

        for ((st, f) <- declSeq) {
            val inv = JExpr.invoke(stateLookup(st.name).innerClassField, txExample.name)

            /* add args to the invocation */
            jArgs.foldLeft(inv)((inv: JInvocation, arg: JVar) => inv.arg(arg))

            // dynamically check the state
            val stBody = body._if(invokeGetState(JExpr._this(), true).eq(stateLookup(st.name).enumVal))._then()

            if (hasReturn) stBody._return(inv)
            else stBody.add(inv)

        }
        // exhaustive return to please compiler
        if (hasReturn) body._return(JExpr._null())




        StateSpecificTransactionInfo(declSeq, meth)
    }

    /* Collects all the [transactions/functions/fields] in a contract.
     * This process also collects related definitions in different states: i.e. if a
     * field is defined in several states, these are collected together.
     * same type signature: if this does not hold, Java static checking will fail.
     * Also generates additional code */
    private def makeDeclarationLookupTables(
                    contract: Contract,
                    stateLookup: Map[String, StateContext],
                    newClass: JDefinedClass,
                    generateStub: Boolean):
                        (Map[String, FieldInfo], Map[String, TransactionInfo]) = {

        /* collect all declarations (fields, functions, transactions) that are particular
         * to a state.
         * Each declaration is also paired with the state it is defined in */
        val declarations: Seq[(Field, State)] = contract.declarations
                // look in all the states of the contract
                .filter(_.isInstanceOf[State])
                .map(_.asInstanceOf[State])
                // make a big, flat list of pairs (d: Declaration, s: State)
                .flatMap((s: State) => s.fields.zip(List.fill(s.fields.size)(s)))

        /* separate declarations by their type */
        val fields = declarations.filter(_._1.isInstanceOf[Field])
                                 .map((x: (Declaration, State)) => (x._2, x._1.asInstanceOf[Field]))

        /* splits items in [ts] into groups based on equality of the result of applying [f] */
        def generalizedPartition[T, S](ts: List[T], f: Function[T, S]): immutable.HashMap[S, Seq[T]] = {
            ts match {
                case h :: rest =>
                    val (equiv, nonEquiv) = rest.partition(f(_).equals(f(h)))
                    generalizedPartition(nonEquiv, f).updated(f(h), h +: equiv)
                case _ => new immutable.HashMap[S, Seq[T]]()
            }
        }

        if (generateStub) {
            (Map.empty, Map.empty)
        }
        else {

            val fieldInfoFunc = makeFieldInfo(newClass, stateLookup) _

            /* this uses the above helper function to group declarations by name. Conceptually, if we
            * define field "f" in states "S1" and "S2", it is one declaration that specifies multiple
            * states, rather than two distinct declarations.
            * For each grouped declaration, the corresponding makeInfo function is called to setup
            * the necessary information for the table */
            var fieldLookup = generalizedPartition[(State, Field), String](fields.toList, _._2.name)
                .transform(fieldInfoFunc)
            var txLookup = Map.empty[String, TransactionInfo]

            /* add on any whole-contract declarations to the lookup table: these are fairly simple */
            for (decl <- contract.declarations) {
                decl match {
                    case f: Field => fieldLookup = fieldLookup.updated(f.name, GlobalFieldInfo(f))
                    case t: Transaction => txLookup = txLookup.updated(t.name, GlobalTransactionInfo(t))
                    case _ => ()
                }
            }

            (fieldLookup, txLookup)
        }
    }

    /* factors out shared functionality for translate[Outer|Inner]Contract.
     * returns Some(stateEnum) if the contract has any states (None otherwise). */
    private def makeTranslationContext(
                                          aContract: Contract,
                                          newClass: JDefinedClass,
                                          contractNameResolutionMap: Map[Contract, String],
                                          protobufOuterClassNames: Map[String, String],
                                          generateStub: Boolean
                                      ): TranslationContext = {

        val (stateDeclarations, stateEnumOption, stateEnumField) = generateStateEnumAndFields(aContract, newClass, false)

        /* setup state lookup table */
        var stateLookup = new immutable.TreeMap[String, StateContext]()
        for (s <- stateDeclarations) {

            /* declare the inner class */
            val innerClass = newClass._class(innerClassName(s.name))

            /* declare the inner class field */
            val innerClassField = newClass.field(JMod.PRIVATE, innerClass, innerClassFieldName(s.name))

            val context =
                StateContext(
                    astState = s,
                    enumVal = stateEnumOption.get.enumConstant(s.name),
                    innerClass = innerClass,
                    innerClassField = innerClassField
                )

            stateLookup = stateLookup.insert(s.name, context)
        }

        /* setup tx/fun/field lookup tables */
        val (fieldLookup, txLookup) = makeDeclarationLookupTables(aContract, stateLookup, newClass, generateStub)

        /* setup the TranslationContext */
        val translationContext = TranslationContext(
            contract = aContract,
            contractClass = newClass,
            contractNameResolutionMap = contractNameResolutionMap,
            protobufOuterClassNames = protobufOuterClassNames,
            states = stateLookup,
            currentStateName = None,
            stateEnumClass = stateEnumOption,
            stateEnumField = stateEnumField,
            txLookup = txLookup,
            fieldLookup = fieldLookup,
            pendingFieldAssignments = Set.empty
        )

        /* i.e. if this contract defines any type states */
        if (translationContext.stateEnumClass.isDefined)
            generateStateHelpers(newClass, translationContext)

        translationContext
    }

    private def generateStateEnumAndFields(
                                          aContract: Contract,
                                          newClass: JDefinedClass,
                                          isStub: Boolean
                                          ): (Seq[State], Option[JDefinedClass], Option[JFieldVar]) = {

        /* setup the state enum */
        val stateDeclarations: Seq[State] =
            aContract.declarations.filter((d: Declaration) => d match {
                case State(_, _, _) => true
                case _ => false
            }).map({ s => s.asInstanceOf[State] })

        var stateEnumOption: Option[JDefinedClass] = None
        var stateEnumField: Option[JFieldVar] = None
        if (stateDeclarations.nonEmpty) {
            val stateEnum = newClass._enum(JMod.PUBLIC, stateEnumNameForClassName(aContract.name))
            stateEnumOption = Some(stateEnum)

            /* Declare the states in the enum */
            for (State(name, _, _) <- stateDeclarations) {
                stateEnum.enumConstant(name)
            }

            /* setup the state field and the [getState] method */
            stateEnumField = Some(newClass.field(JMod.PRIVATE, stateEnum, stateField))

            val stateMeth = newClass.method(JMod.PUBLIC, stateEnum, getStateMeth)
            stateMeth.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)
            stateMeth._throws(model.parseType("com.google.protobuf.InvalidProtocolBufferException").asInstanceOf[AbstractJClass])

            stateMeth.body()
                ._if(JExpr.ref(serializationParamName).neNull())
                ._then().invoke(JExpr._this(), "__restoreObject").arg(JExpr.ref(serializationParamName))

            if (!isStub) {
                stateMeth.body()._return(JExpr.ref(stateField))
            } else {
                stateMeth._throws(model.ref("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException"))
                val objectArrayType = newClass.owner().ref("java.util.ArrayList").narrow(newClass.owner().ref("String"))
                val newArrayExpr = JExpr._new(objectArrayType)
                val argArray = stateMeth.body().decl(objectArrayType, "argArray", newArrayExpr)

                //connectionManager.doTransaction(transaction.name, args)
                val tryBlock = stateMeth.body()._try()

                val doTransactionInvocation = JExpr.invoke(JExpr.ref("connectionManager"), "doTransaction")
                doTransactionInvocation.arg(getStateMeth)
                doTransactionInvocation.arg(argArray)
                doTransactionInvocation.arg(JExpr.invoke("__getGUID"))
                doTransactionInvocation.arg(true)

                // return result
                val marshalledResultDecl = tryBlock.body().decl(newClass.owner().ref("byte[]"), "marshalledResult", doTransactionInvocation)

                val stringClass = model.ref("java.lang.String")
                val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                val enumString = tryBlock.body().decl(stringClass, "enumString", JExpr._new(stringClass).arg(marshalledResultDecl).arg(charset)).invoke("trim")

                val errorBlock = new JBlock()

                tryBlock.body()._return(stateEnum.staticInvoke("valueOf").arg(enumString))

                val ioExceptionCatchBlock = tryBlock._catch(model.directClass("java.io.IOException"))
                ioExceptionCatchBlock.body()._throw(JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException")))

                val failedCatchBlock = tryBlock._catch(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientTransactionFailedException"))
                failedCatchBlock.body()._throw(JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException")))

                val bugCatchBlock = tryBlock._catch(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientTransactionBugException"))
                bugCatchBlock.body()._throw(JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException")))
            }
        }

        (stateDeclarations, stateEnumOption, stateEnumField)
    }


    private def fieldInScopeMethodName(fieldName: String) = {
        "__"+fieldName+"IsInScope"
    }

    private def generateFieldInScopeTests(aContract: Contract, newClass: JDefinedClass) : Unit = {
        for (decl <- aContract.declarations if decl.isInstanceOf[Field]) {
            val field: Field = decl.asInstanceOf[Field]
            val meth = newClass.method(JMod.PUBLIC, model.ref("boolean"), fieldInScopeMethodName(field.name))
            meth._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
            field.availableIn match {
                case None => meth.body()._return(JExpr.TRUE)
                case Some(states) =>
                    // Safe to not load here because by the time we get to it, we will have already exited if we have not loaded
                    val currentState = invokeGetState(JExpr._this(), false)
                    val availableInStatesArray = JExpr.newArray(model.ref("String"))
                    states.foreach( (state : String) => availableInStatesArray.add(JExpr.lit(state)))

                    val availableInStatesList = model.ref("java.util.Arrays").staticInvoke("asList").arg(availableInStatesArray)

                    meth.body()._return(availableInStatesList.invoke("contains").arg(currentState.invoke("toString")))
            }
        }
    }

    private def generateLazySerializationCode(aContract: Contract,
                                              newClass: JDefinedClass,
                                              translationContext: TranslationContext): Unit = {
        // Generate fields.

        // GUID field for this class.
        newClass.field(JMod.PRIVATE, model.ref("String"), guidFieldName)
        // has it been modified/do we need to write it out at the end?
        newClass.field(JMod.PRIVATE, model.ref("boolean"), modifiedFieldName)
        // is it loaded, or should it load itself lazily when we need it?
        newClass.field(JMod.PRIVATE, model.ref("boolean"), loadedFieldName)

        val guidConstructor = newClass.constructor(JMod.PUBLIC)
        guidConstructor.param(model.ref("String"), "__guid_")
        guidConstructor.body().assign(JExpr.ref(modifiedFieldName), JExpr.lit(false))
        guidConstructor.body().assign(JExpr.ref(loadedFieldName), JExpr.lit(false))
        guidConstructor.body().assign(JExpr.ref(guidFieldName), JExpr.ref("__guid_"))

        val getGUIDMeth = newClass.method(JMod.PUBLIC, model.ref("String"), "__getGUID")
        getGUIDMeth.body()._return(newClass.fields get guidFieldName)

        target match {
            case Client(mainContract, _) =>
                if (!aContract.isMain) {
                    generateResetModifiedMethod(aContract, newClass, translationContext)
                    generateRestoreMethod(aContract, newClass, translationContext)
                }
            case Server(_, _) =>
                generateResetModifiedMethod(aContract, newClass, translationContext)
                generateRestoreMethod(aContract, newClass, translationContext)
        }

        if (aContract.isMain) {
            /* We want to set the main contract to 'not loaded' after each
             * transaction, so it reloads data from the blockchain each time. */
            val unloadMeth = newClass.method(JMod.PROTECTED, model.VOID, "__unload")
            unloadMeth.body().assign(JExpr.ref(loadedFieldName), JExpr.lit(false))
        }

        if (!aContract.isMain) {
            val flushMethod = newClass.method(JMod.PUBLIC, model.VOID, "flush");
            flushMethod.body().assign(newClass.fields get loadedFieldName, JExpr.lit(false));
        }
    }

    private def generateResetModifiedMethod(aContract: Contract,
                                           newClass: JDefinedClass,
                                            translationContext: TranslationContext): Unit= {
        /* set is abstract, so we need to use a specific *kind* of set
         * in order to actually instantiate it */
        val hashSetType = model.directClass("java.util.HashSet")
          .narrow(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianSerialized"))

        val setType = model.directClass("java.util.Set")
          .narrow(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianSerialized"))
        // __resetModified collects the set of modified fields and resets the set of modified fields.
        val getModifiedMeth = newClass.method(JMod.PUBLIC, setType, "__resetModified")
        getModifiedMeth._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
        // add a list of things we've already checked so we don't get stuck in loops
        getModifiedMeth.param(setType, "checked");
        val modBody = getModifiedMeth.body()

        modBody.invoke(JExpr.ref("checked"), "add").arg(JExpr._this())

        val returnSet = modBody.decl(setType, "result", JExpr._new(hashSetType))

        // If we're not loaded, we can't have been modified, and we definitely shouldn't look
        // at our own fields, because that will break everything.
        modBody._if(JExpr.ref(loadedFieldName).not())._then()._return(returnSet)

        generateFieldInScopeTests(aContract, newClass)

        for (decl <- aContract.declarations if decl.isInstanceOf[Field]) {
            val field: Field = decl.asInstanceOf[Field]
            if (field.typ.isInstanceOf[NonPrimitiveType]) {
                // Only do anything if this field is in scope considering the current state.

                /* it's a non-primitive type, so it won't be saved directly --
                 * we also have to query it to see if it was modified.
                 * (and which of its sub-objects were modified if any) */
                /* But we have to make sure it wasn't already checked!
                 * Otherwise we'll be foiled by circular data structures. */

                modBody._if(JExpr.invoke(fieldInScopeMethodName(field.name)))._then()._if(JExpr.ref("checked").invoke("contains").arg(JExpr.ref(field.name)).not())
                  ._then()
                  .invoke(returnSet, "addAll").arg(
                    JExpr.ref(field.name).invoke("__resetModified").arg(JExpr.ref("checked")))

            }
        }

        /* return ourselves if we were modified */
        modBody._if(JExpr._this().ref(modifiedFieldName))._then()
          .invoke(returnSet, "add").arg(JExpr._this())

        /* once we call this (at the end of a transaction), we can assume
         * this thing is going to be written out, so mark it as no longer
         * modified. */
        modBody.assign(JExpr.ref(modifiedFieldName), JExpr.lit(false))

        modBody._return(returnSet)
    }

    private def generateRestoreMethod(aContract: Contract,
                                      newClass: JDefinedClass,
                                      translationContext: TranslationContext): Unit= {
        val restoreMeth = newClass.method(JMod.PUBLIC, model.VOID, "__restoreObject")
        restoreMeth.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)
        restoreMeth._throws(model.parseType("com.google.protobuf.InvalidProtocolBufferException")
          .asInstanceOf[AbstractJClass])

        if (aContract.isMain) {
            // If it's the main contract, we know its GUID already.
            restoreMeth.body().assign(JExpr.ref(guidFieldName), JExpr.lit(aContract.name))
        }

        val ifNotLoaded = restoreMeth.body()._if(JExpr.ref(loadedFieldName).not())._then()
        if (target.generateDebugOutput) {
            ifNotLoaded.directStatement("System.out.println(\"Loading " + aContract.name + " @ <\"+__guid+\"> from blockchain...\");")
        }
        ifNotLoaded.decl(model.ref("String"), "__archive_string",
            JExpr.ref(serializationParamName).invoke("getStub").invoke("getStringState").arg(JExpr.ref(guidFieldName)))
        ifNotLoaded.decl(model.BYTE.array(), "__archive_bytes", JExpr.ref("__archive_string").invoke("getBytes"))
        if (target.generateDebugOutput) {
            ifNotLoaded.directStatement("System.out.println(\"Loading data: \"+__archive_string);")
        }
        ifNotLoaded.invoke("__initFromArchiveBytes").arg(JExpr.ref("__archive_bytes")).arg(JExpr.ref(serializationParamName))
        ifNotLoaded.assign(JExpr.ref(loadedFieldName), JExpr.lit(true))
    }


    private def generateStateHelpers(newClass: JDefinedClass,
                                     translationContext: TranslationContext): Unit = {
        /* method to take care of conserved fields during a state transition
         * Invariant for use: [getStateMeth] returns the current state, the new state is passed
         * as an argument. The new state's inner class field must be non-null. */
        val conserveMeth = newClass.method(JMod.PRIVATE, model.VOID, conserveFieldsName)
        conserveMeth._throws(model.parseType("com.google.protobuf.InvalidProtocolBufferException").asInstanceOf[AbstractJClass])
        val nextState = conserveMeth.param(translationContext.stateEnumClass.get, "nextState")
        val conserveBody = conserveMeth.body()
        /* match on the current state */
        for (stFrom <- translationContext.states.values) {
            val thisStateBody = conserveBody._if(
                    invokeGetState(JExpr._this(), false).eq(stFrom.enumVal))
                ._then()
            /* match on the target state */
            for (stTo <- translationContext.states.values if stTo != stFrom) {
                val assignBody = thisStateBody._if(
                        nextState.eq(stTo.enumVal))
                    ._then()
                val (fromName, toName) = (stFrom.astState.name, stTo.astState.name)
                for (f <- translationContext.conservedFields(fromName, toName)) {
                    /* assign the field to the new state from the old */
                    assignBody.assign(stTo.innerClassField.ref(f.name),
                                        stFrom.innerClassField.ref(f.name))
                }
            }
        }

        /* method to null out old state
         * Invariant for use: [getStateMeth] returns the current state; this will be the state
         * whose inner class field this method nullifies. */
        val deleteMeth = newClass.method(JMod.PRIVATE, model.VOID, deleteOldStateName)
        deleteMeth._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
        val deleteBody = deleteMeth.body()
        for (st <- translationContext.states.values) {
            deleteBody._if(
                    invokeGetState(JExpr._this(), false).eq(st.enumVal))
                ._then()
                .assign(st.innerClassField, JExpr._null())
        }
    }

    /* Constructors are mapped to "new_" methods because we need to
         *separate initialization (which may occur via deserialization) from construction.
         * However, for non-top-level contracts, we want to be able to invoke the constructor as a
         * regular constructor.
         *
         * Because we don't know whether this class will be instantiated at runtime (as opposed to at deployment),
         * we generate both versions for all contracts.
         */
    def translateConstructors(decls: Seq[Constructor],
                              newClass: JDefinedClass,
                              translationContext: TranslationContext,
                              aContract: Contract): Unit = {
        // This constructor will be used for parameter names
        val mainConstr = decls.head
        val mainArgs = mainConstr.args
        val argNames = mainArgs.map(_.varName)

        val name = "new_" + mainConstr.name

        val newDispatcher = newClass.method(JMod.PRIVATE, model.VOID, name)
        addTransactionExceptions(newDispatcher, translationContext)

        /* add args to method and collect them in a list */
        val argList: Seq[(String, JVar)] = mainArgs.map((arg: VariableDeclWithSpec) =>
            (arg.varName, newDispatcher.param(resolveType(arg.typIn, table), arg.varName))
        )

        // We need to pass in a stub as well since we might need
        // to deserialize some objects in here (for example, if we call a method on a parameter
        // that returns a property of one of its child objects which isn't yet loaded)
        newDispatcher.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)

        /* construct the local context from this list */
        val localContext: immutable.Map[String, JVar] = argList.toMap

        def checkState(argName: String)(st: String): IJExpression =
            invokeGetState(JExpr.ref(argName), true).invoke("toString").invoke("equals").arg(JExpr.lit(st))

        def anyExpr(exprs: TraversableOnce[IJExpression]): IJExpression = exprs.fold(JExpr.FALSE)(_.cor(_))
        def allExpr(exprs: TraversableOnce[IJExpression]): IJExpression = exprs.fold(JExpr.TRUE)(_.cand(_))

        // Because constructor may have different parameter names, we make copies of parameters with multiple names as appropriate
        def genArgCopy(argPair: (VariableDeclWithSpec, String)): Unit = argPair match {
            case (arg, argName) => newDispatcher.body().decl(resolveType(arg.typIn, table), arg.varName, JExpr.ref(argName))
        }

        def buildConditions(arg: VariableDeclWithSpec): List[IJExpression] =
            arg.typIn match {
                case StateType(_, states, _) => anyExpr(states.map(checkState(arg.varName))) :: Nil
                case _ => Nil
            }

        // Build the conditions to check whether transaction's body to run
        val conditions = decls.map(constructor => (constructor, allExpr(constructor.args.flatMap(buildConditions))))

        def setOwned(block: JBlock, constructor: Constructor): JBlock =
            block.assign(JExpr.ref(constructorReturnsOwnedFieldName), JExpr.lit(constructor.resultType.isOwned))

        def addDispatchCondition(ifSt: JConditional)(info: (Constructor, IJExpression)): Unit = info match {
            case (decl, cond) =>
                setOwned(ifSt._elseif(cond)._then(), decl)
                translateBody(ifSt._elseif(cond)._then(), decl.body, translationContext, localContext)
        }

        // add bodies for each constructor
        conditions.headOption match {
            case Some((constructor, cond)) => {
                val distinctArgNames = decls.tail.flatMap(_.args.zip(argNames)).groupBy {
                        case (arg, argName) => arg.varName
                    }.values.map(_.head)

                distinctArgNames.foreach(genArgCopy)

                val ifStmt = newDispatcher.body()._if(cond)
                translateBody(setOwned(ifStmt._then(), constructor), constructor.body, translationContext, localContext)

                conditions.tail.foreach(addDispatchCondition(ifStmt))
            }

            case None => ()
        }

        assignNewGUID(newClass, aContract, newDispatcher)

        /* When an object is newly created, we always mark it as modified (and loaded). */
        newDispatcher.body().assign(newClass.fields get modifiedFieldName, JExpr.lit(true))
        newDispatcher.body().assign(newClass.fields get loadedFieldName, JExpr.lit(true))

        // Put the entry in the GUID map so we can find it later.
        val putEntryInvocation = newDispatcher.body().invoke(JExpr.ref(serializationParamName), "putEntry")

        putEntryInvocation.arg(newClass.fields get guidFieldName)
        putEntryInvocation.arg(JExpr._this())

        // Put the class in the returned object map so we can invoke transaction with -receiver on it
        target match {
            case Client(mainContract, _) => ()
            case Server(mainContract, _) =>
                if (translationContext.contract == mainContract) {
                    val mapReturnedObjectInvocation = newDispatcher.body().invoke(JExpr.ref(serializationParamName), "mapReturnedObject")
                    mapReturnedObjectInvocation.arg(JExpr._this())
                    mapReturnedObjectInvocation.arg(JExpr.FALSE)
                }
        }

        // -----------------------------------------------------------------------------
        // Also generate a constructor that calls the new_ method that we just generated.
        val constructor = newClass.constructor(JMod.PUBLIC)
        addTransactionExceptions(constructor, translationContext)

        val invocation = constructor.body().invoke(name)

        /* add args to method and collect them in a list */
        for (arg <- mainArgs) {
            constructor.param(resolveType(arg.typIn, table), arg.varName)
            invocation.arg(JExpr.ref(arg.varName))
        }

        constructor.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)
        invocation.arg(JExpr.ref(serializationParamName))

        if (!newClass.fields().containsKey(constructorReturnsOwnedFieldName)) {
            newClass.field(JMod.PRIVATE, model.BOOLEAN, constructorReturnsOwnedFieldName, JExpr.FALSE)
        }

        generateConstructorReturnsOwnedReference(newClass)
    }

    private def translateContract(
                    aContract: Contract,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext) = {
        target match {
            case Client(mainContract, _) =>
                if (aContract != mainContract) {
                    newClass._implements(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianSerialized"))
                }
            case Server(_, _) =>
                newClass._implements(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianSerialized"))
        }

//        aContract match {
//            case javaFFIContractImpl(name, interface, javaPath, declarations) => {
//                newClass._extends(model.directClass((javaPath.map(tuple=> tuple._1)).mkString(".")))
//            }
//            case _ =>
//        }

        generateLazySerializationCode(aContract, newClass, translationContext)

        aContract.declarations.flatMap {
            case c: Constructor => c :: Nil
            case _ => Nil
        }.groupBy(c => c.args.map(d => resolveType(d.typIn, table).name())).foreach {
            case (_, constructors) => translateConstructors(constructors, newClass, translationContext, aContract)
        }

        for (decl <- aContract.declarations if !decl.isInstanceOf[Constructor]) {
            translateDeclaration(decl, newClass, translationContext, aContract)
        }

        /* If the contract didn't already have a new_X() method with zero parameters,
         * add one that sets all the fields to default values.
         */
        if (!hasEmptyConstructor(aContract)) {
            generateInitializer(newClass, translationContext, aContract)
        }

        /* We need to ensure there's an empty constructor.
        * This constructor will be used in unarchiving; e.g. to unarchive class C:
        *          C c = new C(); c.initFromArchive(archive.getC().toByteArray());
        *
        *          The constructor is also used when initializing main contracts.
        */
        newClass.constructor(JMod.PUBLIC)

        generateReceiverOwnershipMethod(aContract, newClass)

        translationContext
    }

    // When a client invokes a method from off-blockchain, the receiver might need to be owned.
    // The runtime needs to check, but the types are erased at compile time. This generated method is invoked
    // by the runtime when it needs to know whether a method takes ownership of its receiver.
    // boolean methodReceiverIsOwnedAtBeginning(String methodName);
    // boolean methodReceiverIsOwnedAtEnd(String methodName);
    private def generateReceiverOwnershipMethod(contract: Contract, newClass: JDefinedClass): Unit = {
        val hashSetType = model.ref("java.util.HashSet").narrow(model.ref("java.lang.String"))
        val beginningField = newClass.field(JMod.STATIC, hashSetType, "transactionsWithOwnedReceiversAtBeginning")
        val endField = newClass.field(JMod.STATIC, hashSetType, "transactionsWithOwnedReceiversAtEnd")


        val beginMethod = newClass.method(JMod.PUBLIC, model.BOOLEAN, "methodReceiverIsOwnedAtBeginning")
        val endMethod = newClass.method(JMod.PUBLIC, model.BOOLEAN, "methodReceiverIsOwnedAtEnd")
        beginMethod.annotate(model.ref("Override"));
        endMethod.annotate(model.ref("Override"));
        val beginMethodNameParam = beginMethod.param(model.ref("java.lang.String"), "methodName")
        val endMethodNameParam = endMethod.param(model.ref("java.lang.String"), "methodName")

        val beginBody = beginMethod.body()
        val beginInitTest = beginBody._if(beginningField.eq(JExpr._null()))
        beginInitTest._then().assign(beginningField, JExpr._new(hashSetType))

        val endBody = endMethod.body()
        val endInitTest = endBody._if(endField.eq(JExpr._null()))
        endInitTest._then().assign(endField, JExpr._new(hashSetType))

        for (decl <- contract.declarations) {
            decl match {
                case iv: Transaction =>
                    if (iv.thisType.isOwned) {
                        beginInitTest._then().invoke(beginningField, "add").arg(decl.name)
                    }
                    if (iv.thisFinalType.isOwned) {
                        endInitTest._then().invoke(beginningField, "add").arg(decl.name)
                    }
                case _ => ()
            }
        }

        beginBody._return(beginningField.invoke("contains").arg(beginMethodNameParam))
        endBody._return(endField.invoke("contains").arg(endMethodNameParam))
    }

    // Contracts translate to compilation units containing one class.
    private def translateOuterContract(aContract: Contract,
                                       programPackage: JPackage,
                                       protobufOuterClassName: String,
                                       contractNameResolutionMap: Map[Contract, String],
                                       protobufOuterClassNames: Map[String, String],
                                       translationContext: TranslationContext,
                                       newClass: JDefinedClass) = {

        translateContract(aContract, newClass, translationContext)
        generateReentrantFlag(newClass, translationContext)

        val stubType = model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState")

        target match {
            case Client(mainContract, _) =>
                if (aContract == mainContract) {
                    newClass._extends(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientBase"))
                    generateClientMainMethod(newClass)
                    generateInvokeClientMainMethod(aContract, newClass)
                }
                else {
                    generateRunMethod(newClass, translationContext, stubType)
                    generateSerialization(aContract, newClass, translationContext)
                }
            case Server(_, _) =>
                if (aContract.isMain) {
                    /* We need to generate special methods for the main contract to align */
                    /* with the Hyperledger chaincode format */
                    generateMainServerClassMethods(newClass, translationContext)
                }

                /* (also contains a ChaincodeStub) */

                generateRunMethod(newClass, translationContext, stubType)
                generateSerialization(aContract, newClass, translationContext)
        }

        /* (also contains a ChaincodeStub) */

        // need to gather the types of the main contract constructor in order to correctly deserialize arguments
        // find the first declaration that is a constructor
        val constructor: Option[Declaration] = translationContext.contract.declarations.find(_.isInstanceOf[Constructor])
        // gather the types of its arguments
        val constructorTypes: Seq[(ObsidianType, ObsidianType)] =
            constructor match {
                case Some(constr: Constructor) => constr.args.map((d: VariableDeclWithSpec) => (d.typIn, d.typOut))
                case _ => List.empty
            }

        /* init method only if server */
        target match {
            case Client(mainContract, _) =>
                if (translationContext.contract != mainContract) {
                    generateInitMethod(translationContext, newClass, stubType, constructorTypes)
                }
            case Server(_, _) =>
                generateInitMethod(translationContext, newClass, stubType, constructorTypes)
        }
        if (constructor.isEmpty) {
            if (!newClass.fields().containsKey(constructorReturnsOwnedFieldName)) {
                newClass.field(JMod.PRIVATE, model.BOOLEAN, constructorReturnsOwnedFieldName, JExpr.TRUE)
            }

            generateConstructorReturnsOwnedReference(newClass)
        }
    }

    private def translateInnerContract(aContract: Contract,
                                       parent: JDefinedClass,
                                       translationContext: TranslationContext
                                      ): Unit = {
        val newClass: JDefinedClass = parent._class(JMod.PUBLIC, aContract.name)

        /* change the information in the translation context */
        val newTranslationContext = translationContext.copy(
            contract = aContract,
            contractClass = newClass
        )

        val _ = translateContract(aContract, newClass, newTranslationContext)
    }

    /* this method generates a flag for the contract in order to check
     * whether a reentrant call has been made.
     */
    private def generateReentrantFlag(newClass: JDefinedClass, translationContext: TranslationContext): Unit = {
        newClass.field(JMod.PUBLIC, model.BOOLEAN, isInsideInvocationFlag().name(), JExpr.lit(false))
    }

    private def generateMainServerClassMethods(newClass: JDefinedClass, translationContext: TranslationContext): Unit = {
        newClass._extends(model.directClass("edu.cmu.cs.obsidian.chaincode.HyperledgerChaincodeBase"))

        val stubType = model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState")
        /* (also contains a ChaincodeStub) */

        /* query method */
        val queryMeth: JMethod = newClass.method(JMod.PUBLIC, model.BYTE.array(), "query")
        queryMeth.param(stubType, serializationParamName)
        queryMeth.param(model.ref("String"), "transName")
        queryMeth.param(model.BYTE.array().array(), "args")

        // TODO
        queryMeth.body()._return(JExpr.newArray(model.BYTE, 0))


        /* getChaincodeID */
        val idMeth = newClass.method(JMod.PUBLIC, model.BYTE.array(), "getChaincodeID")
        idMeth.body()._return(JExpr.newArray(model.BYTE, 0))
        // TODO

        /* Main Method */
        generateServerMainMethod(newClass)
    }

    def addTransactionExceptions(meth: JMethod, translationContext: TranslationContext): Unit = {
        meth._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.BadArgumentException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianRevertException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ReentrancyException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.BadTransactionException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.InvalidStateException"))
        meth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.StateLockException"))

        target match {
            case Client(mainContract, _) =>
                if (translationContext.contract == mainContract) {
                    meth._throws(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException"))
                }
            case Server(_, _) => ()
        }
    }

    private def generateInitMethod(
                                    translationContext: TranslationContext,
                                    newClass: JDefinedClass,
                                    stubType: AbstractJClass,
                                    types: Seq[(ObsidianType, ObsidianType)]): Unit = {
        val initMeth: JMethod = newClass.method(JMod.PUBLIC, model.BYTE.array(), "init")
        initMeth.annotate(classOf[Override])
        initMeth.param(stubType, serializationParamName)
        val runArgs = initMeth.param(model.BYTE.array().array(), "args")

        addTransactionExceptions(initMeth, translationContext)

        // have to check that the args parameter has the correct number of arguments
        val cond = runArgs.ref("length").ne(JExpr.lit(types.length))
        val exceptionType = model.parseType("com.google.protobuf.InvalidProtocolBufferException")
        initMeth.body()._if(cond)._then()._throw(JExpr._new(exceptionType).arg("Incorrect number of arguments to constructor."))

        val constructorName = "new_" + newClass.name()

        val errorBlock: JBlock = new JBlock()
        val invocation: JInvocation = JExpr.invoke(constructorName)

        var runArgsIndex = 0

        for ((typIn, typOut) <- types) {
            val deserializedArg: IJExpression = unmarshallExprExpectingFullObjects(translationContext,
                initMeth.body(),
                runArgs.component(runArgsIndex),
                typIn,
                typOut,
                errorBlock,
                runArgsIndex)
            invocation.arg(deserializedArg)
            runArgsIndex += 1
        }

        invocation.arg(JExpr.ref(serializationParamName))
        initMeth.body().add(invocation)
        initMeth.body()._return(JExpr.newArray(model.BYTE, 0))
    }

    private def generateRunMethod(
                    newClass: JDefinedClass,
                    translationContext: TranslationContext,
                    stubType: AbstractJClass): Unit = {
        val runMeth = newClass.method(JMod.PUBLIC, model.BYTE.array(), "run")
        val exceptionType = model.parseType("com.google.protobuf.InvalidProtocolBufferException")
        runMeth._throws(exceptionType.asInstanceOf[AbstractJClass])
        runMeth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ReentrancyException"))
        runMeth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.BadArgumentException"))
        runMeth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException"))
        runMeth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.InvalidStateException"))
        runMeth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianRevertException"))
        runMeth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.IllegalOwnershipConsumptionException"))
        runMeth._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.StateLockException"))


        runMeth.param(stubType, serializationParamName)
        runMeth.param(model.ref("String"), "transName")
        val runArgs = runMeth.param(model.BYTE.array().array(), "args")

        runMeth.body().invoke("__restoreObject").arg(JExpr.ref(serializationParamName))

        // Put the class in the returned object map so we can invoke transaction with -receiver on it
        target match {
            case Client(mainContract, _) =>
                ()
            case Server(mainContract, _) =>
                if (translationContext.contract == mainContract) {
                    val mapReturnedObjectInvocation = runMeth.body().invoke(JExpr.ref(serializationParamName), "mapReturnedObject")
                    mapReturnedObjectInvocation.arg(JExpr._this())
                    mapReturnedObjectInvocation.arg(JExpr.FALSE)
                }
        }


        val returnBytes = runMeth.body().decl(
            model.BYTE.array(), "returnBytes",
            JExpr.newArray(model.BYTE, 0))

        runMeth._throws(model.ref("edu.cmu.cs.obsidian.chaincode.BadTransactionException"))
        runMeth._throws(model.ref("edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException"))

        /* we use this map to group together all transactions with the same name */
        val txsByName = new mutable.HashMap[String, mutable.Set[Transaction]]()

        for (decl <- translationContext.contract.declarations.filter((d: Declaration) => d.tag == TransactionDeclTag)) {
            val tx = decl.asInstanceOf[Transaction]
            val opt = txsByName.get(tx.name)
            opt match {
                case Some(sameNameTxs) =>
                    sameNameTxs += tx
                    txsByName.put(tx.name, sameNameTxs)
                case None =>
                    txsByName.put(tx.name, mutable.Set(tx))
            }
        }

        var lastTransactionElse: Option[JBlock] = None

        /* for each transaction name, we have a branch in the run method */
        for (txName <- txsByName.keySet) {
            val transEq = JExpr.ref("transName").invoke("equals").arg(JExpr.lit(txName))

            /* If we have an 'else' from the last 'if', attach the new 'if' statement to this 'else'.
             * Otherwise, just attach it to the main method body. */
            val transCond = lastTransactionElse match {
                case None => runMeth.body()._if(transEq)
                case Some(e) => e._if(transEq)
            }

            /* Get the 'else' of this last 'if' statement, to attach the next 'if' to. */
            lastTransactionElse = Some(transCond._else())

            val transCondBody = transCond._then()
            var transactionIsConditional = false // assume transaction is only available in one state unless we find otherwise

            // Iterate through all the transactions by this name.
            for (tx <- txsByName.getOrElse(txName, Set())) {
                val stateEq: IJExpression = {
                    tx.availableIn match {
                        case Some(stateSet) =>
                            var test: IJExpression = JExpr.TRUE
                            for (stateName <- stateSet) {
                                val thisTest = invokeGetState(JExpr._this(), true)
                                  .eq(translationContext.getEnum(stateName))

                                test = JOp.cor(test, thisTest)
                            }
                            test

                        case None => JExpr.TRUE // No need to check anything before running this transaction.
                    }
                }

                val stateCond = transCondBody._if(stateEq)
                val stateCondBody = stateCond._then()

                // Check to make sure we have enough arguments.
                val argsTest = runArgs.ref("length").eq(JExpr.lit(tx.args.length))
                val enoughArgsTest = stateCondBody._if(argsTest)
                val enoughArgs = enoughArgsTest._then()
                val notEnoughArgs = enoughArgsTest._else()

                // TODO: report the error to the client more directly
                notEnoughArgs.invoke(model.ref("System").staticRef("err"), "println").arg("Wrong number of arguments in invocation.")

                val exception = JExpr._new(model.ref("edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException"))
                exception.arg(txName)
                exception.arg(runArgs.ref("length"))
                exception.arg(tx.args.length)
                notEnoughArgs._throw(exception)

                /* parse the (typed) args from raw bytes */
                var txArgsList: List[JVar] = List.empty
                var runArgNumber = 0
                for (txArg <- tx.args) {
                    val runArg = runArgs.component(JExpr.lit(runArgNumber))
                    val javaArgType = resolveType(txArg.typIn, table)
                    val errorBlock = new JBlock()

                    val transactionArgExpr = unmarshallExprExpectingFullObjects(translationContext, enoughArgs, runArg, txArg.typIn, txArg.typOut, errorBlock, runArgNumber)

                    val newTxArg: JVar = enoughArgs.decl(
                        javaArgType,
                        txArg.varName,
                        transactionArgExpr
                    )

                    txArgsList = newTxArg :: txArgsList
                    runArgNumber += 1
                }

                var txInvoke: JInvocation = null
                val txMethName = transactionGetMethodName(tx.name, None) // XXX

                if (tx.retType.isDefined) {
                    txInvoke = JExpr.invoke(txMethName)

                    val returnObj = enoughArgs.decl(resolveType(tx.retType.get, table), "returnObj", txInvoke)

                    // Record the UUID of this object (if it is one).
                    tx.retType.get match {
                        case np: NonPrimitiveType =>
                            val mapInvocation = enoughArgs.invoke(JExpr.ref(serializationParamName), "mapReturnedObject")
                            mapInvocation.arg(returnObj)
                            mapInvocation.arg(np.isOwned)
                        case _ => ()
                    }

                    enoughArgs.assign(returnBytes,
                        tx.retType.get match {
                            case IntType() =>
                                val stringResult = returnObj.invoke("toString")
                                val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                                stringResult.invoke("getBytes").arg(charset)
                            case BoolType() => model.ref("edu.cmu.cs.obsidian.chaincode.ChaincodeUtils")
                              .staticInvoke("booleanToBytes").arg(txInvoke)
                            case StringType() =>    val invocation = returnObj.invoke("getBytes")
                                                    val charset = model.ref("java.nio.charset.StandardCharsets").staticRef("UTF_8")
                                                    invocation.arg(charset)
                            //                            case _ => returnObj.invoke("__archiveBytes")
                            case _ => returnObj.invoke("__getGUID").invoke("getBytes")
                        }

                    )
                } else {
                    txInvoke = enoughArgs.invoke(txMethName)
                }

                for (txArg <- txArgsList.reverse) {
                    txInvoke.arg(txArg)
                }

                // Pass serialization state to transactions
                // for deserialization of child objects.
                txInvoke.arg(JExpr.ref(serializationParamName))

            }
            /* If we aren't in any of the states were we can use this transaction, we throw an exception */
            if (transactionIsConditional) {
                transCondBody._throw(JExpr._new(model.ref("edu.cmu.cs.obsidian.chaincode.InvalidStateException")))
            }
        }

        // add the getState method here, only if it actually exists, i.e only if there is a
        if (translationContext.states.nonEmpty) {
            val transEq = JExpr.ref("transName").invoke("equals").arg(getStateMeth)
            val transCond = lastTransactionElse match {
                case None => runMeth.body()._if(transEq)
                case Some(e) => e._if(transEq)
            }

            /* Get the 'else' of this last 'if' statement, to attach the next 'if' to. */
            lastTransactionElse = Some(transCond._else())

            val transCondBody = transCond._then()
            // Check to make sure we have enough arguments.
            val newInteger = JExpr._new(model.parseType("Integer"))
            newInteger.arg("0")
            val argsTest = runArgs.ref("length").eq(newInteger)
            val enoughArgsTest = transCondBody._if(argsTest)
            val enoughArgs = enoughArgsTest._then()
            val notEnoughArgs = enoughArgsTest._else()

            notEnoughArgs.invoke(model.ref("System").staticRef("err"), "println").arg("Wrong number of arguments in invocation.")

            val exception = JExpr._new(model.ref("edu.cmu.cs.obsidian.chaincode.WrongNumberOfArgumentsException"))
            exception.arg(getStateMeth)
            exception.arg(runArgs.ref("length"))
            exception.arg(0)
            notEnoughArgs._throw(exception)

            enoughArgs.assign(returnBytes, invokeGetState(JExpr._this(), true).invoke("name").invoke("getBytes"))
        }

        /* Find where to throw a 'no such transaction' error.
         * (If we never generated any if statements, we throw it in the main method
         * body, since no transaction can be valid; otherwise, put it in the last
         * 'else' of the if-else tree.) */
        val noSuchTransactionBody = lastTransactionElse match {
            case None => runMeth.body()
            case Some(e) => e
        }

        noSuchTransactionBody._throw(JExpr._new(model.ref("edu.cmu.cs.obsidian.chaincode.NoSuchTransactionException")))

        /* Don't return anything if we put the 'throw' directly in the method body, since
         * it will make the java compiler complain about 'unreachable code.' */
        lastTransactionElse match {
            case None =>
            case Some(_) => runMeth.body()._return(returnBytes)
        }
    }

    private def generateServerMainMethod(newClass: JDefinedClass) = {
        val mainMeth = newClass.method(JMod.STATIC | JMod.PUBLIC, model.VOID, "main")
        val args = mainMeth.param(model.ref("String").array(), "args")

        // main takes one argument specifying the location of the store.
        // If the specified file does not exist, it creates a new store when it exits.

        // newClass instance = new newClass();
        // instance.delegatedMain(args);
        val body = mainMeth.body()
        val constructorInvocation = JExpr._new(newClass)
        constructorInvocation.arg(newClass.name()) // The name of the class suffices as a GUID for the top level contract.
        val instance = body.decl(newClass, "instance", constructorInvocation)

        val invocation = body.invoke(instance, "delegatedMain")
        invocation.arg(args)
    }

    private def generateClientMainMethod(newClass: JDefinedClass) = {
        val mainMeth = newClass.method(JMod.STATIC | JMod.PUBLIC, model.VOID, "main")
        val args = mainMeth.param(model.ref("String").array(), "args")

        // newClass instance = new newClass();
        // instance.delegatedMain(args);
        val body = mainMeth.body()
        val instance = body.decl(newClass, "instance", JExpr._new(newClass))
        val invocation = body.invoke(instance, "delegatedMain")
        invocation.arg(args)
    }

    /* The "invokeClientMain" method is called from delegatedMain.
     * invokeClientMain can assume the server is already connected.
     * It should construct a stub according to the type of the client's main(),
     * and pass it to the client's main() method.
    */
    private def generateInvokeClientMainMethod(aContract: Contract, newClass: JDefinedClass) = {
        val method = newClass.method(JMod.PUBLIC, model.VOID, "invokeClientMain")
        method._throws(model.parseType("com.google.protobuf.InvalidProtocolBufferException").asInstanceOf[AbstractJClass])
        method._throws(model.ref("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException"))
        method._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ReentrancyException"))
        method._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.BadTransactionException"))
        method._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.InvalidStateException"))
        method._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianRevertException"))
        method._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.StateLockException"))

        val mainTransactionOption: Option[Transaction] = aContract.declarations.find((d: Declaration) => d.isInstanceOf[Transaction] && d.asInstanceOf[Transaction].name.equals("main"))
                                                                  .asInstanceOf[Option[Transaction]]

        if (mainTransactionOption.isEmpty) {
            println("Error: can't find main transaction in main contract " + aContract.name)
        }
        else {
            val mainTransaction: Transaction = mainTransactionOption.get

            // The main transaction expects to be passed a stub of a particular type. Construct it.
            val stubType: ObsidianType = mainTransaction.args(0).typIn
            val stubJavaType = resolveType(stubType, table)
            val newStubExpr = JExpr._new(stubJavaType)
            newStubExpr.arg(JExpr.ref("connectionManager"))
            val generateUUID = stubType match {
                case n: NonPrimitiveType =>
                    n.contractName.toString
                case _ => assert(false, "Stub must be of nonprimitive type.")
                    ""
            }
            newStubExpr.arg(generateUUID)
            val stubVariable = method.body().decl(stubJavaType, "stub", newStubExpr)
            // generate a new serialization state to be compatible
            val serializationState = method.body.decl(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientSerializationState"), serializationParamName, JExpr._new(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientSerializationState")))
            val clientMainInvocation = method.body.invoke("main")
            clientMainInvocation.arg(stubVariable).arg(serializationState)
        }

    }

    private def generateSerialization(
                    contract: Contract,
                    inClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        generateSerializer(contract, inClass)
        generateWrapperSerializer(contract, inClass, translationContext)
        generateArchiver(contract, inClass, translationContext)
        generateArchiveInitializer(contract, inClass, translationContext)


        val subcontracts = contract.declarations.filter(d => d.isInstanceOf[Contract])

        for (c <- contract.declarations if c.isInstanceOf[ObsidianContractImpl]) {
            val innerContract: ObsidianContractImpl = c.asInstanceOf[ObsidianContractImpl]
            val javaInnerClasses = inClass.classes().asScala
            val javaInnerClassOption = javaInnerClasses.find((c: AbstractJClass) => (c.name().equals(innerContract.name)))


            if (javaInnerClassOption.isDefined) {
                val javaInnerClass = javaInnerClassOption.get.asInstanceOf[JDefinedClass]
                val stateEnumName = stateEnumNameForClassName(javaInnerClass.name)

                generateSerialization(innerContract, javaInnerClass.asInstanceOf[JDefinedClass], translationContext)
            }
            else {
                println("Bug: can't find inner class in generated Java code for inner class " + innerContract.name)
            }
        }
    }

    // "set" followed by lowercasing the field name.
    private def setterNameForField(fieldName: String) = {
        if (fieldName.length < 1) {
            assert(false, "Bug: field name is empty")
            "set"
        }
        else {
            // Always use US locale, regardless of the user's locale, so that all code is compatible.
            val firstChar = fieldName.substring(0, 1).toUpperCase(java.util.Locale.US)
            val rest = fieldName.substring(1)
            "set" + firstChar + rest
        }
    }

    // "get" followed by lowercasing the field name.
    private def getterNameForField(fieldName: String) = {
        if (fieldName.length < 1) {
            assert(false, "Bug: field name is empty")
            "get"
        }
        else {
            // Always use US locale, regardless of the user's locale, so that all code is compatible.
            val firstChar = fieldName.substring(0, 1).toUpperCase(java.util.Locale.US)
            val rest = fieldName.substring(1)
            "get" + firstChar + rest
        }
    }

    // "has" followed by lowercasing the field name.
    private def hasNameForField(fieldName: String) = {
        if (fieldName.length < 1) {
            assert(false, "Bug: field name is empty")
            "has"
        }
        else {
            // Always use US locale, regardless of the user's locale, so that all code is compatible.
            val firstChar = fieldName.substring(0, 1).toUpperCase(java.util.Locale.US)
            val rest = fieldName.substring(1)
            "has" + firstChar + rest
        }
    }

    private def generateFieldArchiver(
                    field: Field,
                    fieldVar: JVar,
                    builderVar: JFieldRef,
                    body: JBlock,
                    translationContext: TranslationContext,
                    inContract: Contract): Unit = {
        val javaFieldType = resolveType(field.typ, table)

        def handleNonPrimitive(name: String, n: ObsidianType): Unit = {
            val ifNonNull: JConditional = body._if(fieldVar.ne(JExpr._null()))
            val nonNullBody = ifNonNull._then()

            val contract = resolveNonPrimitiveTypeToContract(n, translationContext, inContract)
            if (contract.isEmpty) {
                println("Compilation error: unable to resolve type " + n)
            }
            else {
                val archiveVariable =  {
                    val archiveGUID = JExpr.invoke(fieldVar, "__getGUID");
                    nonNullBody.decl(model.ref("String"),
                        field.name + "ArchiveID",
                        archiveGUID)
                }

                // generate: builder.setField(field);
                val setterName: String = setterNameForField(field.name)

                val invocation: JInvocation = nonNullBody.invoke(builderVar, setterName)
                invocation.arg(archiveVariable)
            }
        }

        field.typ match {
            case IntType() => {
                val ifNonNull: JConditional = body._if(fieldVar.ne(JExpr._null()))
                val nonNullBody = ifNonNull._then()

                // Special serialization for BigInteger, since that's how the Obsidian int type gets translated.
                // The protobuf type for this is just bytes.
                // builder.setField(ByteString.CopyFrom(field.toByteArray()))
                val setterName: String = setterNameForField(field.name)
                val setInvocation = JExpr.invoke(builderVar, setterName)

                val byteStringClass: AbstractJClass =
                    model.parseType("com.google.protobuf.ByteString").asInstanceOf[AbstractJClass]
                val toByteArrayInvocation = JExpr.invoke(fieldVar, "toByteArray")
                val copyFromInvocation = byteStringClass.staticInvoke("copyFrom")
                copyFromInvocation.arg(toByteArrayInvocation)
                setInvocation.arg(copyFromInvocation)

                nonNullBody.add(setInvocation)
            }
            case BoolType() => {
                val setterName: String = setterNameForField(field.name)
                val setInvocation = body.invoke(builderVar, setterName)
                setInvocation.arg(fieldVar)
            }
            case StringType() => {
                val ifNonNull: JConditional = body._if(fieldVar.ne(JExpr._null()))
                val nonNullBody = ifNonNull._then()

                val setterName: String = setterNameForField(field.name)
                val setInvocation = nonNullBody.invoke(builderVar, setterName)
                setInvocation.arg(fieldVar)
            }
            case n: NonPrimitiveType => handleNonPrimitive(field.name, n)
            case _ => () // TODO handle other types
        }
    }

    private def generateStateArchiver(contract: Contract,
                                      state: State,
                                      stateClass: JDefinedClass,
                                      translationContext: TranslationContext): Unit = {
        val contractProtobufClassName = translationContext.getProtobufClassName(contract)
        val protobufClassName = contractProtobufClassName + "." + state.name

        val archiveType = model.directClass(protobufClassName)
        val archiveMethod = stateClass.method(JMod.PUBLIC, archiveType, "archive")
        archiveMethod._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
        val archiveBody = archiveMethod.body()

        val protobufMessageClassBuilder: String = protobufClassName + ".Builder"
        val builderType = model.parseType(protobufMessageClassBuilder)

        /* TODO obviously this workaround is bad; see if there's another way */
        archiveBody.directStatement(protobufMessageClassBuilder + " builder = " +
            protobufClassName + ".newBuilder();")
        val builderVariable = JExpr.ref("builder")

        val declarations = state.fields

        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = stateClass.fields().get(field.name)
            generateFieldArchiver(field, javaFieldVar, builderVariable, archiveBody, translationContext, contract)
        }

        val buildInvocation = JExpr.invoke(builderVariable, "build")
        archiveBody._return(buildInvocation)
    }

    // Generates a method, archive(), which outputs a protobuf object corresponding to the archive of this class.
    private def generateArchiver(
                    contract: Contract,
                    inClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        val protobufClassName = translationContext.getProtobufClassName(contract)

        val archiveType = model.directClass(protobufClassName)
        val archiveMethod = inClass.method(JMod.PUBLIC, archiveType, "archive")
        archiveMethod._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
        val archiveBody = archiveMethod.body()

        val protobufMessageClassBuilder: String = protobufClassName + ".Builder"
        val builderType = model.parseType(protobufMessageClassBuilder)

        /* TODO obviously this workaround is bad; see if there's another way */
        archiveBody.directStatement(protobufMessageClassBuilder + " builder = " +
                                    protobufClassName + ".newBuilder();")
        val builderVariable = JExpr.ref("builder")
        // val builderVariable: JVar = archiveBody.decl(builderType, "builder", archiveType.staticInvoke("newBuilder"))
        // Iterate through fields of this class and archive each one by calling setters on a builder.

        archiveBody.invoke(builderVariable, "setGuid").arg(inClass.fields().get("__guid"))

        val declarations = contract.declarations

        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = inClass.fields().get(field.name)
            generateFieldArchiver(field, javaFieldVar, builderVariable, archiveBody, translationContext, contract)
        }

        /* handle states if there are any */
        if (translationContext.states.nonEmpty) {
            for (stDecl <- declarations.filter(d => d.isInstanceOf[State])) {
                val st = stDecl.asInstanceOf[State]

                // Safe to not load here because we should always be loaded when we are archiving,
                // and therefore we will never actually try to use __st
                val cond = translationContext.getEnum(st.name).eq(invokeGetState(JExpr._this(), false))
                val thisStateBody = archiveBody._if(cond)._then()

                val stateField = translationContext.states(st.name).innerClassField
                val builtState = thisStateBody.invoke(stateField, "archive")
                thisStateBody.invoke(builderVariable, "setState" + st.name)
                             .arg(builtState)
            }
        }

        val buildInvocation = JExpr.invoke(builderVariable, "build")
        archiveBody._return(buildInvocation)
    }


    // Generates a method, __archiveBytes(), which outputs a string in protobuf format.
    private def generateSerializer(contract: Contract, inClass: JDefinedClass): Unit = {
        val archiveMethod = inClass.method(JMod.PUBLIC, model.parseType("byte[]"), "__archiveBytes")
        archiveMethod.annotate(classOf[Override])
        archiveMethod._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))

        val archiveBody = archiveMethod.body()

        val archive = JExpr.invoke(JExpr._this(), "archive")
        val archiveBytes = archive.invoke("toByteArray")
        archiveBody._return(archiveBytes);
    }

    // Generates a method, __wrappedArchiveBytes(), which wraps Foo in a FooOrGuid object and outputs as bytes
    private def generateWrapperSerializer(contract: Contract, inClass: JDefinedClass, translationContext: TranslationContext): Unit = {
        val archiveMethod = inClass.method(JMod.PUBLIC, model.parseType("byte[]"), "__wrappedArchiveBytes")
        archiveMethod.annotate(classOf[Override])
        archiveMethod._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
        val contractName = contract.name

        val protobufClassName = translationContext.getProtobufClassName(contract)
        val protobufMessageClassBuilder: String = protobufClassName + "OrGUID" + ".Builder"
        val builderType = model.parseType(protobufMessageClassBuilder)

        val archiveBody = archiveMethod.body()

        /* TODO obviously this workaround is bad; see if there's another way */
        archiveBody.directStatement(protobufMessageClassBuilder + " builder = " +
          protobufClassName + "OrGUID" + ".newBuilder();")
        val builderVariable = JExpr.ref("builder")

        archiveBody.invoke(builderVariable, "setObj").arg(JExpr._this().invoke("archive"))
        val archiveType = model.directClass(protobufClassName + "OrGUID")

        archiveBody.decl(archiveType, "wrappedObject", builderVariable.invoke("build"))

        val wrappedVariable = JExpr.ref("wrappedObject")

        val bytes = wrappedVariable.invoke("toByteArray")
        archiveBody._return(bytes);
    }

    private def generateFieldInitializer(
                    field: Field,
                    fieldVar: JVar,
                    body: JBlock,
                    archive: JVar,
                    translationContext: TranslationContext,
                    inContract: Contract): Unit = {
        // generate: FieldArchive fieldArchive = field.archive();
        val javaFieldName: String = field.name
        val javaFieldType: AbstractJType = resolveType(field.typ, table)

        def handleNonPrimitive(name: String, n: ObsidianType): Unit = {
            // foo = new Foo(); foo.initFromArchive(archive.getFoo());
            val contract = resolveNonPrimitiveTypeToContract(n, translationContext, inContract)
            if (contract.isEmpty) {
                println("Error: unresolved contract name " + name)
            }
            else {
                val protobufClassName = translationContext.getProtobufClassName(contract.get)

                val archiveType: AbstractJType = model.parseType(protobufClassName)

                // TODO
                /* generate another method that takes the actual archive type
                 * so we don't have to uselessly convert to bytes here */
                body.decl(model.ref("String"), javaFieldName + "ID",
                    archive.invoke(getterNameForField(javaFieldName)))
                body.decl(javaFieldType, javaFieldName + "Val",
                    JExpr.cast(javaFieldType,
                        JExpr.ref(serializationParamName).invoke("getEntry").arg(JExpr.ref(javaFieldName+"ID"))))

                val checkForObj = body._if(JExpr.ref(javaFieldName + "Val").ne(JExpr._null()))

                // If we found the object in the map of loaded objects, assign it to the field.
                // We're good.
                checkForObj._then().assign(fieldVar, JExpr.ref(javaFieldName+"Val"))

                // if we didn't find the object in the map of already-loaded objects,
                // just initialize it with the GUID.
                // If it turns out we actually need it, we'll reconstitute it later
                // using said GUID.
                checkForObj._else().assign(fieldVar,
                    JExpr._new(javaFieldType).arg(JExpr.ref(javaFieldName + "ID")))
                // We also need to put this thing in the map of loaded objects,
                // so if someone else references it later, we get the same reference.
                checkForObj._else().invoke(JExpr.ref(serializationParamName), "putEntry")
                    .arg(JExpr.ref(javaFieldName + "ID"))
                    .arg(fieldVar)

            }
        }

        field.typ match {
            case IntType() => {
                // Special serialization for BigInteger, since that's how the Obsidian int type gets translated.
                // The protobuf type for this is just bytes.
                // if (!archive.getFoo().isEmpty) {
                //     foo = new BigInteger(archive.getFoo().toByteArray())
                // }
                val getterName = getterNameForField(javaFieldName)
                val ifNonempty = body._if(archive.invoke(getterName).invoke("isEmpty").not())
                val newInteger = JExpr._new(model.parseType("java.math.BigInteger"))

                val getCall = archive.invoke(getterName)
                newInteger.arg(JExpr.invoke(getCall, "toByteArray"))

                ifNonempty._then().assign(fieldVar, newInteger)
            }
            case BoolType() => {
                val getterName = getterNameForField(javaFieldName)
                // foo = archive.getFoo();
                val getCall = archive.invoke(getterName)
                body.assign(fieldVar, getCall)
            }
            case StringType() => {
                val getterName = getterNameForField(javaFieldName)

                val ifNonempty = body._if(archive.invoke(getterName).invoke("isEmpty").not())

                // foo = archive.getFoo();
                val getCall = archive.invoke(getterName)
                ifNonempty._then().assign(fieldVar, getCall)
            }
            case n: NonPrimitiveType => handleNonPrimitive(field.name, n)
            case _: BottomType => assert(false, "Bottom type should not occur at codegen.")
            case UnitType() => assert(false, "Fields must not be of unit type.")
        }
    }

    private def getFromArchiveMeth(contract: Contract,
                                   state: State,
                                   stateClass: JDefinedClass,
                                   translationContext: TranslationContext,
                                   archiveType: JDirectClass): JMethod = {
        // TODO: remove this method! We no longer archive any fields with states.

        val fromArchiveMeth = stateClass.method(JMod.PUBLIC, model.VOID, "initFromArchive")
        val archive = fromArchiveMeth.param(archiveType, "archive")
        val fromArchiveBody = fromArchiveMeth.body()

        // Call setters.
        val declarations = state.fields

        assert(declarations.isEmpty, "States should have no fields by the time we do code generation.")

        return fromArchiveMeth
    }


    // TODO: remove this, since we no longer archive state fields with states?
    private def generateStateArchiveInitializer(
                    contract: Contract,
                    state: State,
                    stateClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        val contractProtobufClassName = translationContext.getProtobufClassName(contract)
        val protobufClassName = contractProtobufClassName + "." + state.name

        val archiveType = model.directClass(protobufClassName)

        /* [initFromArchive] setup */
        val fromArchiveMeth = getFromArchiveMeth(contract, state, stateClass, translationContext, archiveType)

        /* [__initFromArchiveBytes] declaration: this just parses the archive and
         * calls [initFromArchive] */
        val fromBytesMeth =
            stateClass.method(JMod.PUBLIC, stateClass, "__initFromArchiveBytes")

        val exceptionType = model.parseType("com.google.protobuf.InvalidProtocolBufferException")
        fromBytesMeth._throws(exceptionType.asInstanceOf[AbstractJClass])
        val archiveBytes = fromBytesMeth.param(model.parseType("byte[]"), "archiveBytes")

        val fromBytesBody = fromBytesMeth.body()
        val parseInvocation: JInvocation = archiveType.staticInvoke("parseFrom")
        parseInvocation.arg(archiveBytes)
        val parsedArchive = fromBytesBody.decl(archiveType, "archive", parseInvocation)

        fromBytesBody.invoke(fromArchiveMeth).arg(parsedArchive)
        fromBytesBody._return(JExpr._this())
    }

    /* generates the method [__initFromArchiveBytes] and [initFromArchive];
     * these methods load the recipient class from a protobuf message in the
     * form of raw bytes and a java message protobuf object, respectively.
     * Overrides __initFromArchiveBytes() in superclass if this is the main contract */
    private def generateArchiveInitializer(
                    contract: Contract,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        val protobufClassName = translationContext.getProtobufClassName(contract)
        val archiveType = model.directClass(protobufClassName)

        /* [initFromArchive] setup */
        val fromArchiveMeth = newClass.method(JMod.PUBLIC, model.VOID, "initFromArchive")
        fromArchiveMeth._throws(model.directClass("com.google.protobuf.InvalidProtocolBufferException"))
        val archive = fromArchiveMeth.param(archiveType, "archive")
        // Also need the serialization-state b/c the object might
        // already be loaded.
        fromArchiveMeth.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)
        val fromArchiveBody = fromArchiveMeth.body()

        /* [__initFromArchiveBytes] declaration: this just parses the archive and
         * calls [initFromArchive] */
        val fromBytesMeth =
            newClass.method(JMod.PUBLIC, newClass, "__initFromArchiveBytes")
        val exceptionType = model.parseType("com.google.protobuf.InvalidProtocolBufferException")
        fromBytesMeth._throws(exceptionType.asInstanceOf[AbstractJClass])
        val archiveBytes = fromBytesMeth.param(model.parseType("byte[]"), "archiveBytes")
        // Need serialization state in case we already loaded the object.
        fromBytesMeth.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)

        val fromBytesBody = fromBytesMeth.body()
        val parseInvocation: JInvocation = archiveType.staticInvoke("parseFrom")
        parseInvocation.arg(archiveBytes)
        val parsedArchive = fromBytesBody.decl(archiveType, "archive", parseInvocation)
        val archiveInv = fromBytesBody.invoke(fromArchiveMeth).arg(parsedArchive)

        archiveInv.arg(JExpr.ref(serializationParamName))
        // If lazy loading, note that we loaded this object.
        fromBytesBody.assign(JExpr.ref(loadedFieldName), JExpr.lit(true))

        fromBytesBody._return(JExpr._this())

        /* [initFromArchive] does most of the grunt work of mapping the protobuf message to
         * fields of the class: the following code does this */

        // Call setters.
        val declarations = contract.declarations

        /* set state enum */
        val enumGetter = "getStateCase"
        val enumName = (stateName: String) =>
            protobufClassName + "." + "StateCase" + "." + ("STATE" + stateName).toUpperCase

        fromArchiveBody.assign(JExpr.ref("__guid"), archive.invoke("getGuid"))

        for (stDecl <- declarations if stDecl.isInstanceOf[State]) {
            val st = stDecl.asInstanceOf[State]
            val thisStateBody = fromArchiveBody._if(
                archive.invoke(enumGetter).invoke("equals").arg(
                    JExpr.direct(enumName(st.name)))
            )._then()
            thisStateBody.assign(JExpr.ref(stateField), translationContext.getEnum(st.name))
        }

        /* this takes care of fields that are not specific to any particular state */
        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]

            val cond = fromArchiveBody._if(JExpr.invoke(fieldInScopeMethodName(field.name)))
            val javaFieldVar = newClass.fields().get(field.name)
            generateFieldInitializer(field, javaFieldVar, cond._then(), archive, translationContext, contract)
        }



    }

    private def translateDeclaration(
                    declaration: Declaration,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext,
                    aContract: Contract): Unit = {
        declaration match {
            case (f: Field) =>
                translateFieldDecl(f, newClass)
            case (t: Transaction) =>
                translateTransDecl(t, newClass, translationContext)
                if (aContract.isMain) {
                    mainTransactions.add(t)
                }
            case (s@State(_, _, _)) => aContract match {
              case obsContract: ObsidianContractImpl => translateStateDecl(s, obsContract, newClass, translationContext)
              case javaContract: JavaFFIContractImpl => ()
            }

            case (c@ObsidianContractImpl(_, _, _,_, _, _)) => translateInnerContract(c, newClass, translationContext)

            case (c@JavaFFIContractImpl(_,_,_,_,_)) => ()

            case t: TypeDecl =>
                assert(false, "TODO")
                ()

            // There is no case for constructors because they are handled separately outside of this
            // method (see translateConstructors()). This should never be hit.
            case _ => assert(false, "Translating unexpected declaration: " + declaration)
        }
    }


    private def resolveType(typ: ObsidianType, table: SymbolTable): AbstractJType = {
        typ match {
            case IntType() => model.directClass("java.math.BigInteger")
            case BoolType() => model.BOOLEAN
            case StringType() => model.ref("String")
            case n: NonPrimitiveType => {
                val contractTableOpt = table.contractLookup.get(n.contractName)
                contractTableOpt match {
                    case None => if (n.isRemote) model.ref(classNameForStub(n.contractName)) else model.ref(n.contractName)
                    case Some(x) => x.contract match {
                        case obsContract : ObsidianContractImpl =>
                            if (n.isRemote) model.ref(classNameForStub(n.contractName)) else model.ref(n.contractName)
                        case javaContract : JavaFFIContractImpl =>
                            val pathSequence = javaContract.javaPath.map(x => x._1)
                            val pathToJavaFile = pathSequence.mkString(".")
                            model.parseType(pathToJavaFile)
                    }
                }
            }
            case _ => model.VOID // TODO: translate PDTs
        }

    }

    // The NonPrimitiveType stores the type name as a string; this method figures out which contract that maps to
    // according to the scope in which the type name is used.
    private def resolveNonPrimitiveTypeToContract(typ: ObsidianType,
                                                  translationContext: TranslationContext,
                                                  containingContract: Contract): Option[Contract] = {
        typ match {
            case t: NonPrimitiveType => {
                val name = t.contractName

                var typeComponents = name.split(".")
                if (typeComponents.isEmpty) {
                    typeComponents = Array(name)
                }

                // Suppose the containing contract is A.B.C and we're looking up B. We want to find A.B unless there's A.B.C.B.
                // We start our search with the innermost contract and ascend until we find a contract with the name we want.

                // recursiveTypeLookup does the actual search.
                def recursiveTypeLookup(containingContract: Contract, typeComponents: Seq[String]): Option[Contract] = {

                    // matchContract looks for a contract WITHIN the given contract.
                    def matchContract(containingContract: Contract, typeComponents: Seq[String]): Option[Contract] = {
                        if (typeComponents.length == 0) {
                            Some(containingContract)
                        }
                        else {
                            val innerContracts = containingContract.declarations.filter((decl: Declaration) => decl.isInstanceOf[Contract])
                            val innerContract = innerContracts.find((decl: Declaration) => decl.asInstanceOf[Contract].name.equals(typeComponents.head))
                            if (innerContract.isDefined) {
                                matchContract(innerContract.get.asInstanceOf[Contract], typeComponents.tail)
                            }
                            else None
                        }
                    }

                    // Check to see if typeComponents are inside this contract. If not, recurse one level up.
                    val matchedContract: Option[Contract] = matchContract(containingContract, typeComponents)

                    val result = if (matchedContract.isDefined) matchedContract
                    else {
                        val outerContract = translationContext.getContainingContract(containingContract)
                        if (outerContract.isDefined)
                            recursiveTypeLookup(outerContract.get, typeComponents)
                        else None
                    }


                    result
                }

                val insideContractResult = recursiveTypeLookup(containingContract, typeComponents)
                if (insideContractResult.isDefined)
                    insideContractResult
                else {
                    // Check for a top-level contract by this name.
                    val foundPair = translationContext.contractNameResolutionMap.find((pair: (Contract, String)) => pair._2.equals(name))
                    if (foundPair.isDefined) Some(foundPair.get._1) else None
                }
            }
            case _ => None
        }
    }

    private def fieldInitializerForType(typ: ObsidianType): Option[IJExpression] = {
        typ match {
            case IntType() =>
                val newInt =
                    model.ref("java.math.BigInteger").staticInvoke("valueOf")
                val _ = newInt.arg(0)
                Some(newInt)
            case BoolType() =>
                Some(JExpr.lit(false))
            case StringType() =>
                Some(JExpr.lit(""))
            // this case encompasses [AstContractType] and [AstStateType]
            case _ => None
        }
    }

    private def translateFieldDecl(decl: Field, newClass: JDefinedClass): Unit = {
        val initializer = fieldInitializerForType(decl.typ)
        if (initializer.isDefined) {
            newClass.field(JMod.PUBLIC, resolveType(decl.typ, table), decl.name, initializer.get)
        }
        else {
            newClass.field(JMod.PUBLIC, resolveType(decl.typ, table), decl.name)
        }
    }

    private def addArgs(inv: JInvocation,
                        args: Seq[Expression],
                        translationContext: TranslationContext,
                        localContext: Map[String, JVar], isFFIInvocation: Boolean): JInvocation = {
        val foldF = (inv: JInvocation, arg: Expression) =>
            inv.arg(translateExpr(arg, translationContext, localContext))
        args.foldLeft(inv)(foldF)

        // Pass chaincode stub to other invoked methods as well, in case
        // an object needs to be restored from the blockchain.
        if (!isFFIInvocation) {
            inv.arg(JExpr.ref(serializationParamName))
        }
        inv
    }

    /* returns an expr because exprs are built bottom-up (unlike everything else) */
    private def translateExpr(e: Expression,
                              translationContext: TranslationContext,
                              localContext: Map[String, JVar]): IJExpression = {
        val recurse = (e: Expression) => translateExpr(e, translationContext, localContext)

        e match {
            case ReferenceIdentifier(x) => dereferenceVariable(x, translationContext, localContext)
            case NumLiteral(n) => model.directClass("java.math.BigInteger").
                                    staticInvoke("valueOf").arg(JExpr.lit(n))
            case StringLiteral(s) => JExpr.lit(s)
            case TrueLiteral() => JExpr.TRUE
            case FalseLiteral() => JExpr.FALSE
            case This() => JExpr._this()
            case Conjunction(e1, e2) => recurse(e1).cand(recurse(e2))
            case Disjunction(e1, e2) => recurse(e1).cor(recurse(e2))
            case LogicalNegation(e1) => recurse(e1).not()
            case Add(e1, e2) => recurse(e1).invoke("add").arg(recurse(e2))
            case Subtract(e1, e2) => recurse(e1).invoke("subtract").arg(recurse(e2))
            case Multiply(e1, e2) => recurse(e1).invoke("multiply").arg(recurse(e2))
            case Mod(e1, e2) => recurse(e1).invoke("mod").arg(recurse(e2))
            case Divide(e1, e2) => recurse(e1).invoke("divide").arg(recurse(e2))
            case Negate(e) => recurse(e).invoke("negate")
            case Equals(e1, e2) => recurse(e1).invoke("equals").arg(recurse(e2))
            case GreaterThan(e1, e2) =>
                recurse(e1).invoke("compareTo").arg(recurse(e2)).eq(JExpr.lit(1))
            case GreaterThanOrEquals(e1, e2) =>
                recurse(e1).invoke("compareTo").arg(recurse(e2)).ne(JExpr.lit(-1))
            case LessThan(e1, e2) =>
                recurse(e1).invoke("compareTo").arg(recurse(e2)).eq(JExpr.lit(-1))
            case LessThanOrEquals(e1, e2) =>
                recurse(e1).invoke("compareTo").arg(recurse(e2)).ne(JExpr.lit(1))
            case NotEquals(e1, e2) =>
                recurse(e1).invoke("equals").arg(recurse(e2)).not()
            case Dereference(e1, f) => recurse(e1).ref(f) /* TODO : do we ever need this? */
            case LocalInvocation(name, args) =>
                if (name == "sqrt" && args.length == 1) {
                    val arg0 = recurse(args(0))
                    val doubleResult = model.ref("java.lang.Math").staticInvoke("sqrt").arg(arg0.invoke("doubleValue"))
                    val intResult = JExpr.cast(model.ref("int"), doubleResult)
                    val stringResult = model.ref("Integer").staticInvoke("toString").arg(intResult)

                    JExpr._new(model.parseType("java.math.BigInteger")).arg(stringResult)
                }
                else {
                    addArgs(translationContext.invokeTransaction(name), args, translationContext, localContext, false)
                }
            /* TODO : this shouldn't be an extra case */
            case Invocation(This(), name, args, isFFIInvocation) =>
                addArgs(translationContext.invokeTransaction(name), args, translationContext, localContext, isFFIInvocation)
            case Invocation(recipient, name, args, isFFIInvocation) =>
                addArgs(JExpr.invoke(recurse(recipient), name), args, translationContext, localContext, isFFIInvocation)
            case Construction(name, args, isFFIInvocation) =>
                val contractRefType = ContractReferenceType(ContractType(name), Owned(), false)
                val resolvedType = resolveType(contractRefType, table)
                addArgs(JExpr._new(resolvedType), args, translationContext, localContext, isFFIInvocation)
            case Parent() => assert(false, "Parents should not exist in code generation"); JExpr._null()
            case Disown(e) => recurse(e)
            case StateInitializer(stateName, fieldName) => JExpr.ref(stateInitializationVariableName(stateName._1, fieldName._1))
        }
    }

    private def translateStateDecl(
                    state: State,
                    contract: Contract,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        val stateClass = translationContext.states(state.name).innerClass

        /* we change one thing: the currently translated state */
        val newTranslationContext = translationContext.copy(currentStateName = Some(state.name))
        for (decl <- state.fields) {
            translateDeclaration(decl, stateClass, newTranslationContext, contract)
        }
        generateStateArchiveInitializer(contract, state, stateClass, translationContext)
        generateStateArchiver(contract, state, stateClass, translationContext)
    }


    private def assignNewGUID(newClass : JDefinedClass, aContract: Contract, meth: JMethod) = {
        /* Generate a GUID for the object when it's created. */
        /* If it's a main contract, we use the contract name as an ID so we know
         * how to find the root of the object graph. */
        if (aContract.isMain) {
            meth.body().assign(newClass.fields get guidFieldName, JExpr.lit(aContract.name))
        } else {
            val generateUUID = JExpr.ref(serializationParamName).invoke("getUUIDFactory").invoke("newUUID").invoke("toString")
            meth.body().assign(newClass.fields get guidFieldName, generateUUID)
        }
    }

    private def generateConstructorReturnsOwnedReference(newClass: JDefinedClass): Unit = {
        if (!newClass.methods().asScala.exists(_.name() == constructorReturnsOwnedReferenceMethName)) {
            val ownedRefMethod = newClass.method(JMod.PUBLIC, model.BOOLEAN, constructorReturnsOwnedReferenceMethName)
            ownedRefMethod.annotate(model.ref("Override"))

            ownedRefMethod.body()._return(JExpr.ref(constructorReturnsOwnedFieldName))
        }
    }

    // Generates a new_Foo() method, which takes the serialization state as a parameter, and initializes all the fields.
    private def generateInitializer(
                                            newClass: JDefinedClass,
                                            translationContext: TranslationContext,
                                            aContract: Contract) {
        val methodName = "new_" + newClass.name()

        val meth: JMethod = newClass.method(JMod.PRIVATE, model.VOID, methodName)

        meth.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)

        val body: JBlock = meth.body()

        assignNewGUID(newClass, aContract, meth)

        for (decl <- aContract.declarations) {
            decl match {
                /* Initialize all fields to suitable default values. */
                case f: Field =>
                    val initializer = fieldInitializerForType(f.typ)
                    if (initializer.isDefined) {
                        body.assign(newClass.fields get f.name, initializer.get)
                    } else {
                        /* Set them to null for now, they'll get re-set soon after. */
                        body.assign(newClass.fields get f.name, JExpr._null)
                    }
                case _ => /* nothing */
            }
        }

        body.invoke(JExpr.ref(serializationParamName), "flushEntries")


        // -----------------------------------------------------------------------------
        // Also generate a constructor that calls the new_ method that we just generated.
        val constructor = newClass.constructor(JMod.PUBLIC)
        constructor._throws(model.directClass("edu.cmu.cs.obsidian.chaincode.ObsidianRevertException"))

        val invocation = constructor.body().invoke(methodName)

        /* add args to method and collect them in a list */

        constructor.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)
        invocation.arg(JExpr.ref(serializationParamName))
    }

    private def dynamicStateCheck(tx: Transaction, body: JBlock, expr: IJExpression, typ: ObsidianType): Unit = {
        typ match {
            case StateType(_, states, _) => {
                val currentState = invokeGetState(expr, true)

                var cond: IJExpression = JExpr.TRUE
                // check if the current state is in any of the possible states
                for (st <- states) {
                    val className = resolveType(typ, table).name()
                    val enumClassName = stateEnumNameForClassName(className)
                    val enumClass = model.ref(packageName + "." + className + "." + enumClassName)
                    val enumConstant = JExpr.enumConstantRef(enumClass, st)
                    cond = JOp.cand(currentState.ne(enumConstant), cond)
                }

                val exception = JExpr._new(model.directClass("edu.cmu.cs.obsidian.chaincode.InvalidStateException"))
                exception.arg(expr)
                exception.arg(currentState.invoke("toString"))
                exception.arg(tx.name)
                body._if(cond)
                  ._then()._throw(exception)
            }
            case _ => ()
        }
    }

    private def translateTransDecl(
                    tx: Transaction,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext): JMethod = {
        // Put all transactions at the top level, for now.
        val javaRetType = tx.retType match {
            case Some(typ) => resolveType(typ, table)
            case None => model.VOID
        }

        val meth: JMethod = newClass.method(JMod.PUBLIC, javaRetType, tx.name)
        addTransactionExceptions(meth, translationContext)

        /* ensure the object is loaded before trying to do anything.
         * (even checking the state!) */
        target match {
            case Client(mainContract, _) =>
                if (translationContext.contract != mainContract) {
                    meth.body().invoke("__restoreObject").arg(JExpr.ref(serializationParamName))

                }
            case Server(_, _) =>
                meth.body().invoke("__restoreObject").arg(JExpr.ref(serializationParamName))
        }

        // Dynamically check the state of the receiver and the arguments.
        dynamicStateCheck(tx, meth.body(), JExpr._this(), tx.thisType)

        /* add args to method and collect them in a list */
        val jArgs: Seq[(String, JVar)] = tx.args.map((arg: VariableDeclWithSpec) =>
            (arg.varName, meth.param(resolveType(arg.typIn, table), arg.varName))
        )

        val argsWithVars = jArgs.zip (tx.args)
        for  (argWithVar <- argsWithVars) {
            dynamicStateCheck(tx, meth.body(), argWithVar._1._2, argWithVar._2.typIn)
        }

        /* We put the method body in a try block, and set the tx flag to false in a finally
         * block. Thus, even if a transaction is thrown, or there is a return statement,
         * the tx flag is still set to false once the whole transaction has been executed.
         */
        val jTry = meth.body()._try()

        /* if the flag has already been set, that means there has been a reentrancy */
        val reentrancyTest = if (tx.isPrivate) JExpr.FALSE else isInsideInvocationFlag()
        val jIf = jTry.body()._if(reentrancyTest)
        val exception = JExpr._new(model.ref("edu.cmu.cs.obsidian.chaincode.ReentrancyException"))
        //
        exception.arg(translationContext.contract.asInstanceOf[ObsidianContractImpl].sourcePath)
        exception.arg(tx.loc.line)
        jIf._then()._throw(exception)

        /* otherwise, we set the flag to true */
        jIf._else().assign(isInsideInvocationFlag(), JExpr.lit(true))

        val argList: Seq[(String, JVar)] =
        // We need to pass the ChaincodeStub to methods so that the objects can
        // restore themselves from the blockchain if need be.
            (serializationParamName, meth.param(model.directClass("edu.cmu.cs.obsidian.chaincode.SerializationState"), serializationParamName)) +: jArgs

        /* construct the local context from this list */
        val localContext: immutable.Map[String, JVar] = argList.toMap

        /* add body */
        translateBody(jIf._else(), tx.body, translationContext, localContext)

        /* once the whole transaction has been executed, we set the flag back to false */
        jTry._finally().assign(isInsideInvocationFlag(), JExpr.lit(false))

        // Clear any pending field assignments between transactions.
        translationContext.pendingFieldAssignments = Set.empty

        meth
    }

    /* these methods make shadowing possible */

    private def dereferenceVariable(name: String,
                                    translationContext: TranslationContext,
                                    localContext: Map[String, JVar]): IJExpression = {
        localContext.get(name) match {
            case Some(variable) => variable
            case None => translationContext.dereferenceVariable(name,model)
        }
    }

    private def assignVariable(name: String,
                               newValue: IJExpression,
                               body: JBlock,
                               translationContext: TranslationContext,
                               localContext: Map[String, JVar],
                               forceFieldAssignment: Boolean
                              ): Unit = {
        localContext.get(name) match {
            case Some(variable) =>
                if (forceFieldAssignment) {
                    body.assign(JExpr._this().ref(name), newValue)
                    body.assign(JExpr.ref(modifiedFieldName), JExpr.lit(true))
                }
                else {
                    body.assign(variable, newValue)
                }
            case None =>
                translationContext.assignVariable(name, newValue, body, forceFieldAssignment)
                // This is a field, so mark that we're modified.
                body.assign(JExpr.ref(modifiedFieldName), JExpr.lit(true))
        }
    }

    private def stateInitializationVariableName(stateName: String, fieldName: String) = {
        "__" + stateName + "__init__" + fieldName
    }

    private def translateStatement(
                    body: JBlock,
                    statement: Statement,
                    translationContext: TranslationContext,
                    localContext: Map[String, JVar]): Map[String, JVar] = {
        var nextContext = localContext
        statement match {
            case VariableDecl(typ, name) =>
                val initializer = typ match {
                    case BoolType() => JExpr.lit(false)
                    case _ => JExpr._null()
                }
                nextContext = localContext.updated(name, body.decl(resolveType(typ, table), name, initializer))
            case VariableDeclWithInit(typ, name, e) =>
                nextContext = localContext.updated(name, body.decl(resolveType(typ, table), name, translateExpr(e, translationContext, localContext)))
            case Return() => body._return()
            case ReturnExpr(e) => body._return(translateExpr(e, translationContext, localContext))
            case Transition(newStateName, updates, permission) =>
                /* We must (in this order):
                 *     0) if this reference is shared, check to see if the referenced object is statelocked
                 *     1) construct the new state's inner class
                 *     2) assign the fields of the new inner class object
                 *     3) clean up the old state
                 *     4) change the state enum
                 */
                if (permission == Shared()) {
                    val stateLockCheck = JExpr.ref(serializationParamName).invoke("objectIsStateLocked").arg(JExpr._this())

                    val ifStateLocked = body._if(stateLockCheck)
                    val exception = JExpr._new(model.ref("edu.cmu.cs.obsidian.chaincode.StateLockException"))
                    exception.arg(translationContext.contract.asInstanceOf[ObsidianContractImpl].sourcePath)
                    exception.arg(statement.loc.line)
                    ifStateLocked._then()._throw(exception)
                }



                /* construct a new instance of the inner contract */
                val newStateContext = translationContext.states(newStateName)

                val newStField = newStateContext.innerClassField
                body.assign(newStField, JExpr._new(translationContext.states(newStateName).innerClass))

                /* assign fields in the update construct */
                updates match {
                    case Some(u) =>
                        for ((f, e) <- u) {
                            assignVariable(f.name, translateExpr(e, translationContext, localContext),
                                body, translationContext, localContext, true)
                        }
                    case None =>
                        // Fields should have been initialized individually, via S1::foo = bar.
                }


                /* assign conserved fields (implicit to programmer) */
                body.invoke(conserveFieldsName).arg(newStateContext.enumVal)

                /* nullify old state inner class field */
                body.invoke(deleteOldStateName)

                /* change the enum to reflect the new state */
                body.assign(JExpr.ref(stateField), translationContext.getEnum(newStateName))

                /* note that we've been modified so the changes will be saved */
                body.assign(JExpr.ref(modifiedFieldName), JExpr.lit(true))

                // Assign according to the state initialization statements, e.g. S1::foo = bar.
                // Do this after updating the current state because assignVariable may invoke setters.
                for (fieldName <- translationContext.pendingFieldAssignments) {
                    translationContext.assignVariable(fieldName, JExpr.ref(stateInitializationVariableName(newStateName, fieldName)), body, true)
                }

                translationContext.pendingFieldAssignments = Set.empty
            case Assignment(ReferenceIdentifier(x), e) =>
                assignVariable(x, translateExpr(e, translationContext,localContext),
                    body, translationContext, localContext, false)
            /* it's bad that this is a special case */
            case Assignment(Dereference(This(), field), e) => {
                /* we don't check the local context and just assume it's a field */
                val newValue = translateExpr(e, translationContext,localContext)
                translationContext.assignVariable(field, newValue, body, false)
                // This is a field, so mark that we're modified.
                // (Note: since we can only assign to fields of 'this', we only need
                // to mark that we assigned here and in assignVariable if it's a field.)
                // TODO: don't generate this for every field we modify...
                body.assign(JExpr.ref(modifiedFieldName), JExpr.lit(true))
            }
            case Assignment(Dereference(eDeref, field), e) => {
                // TODO: do we ever need this in the general case if all contracts are encapsulated?
                assert(false, "TODO")
            }
            case Assignment(StateInitializer(stateName, fieldName), e) => {
                val stateContextOption = translationContext.states.get(stateName._1)
                assert(stateContextOption.isDefined)
                val stateContext = stateContextOption.get

                // Assign to a temporary variable.
                val fieldInfo = translationContext.fieldLookup(fieldName._1)
                val field: Field = fieldInfo match {
                    case GlobalFieldInfo(decl) => decl
                    case StateSpecificFieldInfo(declSeq, _, _) =>
                        val state = stateContext.astState
                        declSeq.find((p: (State, Field)) => p._1 == state).get._2
                }

                val tempVarType = resolveType(field.typ, table)

                val tempVar = body.decl(tempVarType,
                    stateInitializationVariableName(stateName._1, fieldName._1),
                    translateExpr(e, translationContext, localContext))

                translationContext.pendingFieldAssignments = translationContext.pendingFieldAssignments + fieldName._1
            }

            case Revert(e) =>
                val translatedExpr = e match {
                    case None => JExpr._null()
                    case Some (expr) => translateExpr(expr, translationContext, localContext)
                }
                val exception = JExpr._new(model.ref("edu.cmu.cs.obsidian.chaincode.ObsidianRevertException"))
                exception.arg(translationContext.contract.asInstanceOf[ObsidianContractImpl].sourcePath)
                exception.arg(statement.loc.line)
                exception.arg(translatedExpr)
                body._throw(exception)

            case If(e, s) =>
                translateBody(body._if(translateExpr(e, translationContext, localContext))._then(),
                              s, translationContext, localContext)
            case IfThenElse(e, s1, s2) =>
                val jIf = body._if(translateExpr(e, translationContext, localContext))
                translateBody(jIf._then(), s1, translationContext, localContext)
                translateBody(jIf._else(), s2, translationContext, localContext)

            case TryCatch(s1, s2) =>
                val jTry = body._try()
                val jCatch = jTry._catch(model.ref("RuntimeException"))
                translateBody(jTry.body(), s1, translationContext, localContext)
                translateBody(jCatch.body(), s2, translationContext, localContext)

            case Switch(e, cases) =>
                val h :: remainingCases = cases
                val jEx = translateExpr(e, translationContext, localContext)
                val eqState = (s: String) =>
                    // TODO
                    /* somewhat of a bad workaround, but the alternative involves knowing the
                     * type of the expression jEx: in general, this requires an analysis
                     * to link references to declarations */
                    invokeGetState(jEx, true).invoke("toString").invoke("equals").arg(JExpr.lit(s))

                val jIf = body._if(eqState(h.stateName))
                translateBody(jIf._then(), h.body, translationContext, localContext)

                var jPrev = jIf
                for (_case <- remainingCases) {
                    jPrev = jPrev._elseif(eqState(_case.stateName))
                    translateBody(jPrev._then(), _case.body, translationContext, localContext)
                }
                // If no cases matched, this indicates a bug. Abort.
                jPrev._else()._throw(JExpr._new(model.ref("RuntimeException")))
            case IfInState(e, state, s1, s2) =>
                val jEx = translateExpr(e, translationContext, localContext)

                val eqState = (s: String) =>
                    invokeGetState(jEx, true).invoke("toString").invoke("equals").arg(JExpr.lit(s))

                val jIf = body._if(eqState(state._1))

                val shouldStateLock = e match {
                    case ReferenceIdentifier(x) => true
                    case _ => false
                }

                if (shouldStateLock) {
                    val tryBlock = jIf._then()._try()
                    tryBlock.body().invoke(JExpr.ref(serializationParamName), "beginStateLock").arg(jEx)
                    translateBody(tryBlock.body(), s1, translationContext, localContext)
                    tryBlock._finally().invoke(JExpr.ref(serializationParamName), "endStateLock").arg(jEx)
                }
                else {
                    translateBody(jIf._then(), s1, translationContext, localContext)
                }

                if (s2.nonEmpty) {
                    translateBody(jIf._else(), s2, translationContext, localContext)
                }

            case LocalInvocation(methName, args) =>
                body.add(addArgs(translationContext.invokeTransaction(methName),
                        args, translationContext, localContext, false))
            /* TODO : it's bad that this is a special case */
            case Invocation(This(), methName, args, isFFIInvocation) =>
                body.add(addArgs(translationContext.invokeTransaction(methName),
                        args, translationContext, localContext, isFFIInvocation))

            case Invocation(e, methName, args, isFFIInvocation) =>
                addArgs(body.invoke(translateExpr(e, translationContext, localContext), methName),
                        args, translationContext, localContext, isFFIInvocation)
            case StaticAssert(e, l) => () // Nothing to do
            /* all expressions can be statements but no other expressions have a reasonable meaning */
            case _ => ()
        }

        nextContext
    }

    private def translateBody(
                    body: JBlock,
                    statements: Seq[Statement],
                    translationContext: TranslationContext,
                    localContext: Map[String, JVar]): Unit = {
        var nextContext = localContext
        for (st <- statements) {
            nextContext = translateStatement(body, st, translationContext, nextContext)
        }
    }
}

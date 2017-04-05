package edu.cmu.cs.obsidian.codegen

import CodeGen._
import edu.cmu.cs.obsidian.parser._
import com.helger.jcodemodel._
import edu.cmu.cs.obsidian.util._

import scala.collection.{mutable, _}
import collection.JavaConverters._


trait Target
// We have to keep track of which contract is the main client contract because some of the imported contracts may be main contracts for server processes.
case class Client(mainContract: Contract) extends Target
case class Server() extends Target

class CodeGen (val target: Target) {

    private val model: JCodeModel = new JCodeModel()

    /* we must keep track of the transactions in main so that we can match on
     * them in the "invoke" function. Each tuple [(Some(s), tr)] is a transaction
     * represented by a transaction [tr], in a state identified by [s]. If the first
     * element in the pair is [None], then the transaction works in all states */
    private val mainTransactions: mutable.Set[(Option[String], Transaction)] =
        new mutable.HashSet[(Option[String], Transaction)]()

    /* we track the constructor for essentially the same reason */
    private var mainConstructor: Option[JMethod] = None

    /* naming conventions for various generated Java constructs */
    private final val stateField: String = "__state"
    private final val getStateMeth: String = "getState"
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
    /* This method dynamically checks type state and copies the value of conserved fields
     * (i.e. defined in both states) from one state to another */
    private final val conserveFieldsName = "__conserveFields"
    /* based on the result of [getStateMeth], this nulls out the appropriate state field
     * so that the old state can be garbage-collected after a transition */
    private final val deleteOldStateName = "__oldStateToNull"

    final val packageName: String = "edu.cmu.cs.obsidian.generated_code"

    def populateProtobufOuterClassNames (contract: Contract,
                                         protobufOuterClassName: String,
                                         contractNameResolutionMap: Map[Contract, String],
                                         protobufOuterClassNames: mutable.HashMap[String, String]): Unit = {
        protobufOuterClassNames += (contractNameResolutionMap(contract) -> protobufOuterClassName)

        for (d <- contract.declarations if d.isInstanceOf[Contract]) {
            val innerContract = d.asInstanceOf[Contract]
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
                                          programPackage: JPackage): JCodeModel =
    {
        // TODO: refactor this to support imports properly
        val contractNameResolutionMap: Map[Contract, String] = TranslationContext.contractNameResolutionMapForProgram(program)
        val protobufOuterClassNames = mutable.HashMap.empty[String, String]
        for (c <- program.contracts) {
            populateProtobufOuterClassNames(c, protobufOuterClassName, contractNameResolutionMap, protobufOuterClassNames)
        }


        for (imp <- program.imports) {
            translateImport(programPackage, imp, contractNameResolutionMap, protobufOuterClassNames)
        }

        for (c <- program.contracts) {
            translateOuterContract(c, programPackage, protobufOuterClassName, contractNameResolutionMap, protobufOuterClassNames)
        }
        model
    }


    /* [true] iff the contract [c] has a constructor that takes no args */
    private def hasEmptyConstructor(c: Contract): Boolean = {
        for (d <- c.declarations) {
            d match {
                case Constructor(_, args, _) => if (args.length == 0) return true
                case _ => ()
            }
        }
        return false
    }


    private def translateImport(programPackage: JPackage,
                                imp: Import,
                                contractNameResolutionMap: Map[Contract, String],
                                protobufOuterClassNames: Map[String, String]): Unit = {
        // Each import corresponds to a file. Each file has to be read, parsed, and translated into a list of stub contracts.
        val filename = imp.name;

        val ast = Parser.parseFileAtPath(filename, printTokens = false)

        target match {
            case Client(_) =>
                val program = translateProgramInPackage(ast, Util.protobufOuterClassNameForFilename(filename), programPackage)
                val stub = translateStubProgram(ast, programPackage, contractNameResolutionMap, protobufOuterClassNames)

            case Server() => assert(false, "imports not yet supported in servers") // TODO
        }
    }

    private def translateStubProgram(program: Program,
                                     programPackage: JPackage,
                                     contractNameResolutionMap: Map[Contract, String],
                                     protobufOuterClassNames: Map[String, String]): Unit = {
        // TODO: support imports in programs to be translated as stubs
        assert(program.imports.isEmpty, "imports in imported contracts are not yet supported");

        for (contract <- program.contracts) {
            translateStubContract(contract, programPackage, contractNameResolutionMap, protobufOuterClassNames)
        }
    }

    private def classNameForStub(contractName: String) = {
        contractName + "__Stub__"
    }

    private def translateStubContract(contract: Contract,
                                      programPackage: JPackage,
                                      contractNameResolutionMap: Map[Contract, String],
                                      protobufOuterClassNames: Map[String, String]): Unit = {
        var contractClass = programPackage._class(JMod.PUBLIC, classNameForStub(contract.name))
        contractClass._extends(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientStub"))

        val constructor = contractClass.constructor(JMod.PUBLIC)
        val param = constructor.param(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientConnectionManager"), "connectionManager")
        val superConstructorInvocation = constructor.body().invoke("super")
        superConstructorInvocation.arg(param)

        val stubTranslationContext = makeTranslationContext(contract, contractClass, contractNameResolutionMap, protobufOuterClassNames, true)


        for (decl <- contract.declarations) {
            translateStubDeclaration(decl, contractClass, stubTranslationContext)
        }
    }

    private def translateStubDeclaration(decl: Declaration, inClass: JDefinedClass, translationContext: TranslationContext) : Unit = {
        decl match {
            case TypeDecl(_, _) => assert(false, "unsupported"); // TODO
            case f@Field(_, _) => translateStubField(f, inClass, translationContext)
            case Constructor(name, args, body) => // Constructors aren't translated becuase stubs are only for remote instances.
            case f@Func(name, args, retType, body) => translateStubFunction(f, inClass)
            case t@Transaction(_, _, _, _) => translateStubTransaction(t, inClass, translationContext)
            case s@State(_, _) => translateStubState(s, inClass)
            case c@Contract(mod, name, decls) => translateStubContract(c,
                inClass.getPackage(),
                translationContext.contractNameResolutionMap,
                translationContext.protobufOuterClassNames)
        }
    }

    private def translateStubField(decl: Field, newClass: JDefinedClass, translationContext: TranslationContext): Unit = {
        // In a stub, every non-primitive field is also a stub, so we need to make the field of appropriate type for that.
        // Primitive fields will not map to anything; instead, their accesses will translate to remote calls.
        val remoteFieldType = decl.typ match {
            case n@NonPrimitiveType(modifiers, name) => if (!modifiers.contains(IsRemote)) NonPrimitiveType((modifiers :+ IsRemote), name) else n
            case o@_ => o
        }
        newClass.field(JMod.PRIVATE, resolveType(remoteFieldType), decl.fieldName)
    }

    private def translateStubFunction(f: Func, inClass: JDefinedClass) : Unit = {
        // TODO
    }

    private def marshallExpr(unmarshalledExpr: IJExpression, typ: Type): IJExpression = {
        val marshalledArg = typ match
        {
            case IntType() => unmarshalledExpr.invoke("toByteArray");
            case BoolType() => JExpr.cond(unmarshalledExpr, JExpr.ref("TRUE_ARRAY"), JExpr.ref("FALSE_ARRAY"))
            case StringType() => val byteStringClass: AbstractJClass =
                model.parseType("com.google.protobuf.ByteString").asInstanceOf[AbstractJClass]
                val toByteArrayInvocation = JExpr.invoke(unmarshalledExpr, "toByteArray")
                toByteArrayInvocation
            case NonPrimitiveType(modifiers, name) => unmarshalledExpr.invoke("__archiveBytes")
        }

        marshalledArg
    }

    // Returns a pair of an error-checking block option and the resulting expression.
    private def unmarshallExpr(marshalledExpr: IJExpression, typ: Type, errorBlock: JBlock): IJExpression = {
        typ match
        {
            case IntType() =>
                val newInt = JExpr._new(model.parseType("java.math.BigInteger"))
                newInt.arg(marshalledExpr)
                newInt
            case BoolType() =>
                val _ = errorBlock._if(marshalledExpr.ref("length").eq(JExpr.lit(1)).not())
                marshalledExpr.component(JExpr.lit(0)).eq0()
            case StringType() =>
                val byteStringClass: AbstractJClass =
                    model.parseType("com.google.protobuf.ByteString").asInstanceOf[AbstractJClass]

                val copyFromInvocation = byteStringClass.staticInvoke("copyFrom")
                val _ = copyFromInvocation.arg(marshalledExpr)

                copyFromInvocation.invoke("toString")
            case NonPrimitiveType(modifiers, name) =>
                val targetClass = resolveType(typ).asInstanceOf[AbstractJClass]
                val classInstance = JExpr._new(targetClass)
                val invocation = classInstance.invoke("__initFromArchiveBytes")
                val _ = invocation.arg(marshalledExpr)
                invocation
        }
    }

    private def translateStubTransaction(transaction: Transaction, newClass: JDefinedClass, translationContext: TranslationContext) : JMethod = {
        val javaRetType = transaction.retType match {
            case Some(typ) => resolveType(typ)
            case None => model.VOID
        }
        val meth: JMethod = newClass.method(JMod.PUBLIC, javaRetType, transaction.name)
        meth._throws(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException"))


        var argExpressions: List[IJExpression] = Nil
        /* add args */
        for (arg <- transaction.args) {
            argExpressions = argExpressions :+ meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */

        // argsArray = new Object[size]
        val body = meth.body()
        val objectArrayType = newClass.owner().ref("java.util.ArrayList").narrow(newClass.owner().ref("byte[]"))
        val newArrayExpr = JExpr._new(objectArrayType)
        newArrayExpr.arg(JExpr.lit(argExpressions.length))
        val argArray = body.decl(objectArrayType, "argArray", newArrayExpr)


        var i = 0;
        for (i <- 0 until argExpressions.length) {
            val unmarshalledArg = argExpressions(i)

            val marshalledArg = marshallExpr(unmarshalledArg, transaction.args(i).typ)
            val setInvocation = body.invoke(argArray, "add")
            setInvocation.arg(marshalledArg)
        }

        //connectionManager.doTransaction(transaction.name, args)
        val tryBlock = body._try()

        val doTransactionInvocation = JExpr.invoke(JExpr.ref("connectionManager"), "doTransaction")
        doTransactionInvocation.arg(transaction.name)
        doTransactionInvocation.arg(argArray)
        doTransactionInvocation.arg(transaction.retType.isDefined)

        if (transaction.retType.isDefined) {
            // return result
            val marshalledResultDecl = tryBlock.body().decl(newClass.owner().ref("byte[]"), "marshalledResult", doTransactionInvocation)


            val errorBlock = new JBlock()
            val expr = unmarshallExpr(marshalledResultDecl, transaction.retType.get, errorBlock)

            if (!errorBlock.isEmpty) {
                tryBlock.body().add(errorBlock)
            }

            val resultDecl = tryBlock.body().decl(javaRetType, "result", expr)
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

    private def translateStubState(s: State, inClass: JDefinedClass) : Unit = {
        // TODO
    }

    def makeFieldInfo(newClass: JDefinedClass, stateLookup: Map[String, StateContext])
                     (name: String, declSeq: Seq[(State, Field)]): FieldInfo = {
        val fieldType = resolveType(declSeq.head._2.typ)

        /* setup get method */
        val getMeth = newClass.method(JMod.PRIVATE, fieldType, fieldGetMethodName(name))
        val getBody = getMeth.body()
        for ((st, f) <- declSeq) {
            // dynamically check the state
            getBody._if(JExpr.invoke(getStateMeth).eq(stateLookup(st.name).enumVal))
                   ._then()
                   ._return(stateLookup(st.name).innerClassField.ref(f.fieldName))
        }
        // exhaustive return to keep the compiler happy
        getBody._return(JExpr._null())

        /* setup set method */
        val setMeth = newClass.method(JMod.PRIVATE, model.VOID, fieldSetMethodName(name))
        val setBody = setMeth.body()
        val newValue = setMeth.param(fieldType, "newValue")
        for ((st, f) <- declSeq) {
            // dynamically check the state
            setBody._if(JExpr.invoke(getStateMeth).eq(stateLookup(st.name).enumVal))
                ._then()
                .assign(stateLookup(st.name).innerClassField.ref(f.fieldName), newValue)
        }

        StateSpecificFieldInfo(declSeq, getMeth, setMeth)
    }

    def makeTransactionInfo(newClass: JDefinedClass, stateLookup: Map[String, StateContext])
                           (name: String, declSeq: Seq[(State, Transaction)]): TransactionInfo = {
        val txExample = declSeq.head._2

        val (hasReturn, retType) = txExample.retType match {
            case Some(typ) => (true, resolveType(typ))
            case None => (false, model.VOID)
        }

        val meth = newClass.method(JMod.PUBLIC, retType, txExample.name)

        /* add the appropriate args to the method and collect them in a list */
        val jArgs = txExample.args.map( (arg: VariableDecl) => meth.param(resolveType(arg.typ), arg.varName) )

        val body = meth.body()

        for ((st, f) <- declSeq) {
            val inv = JExpr.invoke(stateLookup(st.name).innerClassField, txExample.name)

            /* add args to the invocation */
            jArgs.foldLeft(inv)((inv: JInvocation, arg: JVar) => inv.arg(arg))

            // dynamically check the state
            val stBody = body._if(JExpr.invoke(getStateMeth).eq(stateLookup(st.name).enumVal))._then()

            if (hasReturn) stBody._return(inv)
            else stBody.add(inv)
        }
        // exhaustive return to please compiler
        if (hasReturn) body._return(JExpr._null())

        StateSpecificTransactionInfo(declSeq, meth)
    }

    def makeFuncInfo(newClass: JDefinedClass, stateLookup: Map[String, StateContext])
                    (name: String, declSeq: Seq[(State, Func)]): FuncInfo = {
        val funExample = declSeq.head._2

        val (hasReturn, retType) = funExample.retType match {
            case Some(typ) => (true, resolveType(typ))
            case None => (false, model.VOID)
        }

        val meth = newClass.method(JMod.PUBLIC, retType, funExample.name)

        /* add the appropriate args to the method and collect them in a list */
        val jArgs = funExample.args.map( (arg: VariableDecl) => meth.param(resolveType(arg.typ), arg.varName) )

        val body = meth.body()

        for ((st, f) <- declSeq) {
            val inv = JExpr.invoke(stateLookup(st.name).innerClassField, funExample.name)

            /* add args to the invocation */
            jArgs.foldLeft(inv)((inv: JInvocation, arg: JVar) => inv.arg(arg))

            // dynamically check the state
            val stBody = body._if(JExpr.invoke(getStateMeth).eq(stateLookup(st.name).enumVal))._then()

            if (hasReturn) stBody._return(inv)
            else stBody.add(inv)
        }
        // exhaustive return to please compiler
        if (hasReturn) body._return(JExpr._null())

        StateSpecificFuncInfo(declSeq, meth)
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
                        (Map[String, FieldInfo], Map[String, TransactionInfo], Map[String, FuncInfo]) = {

        /* collect all declarations of any type that are particular to a state: each declaration
         * is paired with the state it is defined in */
        val declarations = contract.declarations
                .filter(_.isInstanceOf[State])
                .map(_.asInstanceOf[State])
                .flatMap((s: State) => s.declarations.zip(List.fill(s.declarations.size)(s)))

        /* collect all declarations by specific type */
        val fields = declarations.filter(_._1.isInstanceOf[Field])
                                 .map((x: (Declaration, State)) => (x._2, x._1.asInstanceOf[Field]))
        val txs = declarations.filter(_._1.isInstanceOf[Transaction])
                              .map((x: (Declaration, State)) => (x._2, x._1.asInstanceOf[Transaction]))
        val funs = declarations.filter(_._1.isInstanceOf[Func])
                               .map((x: (Declaration, State)) => (x._2, x._1.asInstanceOf[Func]))

        /* splits items in [ts] into groups based on equality of the result of applying [f] */
        def generalizedPartition[T, S](ts: List[T], f: Function[T, S]): immutable.HashMap[S, Seq[T]] = {
            ts match {
                case h :: rest =>
                    val (equiv, nonEquiv) = rest.partition(f(_).equals(f(h)))
                    generalizedPartition(nonEquiv, f).updated(f(h), h +: equiv)
                case _ => new immutable.HashMap[S, Seq[T]]()
            }
        }

        /* group declarations by name: this requires declarations with the same name to have the same type */
        if (generateStub) {
            (Map.empty, Map.empty, Map.empty)
        }
        else {
            val fieldInfoFunc = makeFieldInfo(newClass, stateLookup) _
            val transactionInfoFunc = makeTransactionInfo(newClass, stateLookup) _
            val functionInfoFunc = makeFuncInfo(newClass, stateLookup) _

            var fieldLookup = generalizedPartition[(State, Field), String](fields.toList, _._2.fieldName)
                .transform(fieldInfoFunc)
            var txLookup = generalizedPartition[(State, Transaction), String](txs.toList, _._2.name)
                .transform(transactionInfoFunc)
            var funLookup = generalizedPartition[(State, Func), String](funs.toList, _._2.name)
                .transform(functionInfoFunc)

            /* add on any whole-contract declarations */
            for (decl <- contract.declarations) {
                decl match {
                    case f@Field(_, fieldName) => fieldLookup = fieldLookup.updated(fieldName, GlobalFieldInfo(f))
                    case t@Transaction(name, _, _, _) => txLookup = txLookup.updated(name, GlobalTransactionInfo(t))
                    case f@Func(name, _, _, _) => funLookup = funLookup.updated(name, GlobalFuncInfo(f))
                    case _ => ()
                }
            }

            (fieldLookup, txLookup, funLookup)
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

        /* if we're not in the main contract, we need to ensure there's an empty constructor.
         * This constructor will be used in unarchiving; e.g. to unarchive class C:
         *          C c = new C(); c.initFromArchive(archive.getC().toByteArray());
         */
        if (!aContract.mod.contains(IsMain) && !hasEmptyConstructor(aContract)) {
            newClass.constructor(JMod.PUBLIC)
        }

        /* setup the state enum */
        val stateDeclarations: Seq[State] =
            aContract.declarations.filter((d: Declaration) => d match {
                case State(_, _) => true
                case _ => false
            }).map({ s => s.asInstanceOf[State] })

        var stateEnumOption: Option[JDefinedClass] = None
        var stateEnumField: Option[JFieldVar] = None
        if (stateDeclarations.nonEmpty) {
            val stateEnum = newClass._enum(JMod.PUBLIC, stateEnumNameForClassName(aContract.name))
            stateEnumOption = Some(stateEnum)

            /* Declare the states in the enum */
            for (State(name, _) <- stateDeclarations) {
                stateEnum.enumConstant(name)
            }

            /* setup the state field and the [getState] method */
            stateEnumField = Some(newClass.field(JMod.PRIVATE, stateEnum, stateField))
            val stateMeth = newClass.method(JMod.PUBLIC, stateEnum, getStateMeth)
            stateMeth.body()._return(JExpr.ref(stateField))
        }

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
        val (fieldLookup, txLookup, funLookup) = makeDeclarationLookupTables(aContract, stateLookup, newClass, generateStub)

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
            funLookup = funLookup,
            fieldLookup = fieldLookup
        )

        /* i.e. if this contract defines any type states */
        if (translationContext.stateEnumClass.isDefined)
            generateStateHelpers(newClass, translationContext)

        translationContext
    }


    private def generateStateHelpers(newClass: JDefinedClass,
                                     translationContext: TranslationContext): Unit = {
        /* method to take care of conserved fields during a state transition
         * Invariant for use: [getStateMeth] returns the current state, the new state is passed
         * as an argument. The new state's inner class field must be non-null. */
        val conserveMeth = newClass.method(JMod.PRIVATE, model.VOID, conserveFieldsName)
        val nextState = conserveMeth.param(translationContext.stateEnumClass.get, "nextState")
        val conserveBody = conserveMeth.body()
        /* match on the current state */
        for (stFrom <- translationContext.states.values) {
            val thisStateBody = conserveBody._if(
                    JExpr.invoke(getStateMeth).eq(stFrom.enumVal))
                ._then()
            /* match on the target state */
            for (stTo <- translationContext.states.values if stTo != stFrom) {
                val assignBody = thisStateBody._if(
                        nextState.eq(stTo.enumVal))
                    ._then()
                val (fromName, toName) = (stFrom.astState.name, stTo.astState.name)
                for (f <- translationContext.conservedFields(fromName, toName)) {
                    /* assign the field to the new state from the old */
                    assignBody.assign(stTo.innerClassField.ref(f.fieldName),
                                        stFrom.innerClassField.ref(f.fieldName))
                }
            }
        }

        /* method to null out old state
         * Invariant for use: [getStateMeth] returns the current state; this will be the state
         * whose inner class field this method nullifies. */
        val deleteBody = newClass.method(JMod.PRIVATE, model.VOID, deleteOldStateName).body()
        for (st <- translationContext.states.values) {
            deleteBody._if(
                    JExpr.invoke(getStateMeth).eq(st.enumVal))
                ._then()
                .assign(st.innerClassField, JExpr._null())
        }
    }

    private def translateContract(
                    aContract: Contract,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext) = {

        for (decl <- aContract.declarations) {
            translateDeclaration(decl, newClass, translationContext, None, aContract)
        }

        translationContext
    }

    // Contracts translate to compilation units containing one class.
    private def translateOuterContract(aContract: Contract,
                                       programPackage: JPackage,
                                       protobufOuterClassName: String,
                                       contractNameResolutionMap: Map[Contract, String],
                                       protobufOuterClassNames: Map[String, String]) = {
        val newClass: JDefinedClass = programPackage._class(aContract.name)

        val translationContext = makeTranslationContext(aContract, newClass, contractNameResolutionMap, protobufOuterClassNames, false)
        translateContract(aContract, newClass, translationContext)


        target match {
            case Client(mainContract) =>
                if (aContract == mainContract) {
                    newClass._extends(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientBase"))
                    generateClientMainMethod(newClass)
                    generateInvokeClientMainMethod(aContract, newClass)
                }
                else {
                    generateSerialization(aContract, newClass, translationContext)
                }
            case Server() =>
                if (aContract.mod.contains(IsMain)) {
                    /* We need to generate special methods for the main contract to align */
                    /* with the Hyperledger chaincode format */
                    generateMainServerClassMethods(newClass, translationContext)

                }
                generateSerialization(aContract, newClass, translationContext)
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

    private def generateMainServerClassMethods(newClass: JDefinedClass, translationContext: TranslationContext): Unit = {
        newClass._extends(model.directClass("edu.cmu.cs.obsidian.chaincode.ChaincodeBaseMock"))
        val stubType = model.directClass("edu.cmu.cs.obsidian.chaincode.ChaincodeStubMock")

        /* run method */
        generateRunMethod(newClass, translationContext, stubType)

        /* init method */
        generateInitMethod(newClass, stubType)

        /* query method */
        val queryMeth: JMethod = newClass.method(JMod.PUBLIC, model.BYTE.array(), "query")
        queryMeth.param(stubType, "stub")
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

    private def generateInitMethod(
                    newClass: JDefinedClass,
                    stubType: AbstractJClass): Unit = {
        val initMeth: JMethod = newClass.method(JMod.PUBLIC, model.BYTE.array(), "init")
        initMeth.param(stubType, "stub")
        initMeth.param(model.BYTE.array().array(), "args")

        mainConstructor.foreach(c => initMeth.body().invoke(c))

        initMeth.body()._return(JExpr.newArray(model.BYTE, 0));
    }

    private def generateRunMethod(
                    newClass: JDefinedClass,
                    translationContext: TranslationContext,
                    stubType: AbstractJClass): Unit = {
        val runMeth = newClass.method(JMod.PUBLIC, model.BYTE.array(), "run")
        runMeth.param(stubType, "stub")
        runMeth.param(model.ref("String"), "transName")
        val runArgs = runMeth.param(model.BYTE.array().array(), "args")

        val returnBytes = runMeth.body().decl(
                              model.BYTE.array(), "returnBytes",
                              JExpr.newArray(model.BYTE, 0))

        /* for each possible transaction, we have a branch in the run method */
        for ((state, tx) <- mainTransactions) {
            val cond = {
                state match {
                    case Some(stName) =>
                        JExpr.ref(stateField)
                            .eq(
                                translationContext.getEnum(stName))
                            .band(
                                JExpr.ref("transName")
                                    .invoke("equals").arg(
                                        JExpr.lit(tx.name))
                            )
                    case None =>
                        JExpr.ref("transName")
                            .invoke("equals").arg(
                                JExpr.lit(tx.name))
                }
            }

            val stateCond = runMeth.body()._if(cond)
            val stateCondBody = stateCond._then()

            /* parse the (typed) args from raw bytes */
            var txArgsList: List[JVar] = List.empty
            var runArgNumber = 0
            for (txArg <- tx.args) {

                var newTxArg: JVar = null
                if (txArg.typ.isInstanceOf[IntType]) {
                    newTxArg = stateCondBody.decl(
                        resolveType(txArg.typ),
                        txArg.varName,
                        JExpr._new(resolveType(txArg.typ)).arg(
                            runArgs.component(JExpr.lit(runArgNumber))
                        ))
                } else {
                    newTxArg = stateCondBody.decl(
                        resolveType(txArg.typ),
                        txArg.varName,
                        JExpr._new(resolveType(txArg.typ)))
                    stateCondBody.invoke(newTxArg, "__initFromArchiveBytes")
                        .arg(runArgs.component(JExpr.lit(runArgNumber)))
                }
                txArgsList = newTxArg :: txArgsList
                runArgNumber += 1
            }

            var txInvoke: JInvocation = null

            if (tx.retType.isDefined) {
                txInvoke = JExpr.invoke(tx.name)
                if (tx.retType.get.isInstanceOf[IntType]) {
                    stateCondBody.assign(returnBytes, txInvoke.invoke("toByteArray"))
                } else {
                    stateCondBody.assign(returnBytes, txInvoke.invoke("archiveBytes"))
                }
            } else {
                txInvoke = stateCondBody.invoke(tx.name)
            }

            for (txArg <- txArgsList.reverse) {
                txInvoke.arg(txArg)
            }


        }

        runMeth.body()._return(returnBytes)
    }

    private def generateServerMainMethod(newClass: JDefinedClass) = {
        val mainMeth = newClass.method(JMod.STATIC | JMod.PUBLIC, model.VOID, "main")
        val args = mainMeth.param(model.ref("String").array(), "args")

        // main takes one argument specifying the location of the store.
        // If the specified file does not exist, it creates a new store when it exits.

        // newClass instance = new newClass();
        // instance.delegatedMain(args);
        val body = mainMeth.body()
        val instance = body.decl(newClass, "instance", JExpr._new(newClass))
        val invocation = body.invoke(instance, "delegatedMain")
        invocation.arg(args)
    }

    private def generateClientMainMethod(newClass: JDefinedClass) = {
        val mainMeth = newClass.method(JMod.STATIC | JMod.PUBLIC, model.VOID, "main")
        val args = mainMeth.param(model.ref("String").array(), "args")

        // main takes an address and a port of a server to connect to.

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
        method._throws(model.ref("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException"))

        val mainTransactionOption: Option[Transaction] = aContract.declarations.find((d: Declaration) => d.isInstanceOf[Transaction] && d.asInstanceOf[Transaction].name.equals("main"))
                                                                  .asInstanceOf[Option[Transaction]]

        if (mainTransactionOption.isEmpty) {
            println("Error: can't find main transaction in main contract " + aContract.name)
        }
        else {
            val mainTransaction: Transaction = mainTransactionOption.get

            // The main transaction expects to be passed a stub of a particular type. Construct it.
            val stubType: Type= mainTransaction.args(0).typ
            val stubJavaType = resolveType(stubType)
            val newStubExpr = JExpr._new(stubJavaType)
            newStubExpr.arg(JExpr.ref("connectionManager"))
            val stubVariable = method.body().decl(stubJavaType, "stub", newStubExpr)
            val clientMainInvocation = method.body.invoke("main")
            clientMainInvocation.arg(stubVariable)
        }

    }

    private def generateSerialization(
                    contract: Contract,
                    inClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        generateSerializer(contract, inClass)
        generateArchiver(contract, inClass, translationContext)
        generateArchiveInitializer(contract, inClass, translationContext)


        val subcontracts = contract.declarations.filter(d => d.isInstanceOf[Contract])

        for (c <- contract.declarations if c.isInstanceOf[Contract]) {
            val innerContract: Contract = c.asInstanceOf[Contract]
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

    // "set" followed by lowercasing the field name.
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

    private def generateFieldArchiver(
                    field: Field,
                    fieldVar: JVar,
                    builderVar: JFieldRef,
                    body: JBlock,
                    translationContext: TranslationContext,
                    inContract: Contract): Unit = {
        val ifNonNull: JConditional = body._if(fieldVar.ne(JExpr._null()))
        val nonNullBody = ifNonNull._then()
        val javaFieldType = resolveType(field.typ)
        field.typ match {
            case IntType() => {
                // Special serialization for BigInteger, since that's how the Obsidian int type gets translated.
                // The protobuf type for this is just bytes.
                // builder.setField(ByteString.CopyFrom(field.toByteArray()))
                val setterName: String = setterNameForField(field.fieldName)
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
                // TODO
            }
            case StringType() => {
                // TODO
            }
            case n@NonPrimitiveType(_, name) => {
                val contract = resolveNonPrimitiveTypeToContract(n, translationContext, inContract)
                if (contract.isEmpty) {
                    println("Compilation error: unable to resolve type " + name)
                }
                else {
                    val archiveVariableTypeName = translationContext.getProtobufClassName(contract.get)
                    val archiveVariableType: AbstractJType = model.parseType(archiveVariableTypeName)

                    val archiveVariableInvocation = JExpr.invoke(fieldVar, "archive")
                    val archiveVariable = nonNullBody.decl(archiveVariableType,
                        field.fieldName + "Archive",
                        archiveVariableInvocation)

                    // generate: builder.setField(field);
                    val setterName: String = setterNameForField(field.fieldName)

                    val invocation: JInvocation = nonNullBody.invoke(builderVar, setterName)
                    invocation.arg(archiveVariable)
                }
            }
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
        val archiveBody = archiveMethod.body()

        val protobufMessageClassBuilder: String = protobufClassName + ".Builder"
        val builderType = model.parseType(protobufMessageClassBuilder)

        /* TODO obviously this workaround is bad; see if there's another way */
        archiveBody.directStatement(protobufMessageClassBuilder + " builder = " +
            protobufClassName + ".newBuilder();")
        val builderVariable = JExpr.ref("builder")

        val declarations = state.declarations

        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = stateClass.fields().get(field.fieldName)
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
        val archiveBody = archiveMethod.body()

        val protobufMessageClassBuilder: String = protobufClassName + ".Builder"
        val builderType = model.parseType(protobufMessageClassBuilder)

        /* TODO obviously this workaround is bad; see if there's another way */
        archiveBody.directStatement(protobufMessageClassBuilder + " builder = " +
                                    protobufClassName + ".newBuilder();")
        val builderVariable = JExpr.ref("builder")
        // val builderVariable: JVar = archiveBody.decl(builderType, "builder", archiveType.staticInvoke("newBuilder"))
        // Iterate through fields of this class and archive each one by calling setters on a builder.

        val declarations = contract.declarations

        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = inClass.fields().get(field.fieldName)
            generateFieldArchiver(field, javaFieldVar, builderVariable, archiveBody, translationContext, contract)
        }

        /* handle states if there are any */
        if (translationContext.states.nonEmpty) {
            for (stDecl <- declarations.filter(d => d.isInstanceOf[State])) {
                val st = stDecl.asInstanceOf[State]
                val cond = translationContext.getEnum(st.name).eq(JExpr.invoke(getStateMeth))
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

        val archiveBody = archiveMethod.body()

        val archive = JExpr.invoke(JExpr._this(), "archive")
        val archiveBytes = archive.invoke("toByteArray")
        archiveBody._return(archiveBytes);
    }

    private def generateFieldInitializer(
                    field: Field,
                    fieldVar: JVar,
                    body: JBlock,
                    archive: JVar,
                    translationContext: TranslationContext,
                    inContract: Contract): Unit = {
        // generate: FieldArchive fieldArchive = field.archive();
        val javaFieldName: String = field.fieldName
        val javaFieldType: AbstractJType = resolveType(field.typ)

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
                // TODO
            }
            case StringType() => {
                // TODO
            }
            case n@NonPrimitiveType(_, name) => {
                // foo = new Foo(); foo.initFromArchive(archive.getFoo());
                val javaFieldTypeName = javaFieldType.fullName()
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
                    body.assign(fieldVar, JExpr._new(javaFieldType))
                    val init = body.invoke(fieldVar, "initFromArchive")
                    init.arg(archive.invoke(getterNameForField(javaFieldName)))
                }
            }
        }
    }

    private def generateStateArchiveInitializer(
                    contract: Contract,
                    state: State,
                    stateClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        val contractProtobufClassName = translationContext.getProtobufClassName(contract)
        val protobufClassName = contractProtobufClassName + "." + state.name

        val archiveType = model.directClass(protobufClassName)

        /* [initFromArchive] setup */
        val fromArchiveMeth = stateClass.method(JMod.PUBLIC, model.VOID, "initFromArchive")
        val archive = fromArchiveMeth.param(archiveType, "archive")
        val fromArchiveBody = fromArchiveMeth.body()

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

        // Call setters.
        val declarations = state.declarations

        /* this takes care of fields that are not specific to any particular state */
        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = stateClass.fields().get(field.fieldName)
            generateFieldInitializer(field, javaFieldVar, fromArchiveBody, archive, translationContext, contract)
        }

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
        val archive = fromArchiveMeth.param(archiveType, "archive")
        val fromArchiveBody = fromArchiveMeth.body()

        /* [__initFromArchiveBytes] declaration: this just parses the archive and
         * calls [initFromArchive] */
        val fromBytesMeth =
            newClass.method(JMod.PUBLIC, newClass, "__initFromArchiveBytes")
        val exceptionType = model.parseType("com.google.protobuf.InvalidProtocolBufferException")
        fromBytesMeth._throws(exceptionType.asInstanceOf[AbstractJClass])
        val archiveBytes = fromBytesMeth.param(model.parseType("byte[]"), "archiveBytes")

        val fromBytesBody = fromBytesMeth.body()
        val parseInvocation: JInvocation = archiveType.staticInvoke("parseFrom")
        parseInvocation.arg(archiveBytes)
        val parsedArchive = fromBytesBody.decl(archiveType, "archive", parseInvocation)
        fromBytesBody.invoke(fromArchiveMeth).arg(parsedArchive)
        fromBytesBody._return(JExpr._this())

        /* [initFromArchive] does most of the grunt work of mapping the protobuf message to
         * fields of the class: the following code does this */

        // Call setters.
        val declarations = contract.declarations

        /* this takes care of fields that are not specific to any particular state */
        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = newClass.fields().get(field.fieldName)
            generateFieldInitializer(field, javaFieldVar, fromArchiveBody, archive, translationContext, contract)
        }

        val enumGetter = "getStateCase"
        val enumName = (stateName: String) =>
            protobufClassName + "." + "StateCase" + "." + ("STATE" + stateName).toUpperCase

        /* set state enum */
        for (stDecl <- declarations if stDecl.isInstanceOf[State]) {
            val st = stDecl.asInstanceOf[State]
            val thisStateBody = fromArchiveBody._if(
                archive.invoke(enumGetter).invoke("equals").arg(
                    JExpr.direct(enumName(st.name)))
            )._then()
            thisStateBody.assign(JExpr.ref(stateField), translationContext.getEnum(st.name))
        }

        /* this takes care of state-specific fields */
        for (stDecl <- declarations if stDecl.isInstanceOf[State]) {
            val st = stDecl.asInstanceOf[State]

            val stArchiveName = protobufClassName + "." + st.name
            val stArchiveType: AbstractJType = model.parseType(stArchiveName)

            val innerClass = translationContext.states(st.name).innerClass
            val innerClassField = translationContext.states(st.name).innerClassField

            val thisStateBody = fromArchiveBody._if(
                    archive.invoke(enumGetter).invoke("equals").arg(
                    JExpr.direct(enumName(st.name)))
                )._then()

            val stArchiveGetter = "getState" + st.name
            val stArchive =
                thisStateBody.decl(stArchiveType, "stateArchive", archive.invoke(stArchiveGetter))

            thisStateBody.assign(innerClassField, JExpr._new(innerClass))
            thisStateBody.invoke(innerClassField, "initFromArchive").arg(stArchive)
        }
    }

    private def translateDeclaration(
                    declaration: Declaration,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext,
                    currentState: Option[String],
                    aContract: Contract): Unit = {
        (aContract.mod, declaration) match {

            /* the main contract has special generated code, so many functions are different */
            case (Some(IsMain), c@Constructor(_,_,_)) =>
                mainConstructor = Some(translateMainConstructor(c, newClass, translationContext))
            case (Some(IsMain), f@Field(_,_)) =>
                translateFieldDecl(f, newClass)
            case (Some(IsMain), f@Func(_,_,_,_)) =>
                translateFuncDecl(f, newClass, translationContext)
            case (Some(IsMain), t@Transaction(_,_,_,_)) =>
                translateTransDecl(t, newClass, translationContext)
                mainTransactions.add((currentState, t))
            case (Some(IsMain), s@State(_,_)) =>
                translateStateDecl(s, aContract, newClass, translationContext)
            case (Some(IsMain), c@Contract(_,_,_)) => translateInnerContract(c, newClass, translationContext)

            // TODO : shared contracts
            /* shared contracts will generate a sort of shim to interact with
             * a remotely deployed chaincode. */
            case (Some(IsShared), c@Constructor(_,_,_)) => ()
            case (Some(IsShared), f@Field(_,_)) => ()
            case (Some(IsShared), f@Func(_,_,_,_)) => ()
            case (Some(IsShared), t@Transaction(_,_,_,_)) => ()
            case (Some(IsShared), s@State(_,_)) => ()
            case (Some(IsShared), c@Contract(_,_,_)) => ()

            /* Unique contracts and nested contracts are translated the same way */
            case (_, c@Constructor(_,_,_)) =>
                translateConstructor(c, newClass, translationContext)
            case (_, f@Field(_,_)) =>
                translateFieldDecl(f, newClass)
            case (_, f@Func(_,_,_,_)) =>
                translateFuncDecl(f, newClass, translationContext)
            case (_, t@Transaction(_,_,_,_)) =>
                translateTransDecl(t, newClass, translationContext)
            case (_, s@State(_,_)) =>
                translateStateDecl(s, aContract, newClass, translationContext)
            case (_, c@Contract(_,_,_)) => translateInnerContract(c, newClass, translationContext)

            case _ => () // TODO : type declarations
        }
    }

    private def resolveType(typ: Type): AbstractJType = {
        typ match {
            case IntType() => model.directClass("java.math.BigInteger")
            case BoolType() => model.BOOLEAN
            case StringType() => model.ref("String")
            case NonPrimitiveType(_, "address") => model.directClass("java.math.BigInteger")
            case NonPrimitiveType(modifiers, name) => if (modifiers.contains(IsRemote)) model.ref(classNameForStub(name)) else model.ref(name)
        }
    }

    // The NonPrimitiveType stores the type name as a string; this method figures out which contract that maps to
    // according to the scope in which the type name is used.
    private def resolveNonPrimitiveTypeToContract(typ: NonPrimitiveType,
                                                  translationContext: TranslationContext,
                                                  containingContract: Contract): Option[Contract] = {
        var typeComponents = typ.name.split(".")
        if (typeComponents.isEmpty) {
            typeComponents = Array(typ.name)
        }
        //val fullyQualifiedContainingContractName = translationContext.contractNameResolutionMap(containingContract)
        //val containingContractComponents = fullyQualifiedContainingContractName.split(".")

        // Suppose the containing contract is A.B.C and we're looking up B. We want to find A.B unless there's A.B.C.B.
        // We start our search with the innermost contract and ascend until we find a contract with the name we want.

        // recursiveTypeLookup does the actual search.
        def recursiveTypeLookup(containingContract: Contract, typeComponents: Seq[String]): Option[Contract] = {

            // matchContract looks for a contract WITHIN the given contract.
            def matchContract(containingContract: Contract, typeComponents: Seq[String]): Option[Contract] = {
                if (typeComponents.length == 0) Some(containingContract)
                else {
                    val innerContracts = containingContract.declarations.filter((decl: Declaration) => decl.isInstanceOf[Contract])
                    val innerContract = innerContracts.find((decl: Declaration) => decl.asInstanceOf[Contract].name.equals(typeComponents.head))
                    if (innerContract.isDefined) matchContract(innerContract.get.asInstanceOf[Contract], typeComponents.tail)
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
            val foundPair = translationContext.contractNameResolutionMap.find((pair: (Contract, String)) => pair._2.equals(typ.name))
            if (foundPair.isDefined) Some(foundPair.get._1) else None
        }
    }

    private def fieldInitializerForType(typ: Type): Option[IJExpression] = {
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
            case NonPrimitiveType(mod, name) => None
        }
    }

    private def translateFieldDecl(decl: Field, newClass: JDefinedClass): Unit = {
        val initializer = fieldInitializerForType(decl.typ)
        if (initializer.isDefined) {
            newClass.field(JMod.PRIVATE, resolveType(decl.typ), decl.fieldName, initializer.get)
        }
        else {
            newClass.field(JMod.PRIVATE, resolveType(decl.typ), decl.fieldName)
        }
    }

    private def addArgs(inv: JInvocation,
                        args: Seq[Expression],
                        translationContext: TranslationContext,
                        localContext: Map[String, JVar]): JInvocation = {
        val foldF = (inv: JInvocation, arg: Expression) =>
            inv.arg(translateExpr(arg, translationContext, localContext))
        args.foldLeft(inv)(foldF)
    }

    /* returns an expr because exprs are built bottom-up (unlike everything else) */
    private def translateExpr(e: Expression,
                              translationContext: TranslationContext,
                              localContext: Map[String, JVar]): IJExpression = {
        val recurse = (e: Expression) => translateExpr(e, translationContext, localContext)

        e match {
            case Variable(x) => dereferenceVariable(x, translationContext, localContext)
            case NumLiteral(n) => model.directClass("java.math.BigInteger").
                                    staticInvoke("valueOf").arg(JExpr.lit(n))
            case StringLiteral(s) => JExpr.lit(s)
            case TrueLiteral() => JExpr.TRUE
            case FalseLiteral() => JExpr.FALSE
            case This() => JExpr._this()
            case Conjunction(e1, e2) => recurse(e1).band(recurse(e2))
            case Disjunction(e1, e2) => recurse(e1).bor(recurse(e2))
            case LogicalNegation(e1) => recurse(e1).not()
            case Add(e1, e2) => recurse(e1).invoke("add").arg(recurse(e2))
            case Subtract(e1, e2) => recurse(e1).invoke("subtract").arg(recurse(e2))
            case Multiply(e1, e2) => recurse(e1).invoke("multiply").arg(recurse(e2))
            case Divide(e1, e2) => recurse(e1).invoke("divide").arg(recurse(e2))
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
                addArgs(translationContext.invokeTransactionOrFunction(name), args, translationContext, localContext)
            /* TODO : this shouldn't be an extra case */
            case Invocation(This(), name, args) =>
                addArgs(translationContext.invokeTransactionOrFunction(name), args, translationContext, localContext)
            case Invocation(recipient, name, args) =>
                addArgs(JExpr.invoke(recurse(recipient), name), args, translationContext, localContext)
            case Construction(name, args) =>
                addArgs(JExpr._new(model.ref(name)), args, translationContext, localContext)
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
        for (decl <- state.declarations) {
            translateDeclaration(decl, stateClass, newTranslationContext, Some(state.name), contract)
        }
        generateStateArchiveInitializer(contract, state, stateClass, translationContext)
        generateStateArchiver(contract, state, stateClass, translationContext)
    }

    /* the local context at the beginning of the method */

    /* The java constructor in the main contract runs every transaction.
     * The obsidian constructor only runs when the contract is deployed.
     * Thus, the obsidian constructor must be placed in a distinct method. */
    private def translateMainConstructor(
                                            c: Constructor,
                                            newClass: JDefinedClass,
                                            translationContext: TranslationContext) : JMethod = {
        val name = "new_" + newClass.name()

        val meth: JMethod = newClass.method(JMod.PRIVATE, model.VOID, name)

        /* add args to method and collect them in a list */
        val argList: Seq[(String, JVar)] = c.args.map((arg: VariableDecl) =>
                (arg.varName, meth.param(resolveType(arg.typ), arg.varName))
            )

        /* construct the local context from this list */
        val localContext: immutable.Map[String, JVar] = argList.toMap

        /* add body */
        translateBody(meth.body(), c.body, translationContext, localContext)

        meth
    }

    private def translateConstructor(
                                        c: Constructor,
                                        newClass: JDefinedClass,
                                        translationContext: TranslationContext) : JMethod = {
        val meth: JMethod = newClass.constructor(JMod.PUBLIC)

        /* add args to method and collect them in a list */
        val argList: Seq[(String, JVar)] = c.args.map((arg: VariableDecl) =>
            (arg.varName, meth.param(resolveType(arg.typ), arg.varName))
        )

        /* construct the local context from this list */
        val localContext: immutable.Map[String, JVar] = argList.toMap

        /* add body */
        translateBody(meth.body(), c.body, translationContext, localContext)

        meth
    }

    private def translateTransDecl(
                    decl: Transaction,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext): JMethod = {
        val javaRetType = decl.retType match {
            case Some(typ) => resolveType(typ)
            case None => model.VOID
        }
        val meth: JMethod = newClass.method(JMod.PUBLIC, javaRetType, decl.name)
        target match {
            case Client(mainContract) =>
                if ((translationContext.contract == mainContract) && decl.name.equals("main")) {
                    meth._throws(model.directClass("edu.cmu.cs.obsidian.client.ChaincodeClientAbortTransactionException"))
            }
            case _ =>
        }


        /* add args to method and collect them in a list */
        val argList: Seq[(String, JVar)] = decl.args.map((arg: VariableDecl) =>
            (arg.varName, meth.param(resolveType(arg.typ), arg.varName))
        )

        /* construct the local context from this list */
        val localContext: immutable.Map[String, JVar] = argList.toMap

        /* add body */
        translateBody(meth.body(), decl.body, translationContext, localContext)

        meth
    }

    /* these methods make shadowing possible */

    private def dereferenceVariable(name: String,
                                    translationContext: TranslationContext,
                                    localContext: Map[String, JVar]): IJExpression = {
        localContext.get(name) match {
            case Some(variable) => variable
            case None => translationContext.dereferenceVariable(name)
        }
    }

    private def assignVariable(name: String,
                               newValue: IJExpression,
                               body: JBlock,
                               translationContext: TranslationContext,
                               localContext: Map[String, JVar]
                              ): Unit = {
        localContext.get(name) match {
            case Some(variable) => body.assign(variable, newValue)
            case None => translationContext.assignVariable(name, newValue, body)
        }
    }

    private def translateStatement(
                    body: JBlock,
                    statement: Statement,
                    translationContext: TranslationContext,
                    localContext: Map[String, JVar]): Map[String, JVar] = {
        var nextContext = localContext
        statement match {
            case VariableDecl(typ, name) =>
                nextContext = localContext.updated(name, body.decl(resolveType(typ), name, JExpr._null()))
            case Return => body._return()
            case ReturnExpr(e) => body._return(translateExpr(e, translationContext, localContext))
            case Transition(newState, updates) =>
                /* We must (in this order):
                 *     1) construct the new state's inner class,
                 *     2) assign the fields of the new inner class object,
                 *     3) clean up the old state,
                 *     4) change the state enum.
                 */
                /* construct a new instance of the inner contract */
                val newStField = translationContext.states(newState).innerClassField
                body.assign(newStField, JExpr._new(translationContext.states(newState).innerClass))

                /* assign fields in the update construct */
                for ((f, e) <- updates) {
                    body.assign(newStField.ref(f.x), translateExpr(e, translationContext, localContext))
                }

                /* assign conserved fields (implicit to programmer) */
                body.invoke(conserveFieldsName).arg(translationContext.states(newState).enumVal)

                /* nullify old state inner class field */
                body.invoke(deleteOldStateName)

                /* change the enum to reflect the new state */
                body.assign(JExpr.ref(stateField), translationContext.getEnum(newState))

            case Assignment(Variable(x), e) =>
                assignVariable(x, translateExpr(e, translationContext,localContext),
                    body, translationContext, localContext)
            /* it's bad that this is a special case */
            case Assignment(Dereference(This(), field), e) => {
                /* we don't check the local context and just assume it's a field */
                val newValue = translateExpr(e, translationContext,localContext)
                translationContext.assignVariable(field, newValue, body)
            }
            case Assignment(Dereference(eDeref, field), e) => {
                // TODO: do we ever need this in the general case if all contracts are encapsulated?
            }

            case Throw() =>
                body._throw(JExpr._new(model.ref("RuntimeException")))

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
                    JExpr.invoke(jEx, "getState").invoke("toString").invoke("equals").arg(JExpr.lit(s))
                val jIf = body._if(eqState(h.stateName))
                translateBody(jIf._then(), h.body, translationContext, localContext)

                var jPrev = jIf
                for (_case <- remainingCases) {
                    jPrev = jPrev._elseif(eqState(_case.stateName))
                    translateBody(jPrev._then(), _case.body, translationContext, localContext)
                }
            case LocalInvocation(methName, args) =>
                addArgs(translationContext.invokeTransactionOrFunction(methName),
                        args, translationContext, localContext)
            /* TODO : it's bad that this is a special case */
            case Invocation(This(), methName, args) =>
                addArgs(translationContext.invokeTransactionOrFunction(methName),
                        args, translationContext, localContext)

            case Invocation(e, methName, args) =>
                addArgs(body.invoke(translateExpr(e, translationContext, localContext), methName),
                        args, translationContext, localContext)

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

    private def translateFuncDecl(
                    decl: Func,
                    newClass: JDefinedClass,
                    translationContext: TranslationContext): Unit = {
        val javaRetType = decl.retType match {
            case Some(typ) => resolveType(typ)
            case None => model.VOID
        }
        val meth: JMethod = newClass.method(JMod.PRIVATE, javaRetType, decl.name)

        /* add args to method and collect them in a list */
        val argList: Seq[(String, JVar)] = decl.args.map((arg: VariableDecl) =>
            (arg.varName, meth.param(resolveType(arg.typ), arg.varName))
        )

        /* construct the local context from this list */
        val localContext: immutable.Map[String, JVar] = argList.toMap

        /* add body */
        translateBody(meth.body(), decl.body, translationContext, localContext)
    }
}

package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser._
import com.sun.codemodel.internal.{JClass, _}

import scala.collection._

class CodeGen () {
    private val model: JCodeModel = new JCodeModel()

    /* we must keep track of the transactions in main so that we can match on
     * them in the "invoke" function. Each tuple [(Some(s), tr)] is a transaction
     * represented by a transaction [tr], in a state identified by [s]. If the first
     * element in the pair is [None], then the transaction works in all states */
    private val mainTransactions: mutable.Set[(Option[String], Transaction)] =
        new mutable.HashSet[(Option[String], Transaction)]()

    /* we track the constructor for essentially the same reason */
    private var mainConstructor: Option[JMethod] = None

    private final val stateField: String = "__state"
    private final val getStateMeth: String = "getState"
    private def stateEnumName(className: String): String = {
        "State_" + className
    }
    final val packageName: String = "edu.cmu.cs.obsidian.generated_code"

    def translateServerProgram(program: Program, protobufOuterClassName: String): JCodeModel = {
        translateProgram(program, Some(protobufOuterClassName));
    }

    def translateProgram(program: Program, protobufOuterClassName: Option[String]): JCodeModel = {
        // Put all generated code in the same package.
        val programPackage: JPackage = model._package(packageName)
        for (imp <- program.imports) {
            translateImport(programPackage, imp)
        }

        for (c <- program.contracts) {
            translateOuterContract(c, programPackage, protobufOuterClassName)
        }
        model
    }

    def translateClientProgram(program: Program): JCodeModel = {
        translateProgram(program, None);
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

    private def translateImport(programPackage: JPackage, imp: Import): Unit = {
        // Each import corresponds to a file. Each file has to be read, parsed, and translated into a list of stub contracts.
        val filename = imp.name;

    }

    /* factors out shared functionality for translate[Outer|Inner]Contract.
     * returns Some(stateEnum) if the contract has any states (None otherwise). */
    private def translateContract(
                    aContract: Contract,
                    newClass: JDefinedClass,
                    outerClassNames: Option[List[String]]): Option[JDefinedClass] = {

        /* if we're not in the main contract, we need to ensure there's an empty constructor.
         * This constructor will be used in unarchiving; e.g. to unarchive class C:
         *          C c = new C(); c.initFromArchive(archive.getC().toByteArray());
         */
        if (!aContract.mod.contains(IsMain) && !hasEmptyConstructor(aContract)) {
            newClass.constructor(JMod.PUBLIC)
        }

        /* setup the state enum */
        val stateDeclarations: Seq[Declaration] =
            aContract.declarations.filter((d: Declaration) => d match {
                case State(_, _) => true
                case _ => false
            })

        var stateEnumOption: Option[JDefinedClass] = None
        if (stateDeclarations.nonEmpty) {
            val stateEnum = newClass._enum(JMod.PUBLIC, stateEnumName(aContract.name))
            stateEnumOption = Some(stateEnum)

            /* Declare the states in the enum */
            for (State(name, _) <- stateDeclarations) {
                stateEnum.enumConstant(name)
            }

            /* setup the state field and the [getState] method */
            newClass.field(JMod.PRIVATE, stateEnum, stateField)
            val stateMeth = newClass.method(JMod.PUBLIC, stateEnum, getStateMeth)
            stateMeth.body()._return(JExpr.ref(stateField))
        }

        for (decl <- aContract.declarations) {
            translateDeclaration(decl, newClass, stateEnumOption, None, aContract.mod, outerClassNames)
        }

        stateEnumOption
    }

    // Contracts translate to compilation units containing one class.
    private def translateOuterContract(aContract: Contract, programPackage: JPackage, protobufOuterClassName: Option[String]): Unit = {
        val newClass: JDefinedClass = programPackage._class(aContract.name)

        val nestedProtobufClassNames = if (protobufOuterClassName.isDefined) Some(protobufOuterClassName.get::Nil) else None

        val stateEnumOption = translateContract(aContract, newClass, nestedProtobufClassNames)

        /* We need to generate special methods for the main contract to align
         * with the Hyperledger chaincode format */
        if (aContract.mod.contains(IsMain)) {
            generateMainServerClassMethods(newClass, stateEnumOption)
        }

        /* Generate serialization code */
        if (protobufOuterClassName.isDefined) {
            generateSerialization(aContract, newClass, stateEnumOption, nestedProtobufClassNames.get)
        }
        else {
            generateClientRunMethod(aContract, newClass)
        }
    }

    private def translateInnerContract(aContract: Contract, parent: JDefinedClass, outerClassNames: Option[List[String]]): Unit = {
        val newClass: JDefinedClass = parent._class(JMod.PUBLIC, aContract.name)
        val _ = translateContract(aContract, newClass, outerClassNames)
    }

    private def generateMainServerClassMethods(newClass: JDefinedClass, stateEnumOption: Option[JDefinedClass]): Unit = {
        newClass._extends(model.directClass("edu.cmu.cs.obsidian.chaincode.ChaincodeBaseMock"))
        val stubType = model.directClass("edu.cmu.cs.obsidian.chaincode.ChaincodeStubMock")

        /* run method */
        generateRunMethod(newClass, stateEnumOption, stubType)

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
                    stubType: JClass): Unit = {
        val initMeth: JMethod = newClass.method(JMod.PUBLIC, model.BYTE.array(), "init")
        initMeth.param(stubType, "stub")
        initMeth.param(model.BYTE.array().array(), "args")

        mainConstructor.foreach(c => initMeth.body().invoke(c))

        initMeth.body()._return(JExpr.newArray(model.BYTE, 0));
    }

    private def generateRunMethod(
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass],
                    stubType: JClass): Unit = {
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
                                stateEnumOption.get.enumConstant(stName))
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
                    stateCondBody.invoke(newTxArg, "initFromArchiveBytes")
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

    // The "run" method is where the contents of the
    private def generateClientRunMethod(newClass: JDefinedClass, aContract: Contract) = {

    }

    private def generateSerialization(
                    contract: Contract,
                    inClass: JDefinedClass,
                    stateEnum: Option[JDefinedClass],
                    outerClassNames: List[String]): Unit = {
        generateSerializer(contract, inClass)
        generateArchiver(contract, inClass, stateEnum, outerClassNames)
        generateArchiveInitializer(contract, inClass, stateEnum, outerClassNames)


        val subcontracts = contract.declarations.filter(d => d.isInstanceOf[Contract])

        for (c <- contract.declarations if c.isInstanceOf[Contract]) {
            val innerContract: Contract = c.asInstanceOf[Contract]
            val javaInnerClasses = inClass.listClasses()
            val javaInnerClassOption = javaInnerClasses.find((c: JClass) => (c.name().equals(innerContract.name)))
            /* TODO : find state enum, if the inner contract has states */
            if (javaInnerClassOption.isDefined) {
                generateSerialization(innerContract, javaInnerClassOption.get.asInstanceOf[JDefinedClass], null, contract.name::outerClassNames)
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

    private def protobufMessageClassNameForClassName(className: String, outerClassNames: List[String]): String = {
        /*
        val outerClassPath = outerClassNames.foldRight(packageName)(
            (c: String, accum: String) => accum + "." + c
        )
        */

        val messageTypeName: String = outerClassNames.last + "." + className

        return messageTypeName
    }

    private def generateFieldArchiver(
                    field: Field,
                    fieldVar: JVar,
                    builderVar: JFieldRef,
                    body: JBlock,
                    classNamePath: List[String]): Unit = {

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

                val byteStringClass: JClass =
                    model.parseType("com.google.protobuf.ByteString").asInstanceOf[JClass]
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
            case NonPrimitiveType(_, name) => {
                val archiveVariableTypeName =
                    protobufMessageClassNameForClassName(javaFieldType.name(), classNamePath)
                val archiveVariableType: JType = model.parseType(archiveVariableTypeName)

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


    // Generates a method, archive(), which outputs a protobuf object corresponding to the archive of this class.
    private def generateArchiver(
                    contract: Contract,
                    inClass: JDefinedClass,
                    stateEnum: Option[JDefinedClass],
                    outerClassNames: List[String]): Unit = {
        val protobufClassName = protobufMessageClassNameForClassName(contract.name, outerClassNames)
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
        val classNamePath = contract.name :: outerClassNames
        // Iterate through fields of this class and archive each one by calling setters on a builder.

        val declarations = contract.declarations

        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = inClass.fields().get(field.fieldName)
            generateFieldArchiver(field, javaFieldVar, builderVariable, archiveBody, classNamePath)
        }

        /* handle states if there are any */
        if (stateEnum.isDefined) {
            for (stDecl <- declarations.filter(d => d.isInstanceOf[State])) {
                val st = stDecl.asInstanceOf[State]
                val cond = stateEnum.get.enumConstant(st.name).eq(JExpr.invoke(getStateMeth))
                val thisStateBody = archiveBody._if(cond)._then()

                val stProtobufClassName: String = protobufClassName + "." + st.name
                val stProtobufBuilder: String = stProtobufClassName + ".Builder"
                val builderType = model.parseType(stProtobufBuilder)
                /* TODO same workaround as above */
                thisStateBody.directStatement(stProtobufBuilder + " stBuilder = " +
                    stProtobufClassName + ".newBuilder();")
                val stBuilderVar = JExpr.ref("stBuilder")

                for (f <- st.declarations.filter(d => d.isInstanceOf[Field])) {
                    val field = f.asInstanceOf[Field]
                    val javaFieldVar = inClass.fields().get(field.fieldName)
                    generateFieldArchiver(field, javaFieldVar, stBuilderVar, thisStateBody, classNamePath)
                }

                thisStateBody.invoke(builderVariable, "setState" + st.name)
                    .arg(stBuilderVar.invoke("build"))
            }
        }

        val buildInvocation = JExpr.invoke(builderVariable, "build")
        archiveBody._return(buildInvocation)
    }


    // Generates a method, archiveBytes(), which outputs a string in protobuf format.
    private def generateSerializer(contract: Contract, inClass: JDefinedClass): Unit = {
        val archiveMethod = inClass.method(JMod.PUBLIC, model.parseType("byte[]"), "archiveBytes")

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
                    classNamePath: List[String]): Unit = {
        // generate: FieldArchive fieldArchive = field.archive();
        val javaFieldName: String = field.fieldName
        val javaFieldType: JType = resolveType(field.typ)

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
            case NonPrimitiveType(_, name) => {
                // foo = new Foo(); foo.initFromArchive(archive.getFoo());
                val javaFieldTypeName = javaFieldType.fullName()

                val archiveName =
                    protobufMessageClassNameForClassName(javaFieldType.name(), classNamePath)
                val archiveType: JType = model.parseType(archiveName)

                // TODO
                /* generate another method that takes the actual archive type
                 * so we don't have to uselessly convert to bytes here */
                body.assign(fieldVar, JExpr._new(javaFieldType))
                val init = body.invoke(fieldVar, "initFromArchive")
                init.arg(archive.invoke(getterNameForField(javaFieldName)))

            }
        }
    }

    /* generates the method [initFromArchiveBytes] and [initFromArchive];
     * these methods load the recipient class from a protobuf message in the
     * form of raw bytes and a java message protobuf object, respectively.
     * Overrides initFromArchiveBytes() in superclass if this is the main contract */
    private def generateArchiveInitializer(
                    contract: Contract,
                    newClass: JDefinedClass,
                    stateEnum: Option[JDefinedClass],
                    outerClassNames: List[String]): Unit = {

        val protobufClassName =
            protobufMessageClassNameForClassName(contract.name, outerClassNames)
        val archiveType = model.directClass(protobufClassName)

        /* [initFromArchive] setup */
        val fromArchiveMeth = newClass.method(JMod.PUBLIC, model.VOID, "initFromArchive")
        val archive = fromArchiveMeth.param(archiveType, "archive")
        val fromArchiveBody = fromArchiveMeth.body()

        /* [initFromArchiveBytes] declaration: this just parses the archive and
         * calls [initFromArchive] */
        val fromBytesMeth =
            newClass.method(JMod.PUBLIC, model.VOID, "initFromArchiveBytes")
        val exceptionType = model.parseType("com.google.protobuf.InvalidProtocolBufferException")
        fromBytesMeth._throws(exceptionType.asInstanceOf[JClass])
        val archiveBytes = fromBytesMeth.param(model.parseType("byte[]"), "archiveBytes")

        val fromBytesBody = fromBytesMeth.body()
        val parseInvocation: JInvocation = archiveType.staticInvoke("parseFrom")
        parseInvocation.arg(archiveBytes)
        val parsedArchive = fromBytesBody.decl(archiveType, "archive", parseInvocation)
        fromBytesBody.invoke(fromArchiveMeth).arg(parsedArchive)


        /* [initFromArchive] does most of the grunt work of mapping the protobuf message to
         * fields of the class: the following code does this */
        val classNamePath = contract.name :: outerClassNames

        // Call setters.
        val declarations = contract.declarations

        /* this takes care of fields that are not specific to any particular state */
        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]
            val javaFieldVar = newClass.fields().get(field.fieldName)
            generateFieldInitializer(field, javaFieldVar, fromArchiveBody, archive, classNamePath)
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
            thisStateBody.assign(JExpr.ref(stateField), stateEnum.get.enumConstant(st.name))
        }

        /* this takes care of state-specific fields */
        for (stDecl <- declarations if stDecl.isInstanceOf[State]) {
            val st = stDecl.asInstanceOf[State]


            val stArchiveName = protobufClassName + "." + st.name
            val stArchiveType: JType = model.parseType(stArchiveName)

            val thisStateBody = fromArchiveBody._if(
                    archive.invoke(enumGetter).invoke("equals").arg(
                    JExpr.direct(enumName(st.name)))
                )._then()

            val stArchiveGetter = "getState" + st.name
            val stArchive =
                thisStateBody.decl(stArchiveType, "stateArchive", archive.invoke(stArchiveGetter))

            for (f <- st.asInstanceOf[State].declarations if f.isInstanceOf[Field]) {
                val field: Field = f.asInstanceOf[Field]
                val javaFieldVar = newClass.fields().get(field.fieldName)
                generateFieldInitializer(field, javaFieldVar, thisStateBody, stArchive, classNamePath)
            }
        }
    }

    private def translateDeclaration(
                    declaration: Declaration,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass],
                    currentState: Option[String],
                    mod: Option[ContractModifier],
                    outerClassNames: Option[List[String]]): Unit = {
        (mod, declaration) match {

            /* the main contract has special generated code, so many functions are different */
            case (Some(IsMain), c@Constructor(_,_,_)) =>
                mainConstructor = Some(translateMainConstructor(c, newClass, stateEnumOption))
            case (Some(IsMain), f@Field(_,_)) =>
                translateFieldDecl(f, newClass)
            case (Some(IsMain), f@Func(_,_,_,_)) =>
                translateFuncDecl(f, newClass, stateEnumOption)
            case (Some(IsMain), t@Transaction(_,_,_,_)) =>
                translateTransDecl(t, newClass, stateEnumOption)
                mainTransactions.add((currentState, t))
            case (Some(IsMain), s@State(_,_)) =>
                translateStateDecl(s, newClass, stateEnumOption, mod, outerClassNames)
            case (Some(IsMain), c@Contract(_,_,_)) => translateInnerContract(c, newClass, outerClassNames)

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
                translateConstructor(c, newClass, stateEnumOption)
            case (_, f@Field(_,_)) =>
                translateFieldDecl(f, newClass)
            case (_, f@Func(_,_,_,_)) =>
                translateFuncDecl(f, newClass, stateEnumOption)
            case (_, t@Transaction(_,_,_,_)) =>
                translateTransDecl(t, newClass, stateEnumOption)
            case (_, s@State(_,_)) =>
                translateStateDecl(s, newClass, stateEnumOption, mod, outerClassNames)
            case (_, c@Contract(_,_,_)) => translateInnerContract(c, newClass, outerClassNames)

            case _ => () // TODO : type declarations
        }
    }

    private def resolveType(typ: Type): JType = {
        typ match {
            case IntType() => model.directClass("java.math.BigInteger")
            case BoolType() => model.BOOLEAN
            case StringType() => model.ref("String")
            case NonPrimitiveType(_, "address") => model.directClass("java.math.BigInteger")
            case NonPrimitiveType(_, name) => model.ref(name)
        }
    }

    /* The java constructor in the main contract runs every transaction.
     * The obsidian constructor only runs when the contract is deployed.
     * Thus, the obsidian constructor must be placed in a distinct method. */
    private def translateMainConstructor(
                    c: Constructor,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass]) : JMethod = {
        val name = "new_" + newClass.name()

        val meth: JMethod = newClass.method(JMod.PRIVATE, model.VOID, name)

        /* add args */
        for (arg <- c.args) {
            meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */
        translateBody(meth.body(), c.body, stateEnumOption)

        meth
    }

    private def translateConstructor(
                    c: Constructor,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass]) : JMethod = {
        val meth: JMethod = newClass.constructor(JMod.PUBLIC)

        /* add args */
        for (arg <- c.args) {
            meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */
        translateBody(meth.body(), c.body, stateEnumOption)

        meth
    }

    private def translateFieldDecl(decl: Field, newClass: JDefinedClass): Unit = {
        newClass.field(JMod.PRIVATE, resolveType(decl.typ), decl.fieldName)
    }

    /* returns an expr because exprs are built bottom-up (unlike everything else) */
    private def translateExpr(e: Expression): JExpression = {
        /* helper for invocations */
        val foldF = (inv: JInvocation, arg: Expression) =>
            inv.arg(translateExpr(arg))

        e match {
            case Variable(x) => JExpr.ref(x)
            case NumLiteral(n) => model.directClass("java.math.BigInteger").
                                    staticInvoke("valueOf").arg(JExpr.lit(n))
            case StringLiteral(s) => JExpr.lit(s)
            case TrueLiteral() => JExpr.TRUE
            case FalseLiteral() => JExpr.FALSE
            case This() => JExpr._this()
            case Conjunction(e1, e2) => translateExpr(e1).band(translateExpr(e2))
            case Disjunction(e1, e2) => translateExpr(e1).bor(translateExpr(e2))
            case LogicalNegation(e1) => translateExpr(e1).not()
            case Add(e1, e2) => translateExpr(e1).invoke("add").arg(translateExpr(e2))
            case Subtract(e1, e2) => translateExpr(e1).invoke("subtract").arg(translateExpr(e2))
            case Multiply(e1, e2) => translateExpr(e1).invoke("multiply").arg(translateExpr(e2))
            case Divide(e1, e2) => translateExpr(e1).invoke("divide").arg(translateExpr(e2))
            case Equals(e1, e2) => translateExpr(e1).invoke("equals").arg(translateExpr(e2))
            case GreaterThan(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).eq(JExpr.lit(1))
            case GreaterThanOrEquals(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).ne(JExpr.lit(-1))
            case LessThan(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).eq(JExpr.lit(-1))
            case LessThanOrEquals(e1, e2) =>
                translateExpr(e1).invoke("compareTo").arg(translateExpr(e2)).ne(JExpr.lit(1))
            case NotEquals(e1, e2) =>
                translateExpr(e1).invoke("equals").arg(translateExpr(e2)).not()
            case Dereference(e1, f) => translateExpr(e1).ref(f)
            case LocalInvocation(name, args) =>
                args.foldLeft(JExpr.invoke(name))(foldF)
            case Invocation(rec, name, args) =>
                args.foldLeft(JExpr.invoke(translateExpr(rec), name))(foldF)
            case Construction(name, args) =>
                args.foldLeft(JExpr._new(model.ref(name)))(foldF)
        }
    }

    private def translateStateDecl(
                    state: State,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass],
                    mod: Option[ContractModifier],
                    outerClassNames: Option[List[String]]): Unit = {
        for (decl <- state.declarations) {
            translateDeclaration(decl, newClass, stateEnumOption, Some(state.name), mod, outerClassNames)
        }
    }

    private def translateTransDecl(
                    decl: Transaction,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass]): JMethod = {
        val javaRetType = decl.retType match {
            case Some(typ) => resolveType(typ)
            case None => model.VOID
        }
        val meth: JMethod = newClass.method(JMod.PUBLIC, javaRetType, decl.name)

        /* add args */
        for (arg <- decl.args) {
            meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */
        translateBody(meth.body(), decl.body, stateEnumOption)

        meth
    }


    private def translateStatement(
                    body: JBlock,
                    statement: Statement,
                    stateEnumOption: Option[JDefinedClass]): Unit = {
        statement match {
            case VariableDecl(typ, name) => body.decl(resolveType(typ), name, JExpr._null())
            case Return => body._return()
            case ReturnExpr(e) => body._return(translateExpr(e))
            case Transition(newState, updates) =>
                translateBody(body, updates, stateEnumOption)
                body.assign(JExpr.ref(stateField), stateEnumOption.get.enumConstant(newState)) // XXX DO NOT MODITY ENUM HERE
            case Assignment(Dereference(eDeref, field), e) => {
                // TODO: this should call method to set state, not assign manually
                val fRef = JExpr.ref(translateExpr(eDeref), field)
                body.assign(fRef, translateExpr(e))
            }
            case Assignment(Variable(x), e) =>
                val ref = JExpr.ref(x)
                body.assign(ref, translateExpr(e))

            case Throw() =>
                body._throw(JExpr._new(model.ref("RuntimeException")))

            case If(e, s) =>
                translateBody(body._if(translateExpr(e))._then(), s, stateEnumOption)
            case IfThenElse(e, s1, s2) =>
                val jIf = body._if(translateExpr(e))
                translateBody(jIf._then(), s1, stateEnumOption)
                translateBody(jIf._else(), s2, stateEnumOption)

            case TryCatch(s1, s2) =>
                val jTry = body._try()
                val jCatch = jTry._catch(model.ref("RuntimeException"))
                translateBody(jTry.body(), s1, stateEnumOption)
                translateBody(jCatch.body(), s2, stateEnumOption)

            case Switch(e, cases) =>
                val h :: remainingCases = cases
                val jEx = translateExpr(e)
                val eqState = (s: String) =>
                    // TODO
                    /* somewhat of a bad workaround, but the alternative involves knowing the
                     * type of the expression jEx: in general, this requires an analysis
                     * to link references to declarations */
                    JExpr.invoke(jEx, "getState").invoke("toString").invoke("equals").arg(JExpr.lit(s))
                val jIf = body._if(eqState(h.stateName))
                translateBody(jIf._then(), h.body, stateEnumOption)

                var jPrev = jIf
                for (_case <- remainingCases) {
                    jPrev = jPrev._elseif(eqState(_case.stateName))
                    translateBody(jPrev._then(), _case.body, stateEnumOption)
                }

            case LocalInvocation(methName, args) =>
                val inv = body.invoke(methName)

                for (arg <- args) {
                    inv.arg(translateExpr(arg))
                }

            case Invocation(e, methName, args) =>
                val inv = body.invoke(translateExpr(e), methName)

                for (arg <- args) {
                    inv.arg(translateExpr(arg))
                }


            /* all expressions can be statements but no other expressions have a reasonable meaning */
            case _ => ()
        }
    }

    private def translateBody(
                    body: JBlock,
                    statements: Seq[Statement],
                    stateEnumOption: Option[JDefinedClass]): Unit = {
        for (st <- statements) {
            translateStatement(body, st, stateEnumOption)
        }
    }

    private def translateFuncDecl(
                    decl: Func,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass]): Unit = {
        val javaRetType = decl.retType match {
            case Some(typ) => resolveType(typ)
            case None => model.VOID
        }
        val meth: JMethod = newClass.method(JMod.PRIVATE, javaRetType, decl.name)

        /* add args */
        for (arg <- decl.args) {
            meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */
        translateBody(meth.body(), decl.body, stateEnumOption)
    }
}

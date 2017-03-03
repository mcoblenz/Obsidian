package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.util._
import com.sun.codemodel.internal.{JClass, _}

import scala.collection._

class CodeGen () {
    private val model: JCodeModel = new JCodeModel()

    /* we must keep track of the transactions in main so that we can match on
     * them in the "invoke" function. Each pair [(Some(s), m)] is a transaction
     * represented by a method [m], in a state identified by [s]. If the first
     * element in the pair is [None], then the transaction works in all states */
    private val mainTransactions: mutable.Set[(Option[String], JMethod)] =
        new mutable.HashSet[(Option[String], JMethod)]()

    /* we track the constructor for essentially the same reason */
    private var mainConstructor: Option[JMethod] = None

    private final val stateField: String = "__state"
    private final val getStateMeth: String = "getState"
    private def stateEnumName(className: String): String = {
        "State_" + className
    }
    final val packageName: String = "edu.cmu.cs.obsidian.generated_code"

    def translateProgram(program: Program, protobufOuterClassName: String): JCodeModel = {
        // Put all generated code in the same package.
        val programPackage: JPackage = model._package(packageName)
        for (c <- program.contracts) {
            translateOuterContract(c, programPackage, protobufOuterClassName)
        }
        model
    }

    /* factors out shared functionality for translate[Outer|Inner]Contract.
     * returns Some(stateEnum) if the contract has any states (None otherwise). */
    private def translateContract(
                    aContract: Contract,
                    newClass: JDefinedClass,
                    outerClassNames: List[String]): Option[JDefinedClass] = {
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
    private def translateOuterContract(aContract: Contract, programPackage: JPackage, protobufOuterClassName: String): Unit = {
        val newClass: JDefinedClass = programPackage._class(aContract.name)

        val stateEnumOption = translateContract(aContract, newClass, protobufOuterClassName::Nil)

        /* We need to generate special methods for the main contract to align
         * with the Hyperledger chaincode format */
        if (aContract.mod.contains(IsMain)) {
            generateMain(newClass, stateEnumOption)
        }

        /* Generate serialization code */
        generateSerialization(aContract, newClass, protobufOuterClassName::Nil)
    }

    private def translateInnerContract(aContract: Contract, parent: JDefinedClass, outerClassNames: List[String]): Unit = {
        val newClass: JDefinedClass = parent._class(JMod.PUBLIC, aContract.name)
        val _ = translateContract(aContract, newClass, outerClassNames)
    }

    private def generateMain(newClass: JDefinedClass, stateEnumOption: Option[JDefinedClass]): Unit = {
        newClass._extends(model.directClass("edu.cmu.cs.obsidian.chaincode.ChaincodeBaseMock"))
        val stubType = model.directClass("edu.cmu.cs.obsidian.chaincode.ChaincodeStubMock")

        /* run method */
        val runMeth = newClass.method(JMod.PUBLIC, model.ref("String"), "run")
        runMeth.param(stubType, "stub")
        runMeth.param(model.ref("String"), "transName")
        runMeth.param(model.ref("String").array(), "args")


        for ((state, tr) <- mainTransactions) {
            val cond = {
                state match {
                    case Some(stName) =>
                        JExpr.ref(stateField)
                            .eq(
                        stateEnumOption.get.enumConstant(stName))
                            .band(
                        JExpr.ref("transName")
                            .eq(
                        JExpr.lit(tr.name()))
                            )
                    case None =>
                        JExpr.ref("transName")
                            .eq(
                        JExpr.lit(tr.name()))
                }
            }
            val stateCond = runMeth.body()._if(cond)
            // TODO : need to parse args here and use for invocation
            stateCond._then().invoke(tr)
        }
        runMeth.body()._return(JExpr.lit(""))

        /* init method */
        // TODO : do we need this? How does construction work with Java chaincode
        val initMeth: JMethod = newClass.method(JMod.PUBLIC, model.ref("String"), "init")
        initMeth.param(stubType, "stub")
        initMeth.param(model.ref("String").array(), "args")

        mainConstructor.foreach(c => initMeth.body().invoke(c))

        initMeth.body()._return(JExpr.lit(""))


        /* query method */
        val queryMeth: JMethod = newClass.method(JMod.PUBLIC, model.ref("String"), "query")
        queryMeth.param(stubType, "stub")
        queryMeth.param(model.ref("String"), "transName")
        queryMeth.param(model.ref("String").array(), "args")

        // TODO
        queryMeth.body()._return(JExpr.lit(""))


        /* getChaincodeID */
        val idMeth = newClass.method(JMod.PUBLIC, model.ref("String"), "getChaincodeID")
        idMeth.body()._return(JExpr.lit(""))
        // TODO

        // TODO
        /* this is TEMPORARY: we don't want this constructor for real chaincode.
         * in the real chaincode, this will just load the fields from the store */
        val newMeth = newClass.constructor(JMod.PUBLIC)
        newMeth.body().invoke("init").arg(JExpr._null()).arg(JExpr._null())

        /* Main Method */
        val mainMeth = newClass.method(JMod.STATIC | JMod.PUBLIC, model.VOID, "main")
        mainMeth.param(model.ref("String").array(), "args")
        mainMeth.body().directStatement("""System.out.println("hello world\n");""")
        // TODO
    }


    private def generateSerialization(contract: Contract, inClass: JDefinedClass, outerClassNames: List[String]): Unit = {
        generateSerializer(contract, inClass)
        generateArchiver(contract, inClass, outerClassNames)
        generateArchiveConstructor(contract, inClass)
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

    private def protobufMessageClassNameForClassName(className: String, outerClassNames: List[String]): String = {
        val outerClassPath = outerClassNames.foldRight(packageName)(
            (c: String, accum: String) => accum + "." + c
        )

        val messageTypeName: String = outerClassPath + "." + className

        return messageTypeName
    }

    // Generates a method, archive(), which outputs a protobuf object corresponding to the archive of this class.
    private def generateArchiver(contract: Contract, inClass: JDefinedClass, outerClassNames: List[String]): Unit = {
        val protobufMessageClassName = protobufMessageClassNameForClassName(contract.name, outerClassNames)
        val archiveType = model.parseType(protobufMessageClassName).asInstanceOf[JClass]
        val archiveMethod = inClass.method(JMod.PUBLIC, archiveType, "archive")
        val archiveBody = archiveMethod.body()

        val protobufMessageClassBuilder: String = protobufMessageClassName + ".Builder"
        val builderType = model.parseType(protobufMessageClassBuilder)
        val builderVariable: JVar = archiveBody.decl(builderType, "builder", archiveType.staticInvoke("newBuilder"))
        // Iterate through fields of this class and archive each one by calling setters on a builder.


        val declarations = contract.declarations

        for (f <- declarations if f.isInstanceOf[Field]) {
            val field: Field = f.asInstanceOf[Field]

            // generate: FieldArchive fieldArchive = field.archive();
            val javaFieldName: String = field.fieldName
            val javaFieldType: JType = resolveType(field.typ)
            val javaFieldMap = inClass.fields()
            val javaFieldVariable: JVar = javaFieldMap.get(javaFieldName)

            if (javaFieldType.isPrimitive) {
                // TODO
            }
            else if (javaFieldType.fullName().equals("java.math.BigInteger")) {
                // Special serialization for BigInteger, since that's how the Obsidian int type gets translated.
                // The protobuf type for this is just bytes.
                // builder.setField(ByteString.CopyFrom(field.toByteArray()))
                val setterName: String = setterNameForField(javaFieldName)
                val setInvocation = archiveBody.invoke(builderVariable, setterName)

                val byteStringClass: JClass = model.parseType("com.google.protobuf.ByteString").asInstanceOf[JClass]
                val toByteArrayInvocation = JExpr.invoke(javaFieldVariable, "toByteArray")
                val copyFromInvocation = byteStringClass.staticInvoke("copyFrom")
                copyFromInvocation.arg(toByteArrayInvocation)
                setInvocation.arg(copyFromInvocation)
            }
            else {
                val javaFieldTypeName = javaFieldType.fullName()
                //
                val archiveVariableTypeName = protobufMessageClassNameForClassName(javaFieldType.name(), outerClassNames)
                val archiveVariableType: JType = model.parseType(archiveVariableTypeName)

                val archiveVariableInvocation = JExpr.invoke(javaFieldVariable, "archive")
                val archiveVariable = archiveBody.decl(archiveVariableType, javaFieldName + "Archive", archiveVariableInvocation)

                // generate: builder.setField(field);
                val setterName: String = setterNameForField(javaFieldName)

                val invocation: JInvocation = archiveBody.invoke(builderVariable, setterName)
                invocation.arg(archiveVariable)
            }
        }

        val subcontracts = contract.declarations.filter(d => d.isInstanceOf[Contract])

        for (c <- contract.declarations if c.isInstanceOf[Contract]) {
            val innerContract: Contract = c.asInstanceOf[Contract]
            val javaInnerClasses = inClass.listClasses()
            val javaInnerClassOption = javaInnerClasses.find((c: JClass) => (c.name().equals(innerContract.name)))
            if (javaInnerClassOption.isDefined) {
                generateArchiver(innerContract, javaInnerClassOption.get.asInstanceOf[JDefinedClass], contract.name :: outerClassNames)
            }
            else {
                println("Bug: can't find inner class in generated Java code for inner class " + innerContract.name)
            }
        }

        // Serialize state enum.
        // builder.set__State(...)

        val buildInvocation = JExpr.invoke(builderVariable, "build")
        archiveBody._return(buildInvocation)
    }


    // Generates a method, archiveString(), which outputs a string in protobuf format.
    private def generateSerializer(contract: Contract, inClass: JDefinedClass): Unit = {
        // TODO
        /*
        val archiveMethod = inClass.method(JMod.PUBLIC, model.parseType("String"), "archiveString")

        val archiveBody = archiveMethod.body()
        */
    }

    private def generateArchiveConstructor(contract: Contract, newClass: JDefinedClass): Unit = {
        // TODO
    }

    private def translateDeclaration(
                    declaration: Declaration,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass],
                    currentState: Option[String],
                    mod: Option[ContractModifier],
                    outerClassNames: List[String]): Unit = {
        (mod, declaration) match {

            /* the main contract has special generated code, so many functions are different */
            case (Some(IsMain), c@Constructor(_,_,_)) =>
                mainConstructor = Some(translateMainConstructor(c, newClass, stateEnumOption))
            case (Some(IsMain), f@Field(_,_)) =>
                translateFieldDecl(f, newClass)
            case (Some(IsMain), f@Func(_,_,_)) =>
                translateFuncDecl(f, newClass, stateEnumOption)
            case (Some(IsMain), t@Transaction(_,_,_)) =>
                val entry = (currentState, translateTransDecl(t, newClass, stateEnumOption))
                mainTransactions.add(entry)
            case (Some(IsMain), s@State(_,_)) =>
                translateStateDecl(s, newClass, stateEnumOption, mod, outerClassNames)
            case (Some(IsMain), c@Contract(_,_,_)) => translateInnerContract(c, newClass, outerClassNames)

            // TODO : shared contracts
            /* shared contracts will generate a sort of shim to interact with
             * a remotely deployed chaincode. */
            case (Some(IsShared), c@Constructor(_,_,_)) => ()
            case (Some(IsShared), f@Field(_,_)) => ()
            case (Some(IsShared), f@Func(_,_,_)) => ()
            case (Some(IsShared), t@Transaction(_,_,_)) => ()
            case (Some(IsShared), s@State(_,_)) => ()
            case (Some(IsShared), c@Contract(_,_,_)) => ()

            /* Unique contracts and nested contracts are translated the same way */
            case (_, c@Constructor(_,_,_)) =>
                translateConstructor(c, newClass, stateEnumOption)
            case (_, f@Field(_,_)) =>
                translateFieldDecl(f, newClass)
            case (_, f@Func(_,_,_)) =>
                translateFuncDecl(f, newClass, stateEnumOption)
            case (_, t@Transaction(_,_,_)) =>
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
                    outerClassNames: List[String]): Unit = {
        for (decl <- state.declarations) {
            translateDeclaration(decl, newClass, stateEnumOption, Some(state.name), mod, outerClassNames)
        }
    }

    private def translateTransDecl(
                    decl: Transaction,
                    newClass: JDefinedClass,
                    stateEnumOption: Option[JDefinedClass]): JMethod = {
        val meth: JMethod = newClass.method(JMod.PUBLIC, model.VOID, decl.name)

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
            case VariableDecl(typ, name) => body.decl(resolveType(typ), name)
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
                val h :: _ = cases
                val jEx = translateExpr(e)
                val eqState = (s: String) =>
                    JExpr.invoke(jEx, "getState").eq(JExpr.lit(s))

                val jIf = body._if(eqState(h.stateName))
                translateBody(jIf._then(), h.body, stateEnumOption)

                var jPrev = jIf
                for (_case <- cases) {
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
        val meth: JMethod = newClass.method(JMod.PRIVATE, model.VOID, decl.name)

        /* add args */
        for (arg <- decl.args) {
            meth.param(resolveType(arg.typ), arg.varName)
        }

        /* add body */
        translateBody(meth.body(), decl.body, stateEnumOption)
    }
}

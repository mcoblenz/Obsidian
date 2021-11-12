package edu.cmu.cs.obsidian.codegen

import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.codegen.Util._
import edu.cmu.cs.obsidian.typecheck.{IntType, ObsidianType}

import java.io.{FileReader, StringWriter}

// reminder: use abstract class if want to create a base class that requires constructor arguments
sealed trait YulAST

object LiteralKind extends Enumeration {
    type LiteralKind = Value
    val number: codegen.LiteralKind.Value = Value("int")
    val boolean: codegen.LiteralKind.Value = Value("bool")
    val string: codegen.LiteralKind.Value = Value("string")
}

trait Expression extends YulAST

trait YulStatement extends YulAST

// for each asm struct, create a case class
case class TypedName(name: String, typ: ObsidianType) extends YulAST {
    override def toString: String = {
        name
/*
ideally we'd want to do something like this:

        s"$name ${
            if (ntype.isEmpty) {
                ""
            } else {
                s" : $ntype"
            }
        }"

but as of Sept2021, solc does not support that output even though it's in the def, e.g.

Error: "string" is not a valid type (user defined types are not yet supported).
   --> /sources/SetGetNoArgsNoConstructNoInit.yul:144:29:
    |
144 | function IntContainer___get(this  : string) -> _ret_2 {
    |                             ^^^^^^^^^^^^^^
*/
    }
}

case class Case(value: Literal, body: Block) extends YulAST {
    override def toString: String = {
        s"case ${value.toString} ${brace(body.toString)}"
    }
}

case class Literal(kind: LiteralKind.LiteralKind, value: String, vtype: String) extends Expression {
    override def toString: String = {
        val msg: String = "internal error: literal with inconsistent type string"
        kind match {
            case LiteralKind.number => assert(vtype == "int", msg)
            case LiteralKind.boolean => assert(vtype == "bool", msg)
            case LiteralKind.string => assert(vtype == "string", msg)
        }
        // from the spec, "Unless it is the default type, the type of a literal has to be specified
        // after a colon", which seems like this should be what we want to do:
        //
        // s"${value}:${mapObsTypeToABI(vtype)}"
        //
        // however, as of 12 April 2021, this produces a ton of warnings from solc about "user
        // defined types are not yet supported"
        kind match {
            case edu.cmu.cs.obsidian.codegen.LiteralKind.number => value
            case edu.cmu.cs.obsidian.codegen.LiteralKind.boolean => value
            case edu.cmu.cs.obsidian.codegen.LiteralKind.string => quote(value)
        }
    }
}

case class Identifier(name: String) extends Expression {
    override def toString: String = {
        name
    }
}

case class FunctionCall(functionName: Identifier, arguments: Seq[Expression]) extends Expression {
    override def toString: String = {
        s"${functionName.toString}${paren(arguments.map(id => id.toString).mkString(", "))}"
    }
}

case class Assignment(variableNames: Seq[Identifier], value: Expression) extends YulStatement {
    override def toString: String = {
        s"${variableNames.map(id => id.name).mkString(", ")} := ${value.toString}"
    }
}

/**
  * This class represents variable declarations in Yul. These take the form of one or more names,
  * optionally paired with types, and 0 or 1 expressions to which they should be bound. Some examples
  * are and how they would be represented by arguments to this class are shown informally below:
  * let x := 7                      ~~> <(x,none)>,some(7)
  * let x := add(y,3)               ~~> <(x,none)>,some(add(y,3))
  * // here, since x has no type it's assumed to be u256 and gets assigned 0
  * let x                           ~~> <(x,none)>,none
  * let v:uint32 := 0 : uint32      ~~> <(x,some(uint32)>,some(0)
  * let x,y = g()                   ~~> <(x,none),(y,none)>,some(g())
  * let v:uint256, t := f()         ~~> <(v,some(uint256),(t,none)>,some(f())
  *
  * @param variables the sequence of variables, paired with their optional types, to declare
  * @param value     the optional expression to which to bind the variables declared
  */
case class VariableDeclaration(variables: Seq[(Identifier, Option[String])], value: Option[Expression]) extends YulStatement {
    override def toString: String = {
        // todo:
        // added below after ._1.name, this code correctly adds type annotations to the output Yul,
        // but as of Version: 0.8.1+commit.df193b15.Linux.g++ of solc, you get errors like
        // "Error: "bool" is not a valid type (user defined types are not yet supported)."
        // when you run that code through solc even though the spec says otherwise.
        /*
                        + (v._2 match {
                            case Some(t) => s" : $t"
                            case None => ""
                        })
        */

        s"let ${
            variables.map(v => v._1.name).mkString(", ")
        }" +
            (value match {
                case Some(e) => s" := ${e.toString}"
                case None => ""
            }

                )
    }
}

case class FunctionDefinition(name: String,
                              parameters: Seq[TypedName],
                              returnVariables: Seq[TypedName],
                              body: Block) extends YulStatement {
    override def toString: String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/function.mustache"), "function")
        val scope = new FuncScope(this)
        mustache.execute(new StringWriter(), scope).toString
    }
}

case class If(condition: Expression, body: Block) extends YulStatement {
    override def toString: String = {
        s"if ${condition.toString} ${brace(body.toString)}"
    }
}

case class Switch(expression: Expression, cases: Seq[Case]) extends YulStatement {
    override def toString: String = {
        s"switch ${expression.toString}" + "\n" +
            (if (cases.isEmpty) brace("") else cases.map(c => c.toString).mkString("\n")) +
            "\n"
    }
}

case class ForLoop(pre: Block, condition: Expression, post: Block, body: Block) extends YulStatement {
    override def toString: String = {
        s"for ${brace(pre.toString)} ${condition.toString} ${brace(post.toString)}" + "\n" +
            brace(body.toString)
    }
}

case class Break() extends YulStatement {
    override def toString: String = "break"
}

case class Continue() extends YulStatement {
    override def toString: String = "continue"
}

case class Leave() extends YulStatement {
    override def toString: String = "leave"
}

case class ExpressionStatement(expression: Expression) extends YulStatement {
    override def toString: String = {
        expression.toString
    }
}

case class Block(statements: Seq[YulStatement]) extends YulStatement {
    override def toString: String = {
        statements.map(s => s.toString).mkString("\n")
    }
}


/*
    Object = 'object' StringLiteral '{' Code ( Object | Data )* '}'
    Code = 'code' Block
    Data = 'data' StringLiteral ( HexLiteral | StringLiteral )
    HexLiteral = 'hex' ('"' ([0-9a-fA-F]{2})* '"' | '\'' ([0-9a-fA-F]{2})* '\'')
    StringLiteral = '"' ([^"\r\n\\] | '\\' .)* '"'
 */
case class Code(block: Block) extends YulAST

case class Data(name: String, hex: Seq[HexLiteral], str: Seq[StringLiteral]) extends YulAST

case class HexLiteral(content: String) extends YulAST

case class StringLiteral(content: String) extends YulAST

case class YulObject2(contractName : String, data: Seq[Data], mainContractTransactions: Seq[FunctionDefinition], mainContractSize: Int, otherTransactions: Seq[FunctionDefinition]) extends YulAST{
    def yulString(): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"), "example")
        val scope = new ObjScope(this)
        val raw: String = mustache.execute(new StringWriter(), scope).toString
        // mustache butchers certain characters; this fixes that. there may be other characters that
        // need to be added here
        raw.replaceAll("&amp;", "&").
            replaceAll("&gt;", ">").
            replaceAll("&#10;", "\n").
            replaceAll("&#61;", "=").
            replaceAll("&quot;", "\"").
            replaceAll("&#13;", "\r")
    }

    class ObjScope(obj: YulObject2) {
        // the values here are defined, as much as possible, in the same order as the mustache file
        val contractName: String = obj.contractName
        val deployedName : String = obj.contractName + "_deployed"

        val freeMemPointer = 64 // 0x40: currently allocated memory size (aka. free memory pointer)
        val firstFreeMem = 128 //  0x80: first byte in memory not reserved for special usages

        // the free memory pointer points to 0x80 initially
        var memoryInit: Expression = apply("mstore", intlit(freeMemPointer), intlit(firstFreeMem))

        def callValueCheck(): YulStatement = callvaluecheck

        val datasize: Expression = apply("datasize", rawstringlit(deployedName))

        def codeCopy(): Expression = apply("codecopy", intlit(0), apply("dataoffset", rawstringlit(deployedName)), datasize)

        def defaultReturn(): Expression = apply("return", intlit(0), datasize)

        val mainSize: Int = mainContractSize

        // together, these keep track of and create temporary variables for returns in the dispatch table
        var deRetCnt = 0

        def nextDeRet(): String = {
            deRetCnt = deRetCnt + 1
            s"_dd_ret_${deRetCnt.toString}" //todo: better naming convention?
        }

        /**
          * compute the sequence of statements for the dispatch table for a function
          *
          * @param f the function to be added to dispatch
          * @return the sequence to add to dispatch
          */
        def dispatchEntry(f: FunctionDefinition): Seq[YulStatement] = {
            // temporary variables to store the return from calling the function
            val temps: Seq[Identifier] = f.returnVariables.map(_ => Identifier(nextDeRet()))

            //todo: second argument to the application is highly speculative; check once you have more complex functions
            // the actual call to f, and a possible declaration of the temporary variables if there are any returns
            val call_to_f: Expression = apply(f.name, f.parameters.map(p => Identifier(p.name)): _*)
            val call_f_and_maybe_assign: YulStatement =
                if (f.returnVariables.nonEmpty) {
                    codegen.VariableDeclaration(temps.map(i => (i, None)), Some(call_to_f))
                } else {
                    ExpressionStatement(call_to_f)
                }

            val mp_id: Identifier = Identifier("memPos")

            Seq(
                //    if callvalue() { revert(0, 0) }
                callvaluecheck,
                // abi_decode_tuple_(4, calldatasize())
                codegen.ExpressionStatement(apply("abi_decode_tuple", intlit(4), apply("calldatasize"))),
                //    fun_retrieve_24()
                call_f_and_maybe_assign,
                //    let memPos := allocate_unbounded()
                decl_1exp(mp_id, apply("allocate_unbounded")),
                //    let memEnd := abi_encode_tuple__to__fromStack(memPos)
                //    let memEnd := abi_encode_tuple_t_uint256__to_t_uint256__fromStack(memPos , ret_0), etc.
                // nb: the code for these is written dynamically below so we can assume that they exist before they do
                decl_1exp(Identifier("memEnd"), apply("abi_encode_tuple_to_fromStack" + temps.length.toString, mp_id +: temps: _*)),
                //    return(memPos, sub(memEnd, memPos))
                codegen.ExpressionStatement(apply("return", mp_id, apply("sub", Identifier("memEnd"), mp_id)))
            )
        }

        val transactions: Seq[Func] = (mainContractTransactions ++ otherTransactions).map(t => new Func(t.toString))

        // the dispatch table gets one entry for each transaction in the main contract. the transactions
        // elaborations are added below, and those have a `this` argument added, which is supplied in the
        // dispatch table from an allocated instance of the main contract at the top of memory. but the
        // outside world does not see that, so the hashes in the dispatch array are the ones that
        // result from the transaction definitions WITHOUT the `this` argument.
        def dispatchTable(): codegen.Switch =
            codegen.Switch(Identifier("selector"),
                mainContractTransactions.map(t => codegen.Case(hexlit(hashOfFunctionDef(t)), Block(dispatchEntry(addThisArgument(t))))))

        def abiEncodeTupleFuncs(): YulStatement = Block((mainContractTransactions ++ otherTransactions)
                                                        .map(t => t.returnVariables.length)
                                                        .toSet
                                                        .map(write_abi_encode)
                                                        .toSeq)

        class Func(val code: String) {}

        class Case(val hash: String) {}

        class Call(val call: String) {}
    }
}

/**
  * @param name
  * @param code
  * @param runtimeSubobj
  * @param data
  * @param mainContractSize if the contract being represented is the main contract, this should be Some(the size it takes in memory), and None otherwise
  */
case class YulObject(name: String, code: Code, runtimeSubobj: Seq[YulObject], data: Seq[Data], mainContractSize: Option[Int]) extends YulAST {
    def yulString(): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"), "example")
        val scope = new ObjScope(this)
        val raw: String = mustache.execute(new StringWriter(), scope).toString
        // mustache butchers certain characters; this fixes that. there may be other characters that
        // need to be added here
        raw.replaceAll("&amp;", "&").
            replaceAll("&gt;", ">").
            replaceAll("&#10;", "\n").
            replaceAll("&#61;", "=").
            replaceAll("&quot;", "\"").
            replaceAll("&#13;", "\r")
    }

    // ObjScope and FuncScope are designed to facilitate mustache templates, with the following rules
    // - Each val is a tag in the template.
    // - If you need to fill in a section with non-empty list, define a function (which takes no args)
    //   named by the section tag, let it return an array of object, the object name does not matter,
    //   but the it must contain a val named by the tag inside the section tag (the best way to understand
    //   this is to track down an example)
    class ObjScope(obj: YulObject) {

        val mainContractName: String = obj.name
        val creationObject: String = mainContractName
        val runtimeObjectName: String = mainContractName + "_deployed"
        var runtimeFunctionArray: Array[Func] = Array[Func]()
        var deployFunctionArray: Array[Func] = Array[Func]()
        var dispatch = false
        var dispatchArray: Array[codegen.Case] = Array[codegen.Case]()
        var deployCall: Array[Call] = Array[Call]()

        val freeMemPointer = 64 // 0x40: currently allocated memory size (aka. free memory pointer)
        val firstFreeMem = 128 //  0x80: first byte in memory not reserved for special usages
        // the free memory pointer points to 0x80 initially

        var memoryInitRuntime: Expression = apply("mstore", intlit(freeMemPointer), intlit(firstFreeMem))

        var memoryInit: Expression = memoryInitRuntime

        def callValueCheck(): YulStatement = callvaluecheck

        // together, these keep track of and create temporary variables for returns in the dispatch table
        var deRetCnt = 0

        def nextDeRet(): String = {
            deRetCnt = deRetCnt + 1
            s"_dd_ret_${deRetCnt.toString}" //todo: better naming convention?
        }

        /**
          * compute the sequence of statements for the dispatch table for a function
          *
          * @param f the function to be added to dispatch
          * @return the sequence to add to dispatch
          */
        def dispatchEntry(f: FunctionDefinition): Seq[YulStatement] = {
            // temporary variables to store the return from calling the function
            val temps: Seq[Identifier] = f.returnVariables.map(_ => Identifier(nextDeRet()))

            //todo: second argument to the application is highly speculative; check once you have more complex functions
            // the actual call to f, and a possible declaration of the temporary variables if there are any returns
            val call_to_f: Expression = apply(f.name, f.parameters.map(p => Identifier(p.name)): _*)
            val call_f_and_maybe_assign: YulStatement =
                if (f.returnVariables.nonEmpty) {
                    codegen.VariableDeclaration(temps.map(i => (i, None)), Some(call_to_f))
                } else {
                    ExpressionStatement(call_to_f)
                }

            val mp_id: Identifier = Identifier("memPos")

            Seq(
                //    if callvalue() { revert(0, 0) }
                callvaluecheck,
                // abi_decode_tuple_(4, calldatasize())
                codegen.ExpressionStatement(apply("abi_decode_tuple", intlit(4), apply("calldatasize"))),
                //    fun_retrieve_24()
                call_f_and_maybe_assign,
                //    let memPos := allocate_unbounded()
                decl_1exp(mp_id, apply("allocate_unbounded")),
                //    let memEnd := abi_encode_tuple__to__fromStack(memPos)
                //    let memEnd := abi_encode_tuple_t_uint256__to_t_uint256__fromStack(memPos , ret_0), etc.
                // nb: the code for these is written dynamically below so we can assume that they exist before they do
                decl_1exp(Identifier("memEnd"), apply("abi_encode_tuple_to_fromStack" + temps.length.toString, mp_id +: temps: _*)),
                //    return(memPos, sub(memEnd, memPos))
                codegen.ExpressionStatement(apply("return", mp_id, apply("sub", Identifier("memEnd"), mp_id)))
            )
        }

        def deploy(): Array[Call] = deployCall

        def deployFunctions(): Array[Func] = deployFunctionArray

        def runtimeFunctions(): Array[Func] = runtimeFunctionArray

        var abiEncodesNeeded: Set[Int] = Set()

        val mainSize: Int = mainContractSize.getOrElse(0)

        // process the runtime object; todo this may only ever be a singleton
        for (sub <- obj.runtimeSubobj) {
            for (s <- sub.code.block.statements) {
                s match {
                    case f: FunctionDefinition =>
                        dispatch = true
                        runtimeFunctionArray = runtimeFunctionArray :+ new Func(f.toString)
                        if (mainContractSize.nonEmpty) {
                            // generate the dispatch table entry for things in main
                            dispatchArray = dispatchArray :+ codegen.Case(hexlit(hashOfFunctionDef(f)), Block(dispatchEntry(f)))
                        }
                        // note the number of return variables so that we generate the appropriate abi encode functions (without repetitions)
                        abiEncodesNeeded = abiEncodesNeeded + f.returnVariables.length
                    case x =>
                        assert(assertion = false, s"subobject case for ${x.toString} unimplemented")
                        ()
                }
            }
        }

        def dispatchCase(): codegen.Switch = codegen.Switch(Identifier("selector"), dispatchArray.toSeq)

        val datasize: Expression = apply("datasize", rawstringlit(runtimeObjectName))

        def codeCopy(): Expression = apply("codecopy", intlit(0), apply("dataoffset", rawstringlit(runtimeObjectName)), datasize)

        def defaultReturn(): Expression = apply("return", intlit(0), datasize)

        /**
          * compute the abi tuple encode function for a given number of returns. for example, for 0,
          * 1, and 2 returns:
          *
          * {{{
          * function abi_encode_tuple__to__fromStack(headStart ) -> tail {
          *    tail := add(headStart, 0)
          * }
          * }}}
          *
          * {{{
          * function abi_encode_tuple_t_uint256__to_t_uint256__fromStack(headStart , value0) -> tail {
          *    tail := add(headStart, 32)
          *    abi_encode_t_uint256_to_t_uint256_fromStack(value0,  add(headStart, 0))
          * }
          * }}}
          *
          * {{{
          * function abi_encode_tuple_t_uint256_t_uint256__to_t_uint256_t_uint256__fromStack(headStart , value0, value1) -> tail {
          *    tail := add(headStart, 64)
          *    abi_encode_t_uint256_to_t_uint256_fromStack(value0,  add(headStart, 0))
          *    abi_encode_t_uint256_to_t_uint256_fromStack(value1,  add(headStart, 32))
          * }
          * }}}
          *
          * @param n the number of returns
          * @return the function definition for output
          */
        def write_abi_encode(n: Int): FunctionDefinition = {
            val var_indices: Seq[Int] = Seq.tabulate(n)(i => i)
            val encode_lines: Seq[YulStatement] = var_indices.map(i =>
                ExpressionStatement(apply("abi_encode_t_uint256_to_t_uint256_fromStack",
                    Identifier("value" + i.toString),
                    apply("add", Identifier("headStart"), intlit((n - 1) * 32)))))

            val bod: Seq[YulStatement] = assign1(Identifier("tail"), apply("add", Identifier("headStart"), intlit(32 * n))) +: encode_lines
            FunctionDefinition("abi_encode_tuple_to_fromStack" + n.toString,
                TypedName("headStart", IntType()) +: var_indices.map(i => TypedName("value" + i.toString, IntType())),
                Seq(TypedName("tail", IntType())), Block(bod))
        }

        def abiEncodeTupleFuncs(): YulStatement = Block(abiEncodesNeeded.map(write_abi_encode).toSeq)

        class Func(val code: String) {}

        class Case(val hash: String) {}

        class Call(val call: String) {}
    }
}

package edu.cmu.cs.obsidian.codegen

import com.github.mustachejava.DefaultMustacheFactory
import edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.codegen.Util._
import edu.cmu.cs.obsidian.typecheck.ObsidianType

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
        s"case ${value.toString} ${brace(body.toString)}\n"
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
        if (cases.nonEmpty) {
            s"switch ${expression.toString}" + "\n" +
                cases.map(c => c.toString).mkString("\n") +
                "\n"
        } else {
            ""
        }
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

case class LineComment(s: String) extends YulStatement {
    override def toString: String = {
        s"// $s\n"
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

/** This class encapsulates how we write the Yul program resulting from the translation of an obsidian
  * program out to a string that can be written to a file and compiled to EVM.
  *
  * @param contractName             the name of the contract to write to a file
  * @param data                     the data of the contract -- CURRENTLY IGNORED
  * @param mainContractTransactions the transactions that came from the main contract in obsidian
  * @param mainContractSize         the number of bytes that the main contract needs when laid out in memory
  * @param otherTransactions        the transactions that came from each of the other contracts in obsidian
  */
case class YulObject(contractName: String,
                     data: Seq[Data],
                     mainContractTransactions: Seq[YulStatement],
                     mainContractSize: Int,
                     otherTransactions: Seq[YulStatement]) extends YulAST {
    def yulString(): String = {
        val mf = new DefaultMustacheFactory()
        val mustache = mf.compile(new FileReader("Obsidian_Runtime/src/main/yul_templates/object.mustache"), "example")
        val scope = new YulMustache(this)
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

    /** This class encapulsates the interaction between our representation of a Yul object and
      * the mustache description given in `object.mustache`, and from there how it's written to a file.
      *
      * @param obj the object to present to mustache
      */
    class YulMustache(obj: YulObject) {
        // the values here are defined, as much as possible, in the same order as the mustache file
        val contractName: String = obj.contractName
        val deployedName: String = obj.contractName + "_deployed"

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
            s"_dd_ret_${deRetCnt.toString}"
        }

        /**
          * compute the sequence of statements for the dispatch entry table for a function
          *
          * @param f the function to be added to dispatch
          * @return the sequence to add to dispatch
          */
        def dispatchEntry(f: FunctionDefinition): Seq[YulStatement] = {
            // temporary variables to store the return from calling the function
            val returns_from_call: Seq[Identifier] = f.returnVariables.map(_ => Identifier(nextDeRet()))

            // temporary variables to store the results of decoding the args to the function
            val params_from_decode: Seq[Identifier] = dropThisArgument(f).parameters.map(_ => Identifier(nextDeRet()))

            // f gets called with the special "this" parameter that's declared above, followed by
            // the temporary variables that store the decoded arguments
            val call_to_f: Expression = apply(f.name, Identifier("this") +: params_from_decode: _*)

            // if f returns something then we assign to that but otherwise we just call it for effect
            val call_f_and_maybe_assign: YulStatement =
                if (f.returnVariables.nonEmpty) {
                    codegen.VariableDeclaration(returns_from_call.map(i => (i, None)), Some(call_to_f))
                } else {
                    ExpressionStatement(call_to_f)
                }

            val maybe_assign_params: YulStatement =
                if (dropThisArgument(f).parameters.nonEmpty) {
                    decl_nexp(params_from_decode, apply(abi_decode_tuple_name(dropThisArgument(f)), intlit(4), apply("calldatasize")))
                } else {
                    LineComment(s"${f.name} takes no parameters")
                }

            val mp_id: Identifier = Identifier("memPos")

            Seq(
                LineComment(s"entry for ${f.name}"),
                //    if callvalue() { revert(0, 0) }
                callvaluecheck,
                // abi_decode_tuple_(4, calldatasize())
                maybe_assign_params,
                //    fun_retrieve_24()
                call_f_and_maybe_assign,
                //    let memPos := allocate_unbounded()
                decl_1exp(mp_id, apply("allocate_unbounded")),
                //    let memEnd := abi_encode_tuple__to__fromStack(memPos)
                //    let memEnd := abi_encode_tuple_t_uint256__to_t_uint256__fromStack(memPos , ret_0), etc.
                // nb: the code for these is written dynamically below so we can assume that they exist before they do
                decl_1exp(Identifier("memEnd"), apply(abi_encode_name(returns_from_call.length), mp_id +: returns_from_call: _*)),
                //    return(memPos, sub(memEnd, memPos))
                codegen.ExpressionStatement(apply("return", mp_id, apply("sub", Identifier("memEnd"), mp_id)))
            )
        }

        // TODO here and above in the dispatch table we do not respect privacy; we should iterate
        //  only over the things in the main contract that are public

        // todo: there's a fair amount of repetition here with t.asInstanceOf and dropThisArgument;
        //   tighten that up so it's easier to follow. at the same time think about how to only emit
        //   the decoders and encoders we actually need (i.e. if f only has `this` as a param, the decoder we
        //   emit never gets called and gets optimized away. that's fine enough but why not make it better?)

        // the dispatch table gets one entry for each transaction in the main contract. the transactions
        // elaborations are added below, and those have a `this` argument added, which is supplied in the
        // dispatch table from an allocated instance of the main contract at the top of memory. but the
        // outside world does not see that, so the hashes in the dispatch array are the ones that
        // result from the transaction definitions WITHOUT the `this` argument.
        def dispatchTable(): codegen.Switch =
            codegen.Switch(Identifier("selector"),
                mainContractTransactions.map(t => codegen.Case(hexlit(hashOfFunctionDef(dropThisArgument(t.asInstanceOf[FunctionDefinition]))),
                    Block(dispatchEntry(t.asInstanceOf[FunctionDefinition])))))

        // traverse the transactions and compute the abi functions we need to emit.
        def abiEncodeTupleFuncs(): YulStatement = Block(
            LineComment("abi encode tuple functions") +:
                (mainContractTransactions ++ otherTransactions)
                    .map(t => t.asInstanceOf[FunctionDefinition].returnVariables.length)
                    .distinct
                    .map(write_abi_encode_tuple_from_stack))

        def abiDecodeFuncs(): YulStatement = Block(
            LineComment("abi decode functions") +:
                (// for each transaction, emit the decoder for the tuple of its arguments
                    mainContractTransactions.map(t => write_abi_decode_tuple(dropThisArgument(t.asInstanceOf[FunctionDefinition]))).distinct
                        // for each transaction, collect up the types that it uses, and then emit the decodes for those types
                        ++ mainContractTransactions.flatMap(t => dropThisArgument(t.asInstanceOf[FunctionDefinition]).parameters.map(tn => tn.typ)).distinct.map(write_abi_decode))
        )

        def transactions(): YulStatement = Block(LineComment("translated transactions") +: (mainContractTransactions ++ otherTransactions))
    }
}

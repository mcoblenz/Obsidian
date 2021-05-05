package edu.cmu.cs.obsidian.codegen

import edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.typecheck.ObsidianType
import org.bouncycastle.jcajce.provider.digest.Keccak
import org.bouncycastle.util.encoders.Hex

/* utility functions shared between yulAST and CodeGenYul */
object Util {
    /**
      * wrap a string in balanced braces
      *
      * @param str the string to wrap
      * @return the string inside balanced braces
      */
    def brace(str: String): String = s"{$str}"

    /**
      * wrap a string in balanced parentheses
      *
      * @param str the string to wrap
      * @return the string inside balanced parentheses
      */
    def paren(str: String): String = s"($str)"

    /**
      * wrap a string in balanced quotes
      *
      * @param str the string to wrap
      * @return the string inside balanced quotes
      */
    def quote(str: String): String = "\"" + str + "\"" // escape characters are known to not work with string interpolation

    /**
      * shorthand for building Yul integer literals
      *
      * @param i the scala integer
      * @return the corresponding Yul integer literal
      */
    def intlit(i: Int): Literal = Literal(LiteralKind.number, i.toString, "int")

    /**
      * shorthand for building Yul boolean literals
      *
      * @param b the scala integer
      * @return the corresponding Yul boolean literal
      */
    def boollit(b: Boolean): Literal = Literal(LiteralKind.boolean, b.toString, "bool")

    /**
      * shorthand for building Yul hex literals
      *
      * @param s the scala hex
      * @return the corresponding Yul hex literal
      */
    def hexlit(s: String): Literal = Literal(LiteralKind.number, s, "int")

    /**
      * shorthand for building Yul string literals
      *
      * @param s the scala string
      * @return the corresponding Yul string literal
      */
    def stringlit(s: String): Literal = Literal(LiteralKind.string, s, "string")

    /**
      * shorthand for building Yul function applications
      *
      * @param n the name of the Yul function to apply
      * @param es the possibly empty sequence of arguments for the function
      * @return the expression that applies the function to the arguments
      */
    def apply(n: String, es: Expression*): Expression = FunctionCall(Identifier(n), es)

    /**
      * @return the yul call value check statement, which makes sure that funds are not spent inappropriately
      */
    def callvaluecheck: YulStatement = codegen.If(apply("callvalue"), Block(Seq(ExpressionStatement(apply("revert", intlit(0), intlit(0))))))

    /**
      * shorthand for bulding yul assignment statements, here assigning one expression to just one
      * identifier
      *
      * @param id the identifier to be assigned
      * @param e the expression to assign to it
      * @return the Yul assignment expression
      */
    def assign1(id: Identifier, e: Expression): Assignment = codegen.Assignment(Seq(id), e)

    /**
      * shorthand for building the yul expression that declares one variable without giving it a type
      * or an initial value
      *
      * @param id the name of the variable to be declared
      * @return the expression declaring the variable
      */
    def decl_0exp(id: Identifier): VariableDeclaration = VariableDeclaration(Seq((id, None)), None)

    /**
      * shorthand for building the yul expression that declares one variable with a type and no
      * initial value
      *
      * @param id the name of the variable to be declared
      * @return the expression declaring the variable
      */
    def decl_0exp_t(id: Identifier, t: ObsidianType): VariableDeclaration =
        VariableDeclaration(Seq((id, Some(mapObsTypeToABI(t.baseTypeName)))), None)
    /**
      * shorthand for building the yul expression that declares one variable with a type and no
      * initial value
      *
      * @param id the name of the variable to be declared
      * @return the expression declaring the variable
      */
    def decl_0exp_t_init(id: Identifier, t: ObsidianType, e: Expression): VariableDeclaration =
        VariableDeclaration(Seq((id, Some(mapObsTypeToABI(t.baseTypeName)))), Some(e))
    /**
      * shorthand for building the yul expression that declares a sequence (non-empty) of identifiers
      * with an initial value but no typing information
      *
      * @param id the identifiers to be declared, which cannot be the empty sequence
      * @param e the expression to assign the identifiers to as an initial value
      * @return the Yul expression for the declaration
      */
    def decl_nexp(id: Seq[Identifier], e: Expression): VariableDeclaration = {
        assert(id.nonEmpty, "internal error: tried to build a declaration with no identifiers")
        VariableDeclaration(id.map(i => (i, None)), Some(e))
    }

    /**
      * shorthand for building the yul expression that declares just identifier
      * with an initial value but no typing information
      *
      * @param id the identifier to be declared
      * @param e the expression to assign the identifier to as an initial value
      * @return the Yul expression for the declaration
      */
    def decl_1exp(id: Identifier, e: Expression): VariableDeclaration = decl_nexp(Seq(id), e)

    def mapObsTypeToABI(ntype: String): String = {
        // todo: this covers the primitive types from ObsidianType.scala but is hard to maintain because
        // it's basically hard coded, and doesn't traverse the structure of more complicated types.
        //
        // see https://docs.soliditylang.org/en/latest/abi-spec.html#types
        ntype match {
            case "bool" => "bool"
            case "int" => "u256"
            case "string" => "string"
            case "Int256" => "int256"
            case "unit" => assert(assertion = false, "unimplemented: unit type not encoded in Yul"); ""
            case _ => assert(assertion = false, "yul codegen encountered an obsidian type without a mapping to the ABI"); ""
        }
    }


    /**
      * returns the width of an obsidian type. right now this is always 1 because obsidian
      * does not currently implement tuples. when it does, updating this function will cause yul
      * codegen to correctly emit code that uses tuples in yul.
      *
      * @param t the obsidian type of interest
      * @return the width of the type
      */
    def obsTypeToWidth(t: ObsidianType): Int = { 1 }

    def functionRename(name: String): String = {
        name //todo some sort of alpha variation here combined with consulting a mapping; consult the ABI
    }

    def keccak256(s: String): String = {
        val digestK: Keccak.Digest256 = new Keccak.Digest256()
        s"0x${Hex.toHexString(digestK.digest(s.getBytes).slice(0, 4))}"
    }

    def hashOfFunctionDef(f: FunctionDefinition): String = {
        /* The first four bytes of the call data for a function call specifies the function to be
         called. It is the first (left, high-order in big-endian) four bytes of the Keccak-256 hash
         of the signature of the function. The signature is defined as the canonical expression of
         the basic prototype without data location specifier, i.e. the function name with the
         parenthesised list of parameter types. Parameter types are split by a single comma -
         no spaces are used. */
        // todo: the keccak256 implementation seems to agree with solc, but it's never been run on functions with arguments.
        keccak256(f.name + paren(f.parameters.map(p => mapObsTypeToABI(p.ntype)).mkString(",")))
    }
}

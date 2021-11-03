package edu.cmu.cs.obsidian.codegen


import edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.parser.ContractTable
import edu.cmu.cs.obsidian.typecheck._
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
    def stringlit(s: String): Literal = Literal(LiteralKind.string, quote(s), "string")

    /**
      * shorthand for inserting strings into the Yul directly, without quotations to make them a
      * string literal
      *
      * @param s the scala string
      * @return the corresponding Yul string literal
      */
    def rawstringlit(s: String): Literal = Literal(LiteralKind.string, s, "string")

    /**
      * shorthand for building Yul function applications
      *
      * @param n  the name of the Yul function to apply
      * @param es the possibly empty sequence of arguments for the function
      * @return the expression that applies the function to the arguments
      */
    def apply(n: String, es: Expression*): Expression = FunctionCall(Identifier(n), es)

    /**
      * helper function for a common subexpression that checks if a condition holds and calls revert if so
      *
      * @param id the expresion to check for being zero
      * @return the yul if-statement doing the check
      */
    def revertIf(cond: Expression): YulStatement =
        edu.cmu.cs.obsidian.codegen.If(cond, Block(Seq(ExpressionStatement(apply("revert", intlit(0), intlit(0))))))

    /**
      * @return the yul call value check statement, which makes sure that funds are not spent inappropriately
      */
    def callvaluecheck: YulStatement = {
        revertIf(apply("callvalue"))
    }

    /**
      * helper function for a common subexpression that checks if something is zero and calls revert forward if so
      *
      * @param id the expresion to check for being zero
      * @return the yul if-statement doing the check
      */
    def revertForwardIfZero(id: Expression): YulStatement =
        edu.cmu.cs.obsidian.codegen.If(apply("iszero", id), Block(Seq(ExpressionStatement(apply("revert_forward_1")))))

    /**
      * shorthand for bulding yul assignment statements, here assigning one expression to just one
      * identifier
      *
      * @param id the identifier to be assigned
      * @param e  the expression to assign to it
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
      * @param t  the type for id
      * @return the expression declaring the variable
      */
    def decl_0exp_t(id: Identifier, t: ObsidianType): VariableDeclaration =
        VariableDeclaration(Seq((id, Some(obsTypeToYulTypeAndSize(t.baseTypeName)._1))), None)

    /**
      * shorthand for building the yul expression that declares one variable with a type and no
      * initial value
      *
      * @param id the name of the variable to be declared
      * @param t  the type for id
      * @param e  the expression of type t to which id will be bound
      * @return the expression declaring the variable
      */
    def decl_0exp_t_init(id: Identifier, t: ObsidianType, e: Expression): VariableDeclaration =
        VariableDeclaration(Seq((id, Some(obsTypeToYulTypeAndSize(t.baseTypeName)._1))), Some(e))

    /**
      * shorthand for building the yul expression that declares a sequence (non-empty) of identifiers
      * with an initial value but no typing information
      *
      * @param id the identifiers to be declared, which cannot be the empty sequence
      * @param e  the expression to assign the identifiers to as an initial value
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
      * @param e  the expression to assign the identifier to as an initial value
      * @return the Yul expression for the declaration
      */
    def decl_1exp(id: Identifier, e: Expression): VariableDeclaration = decl_nexp(Seq(id), e)

    /**
      * given the string name of an obsidian type, provide the name of the Yul type that it maps to
      * paired with the number of bytes needed to store a value of that type in Yul.
      *
      * @param ntype the name of the obsidian type
      * @return a pair of the name of the corresponding Yul type and the size of its values
      */
    def obsTypeToYulTypeAndSize(ntype: String): (String, Int) = {
        // todo: this covers the primitive types from ObsidianType.scala but is hard to maintain because
        // it's basically hard coded, and doesn't traverse the structure of more complicated types.
        //
        // see https://docs.soliditylang.org/en/latest/abi-spec.html#types
        ntype match {
            case "bool" => ("bool", 1)
            case "int" => ("u256", 32)
            case "string" => ("string", -1) // todo this -1 is a place holder
            case "Int256" => ("int256", 32)
            case "unit" => assert(assertion = false, "unimplemented: unit type not encoded in Yul"); ("", -1)
            // fall through here and return the type unmodified; it'll be a structure that is defined by the file in question
            case ntype => (ntype, -1) // todo need to compute the size of richer types, too.
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
    def obsTypeToWidth(t: ObsidianType): Int = {
        // this only gets called in one place, which is the translation of local invocations.
        // it should either be 1 or 0, indicating if the return type is void (0) or not (1)
        t match {
            case primitiveType: PrimitiveType => primitiveType match {
                case IntType() => 1
                case BoolType() => 1
                case StringType() => 1
                case Int256Type() => 1
                case UnitType() => 0
            }
            case primitiveType: NonPrimitiveType => assert(false, "width not implemented for nonprimitive types!"); -1
            case BottomType() => assert(false, "width not implemented for the bottom type!"); -1
        }
    }

    /**
      * return the top 4 bytes of the keccak256 hash of a string
      *
      * @param s the string to hash
      * @return the top 4 bytes of the keccak256 of the input string
      */
    def keccak256(s: String): String = {
        val digestK: Keccak.Digest256 = new Keccak.Digest256()
        s"0x${Hex.toHexString(digestK.digest(s.getBytes).slice(0, 4))}"
    }

    /**
      * given the name of a function and the sequence of its argument types, computes the function
      * selector hash
      *
      * @param name  the name of the function
      * @param types the sequence of types
      * @return from the spec: "The first four bytes of the call data for a function call specifies the function to be
      *         called. It is the first (left, high-order in big-endian) four bytes of the Keccak-256 hash
      *         of the signature of the function. The signature is defined as the canonical expression of
      *         the basic prototype without data location specifier, i.e. the function name with the
      *         parenthesised list of parameter types. Parameter types are split by a single comma -
      *         no spaces are used"
      */
    def hashOfFunctionName(name: String, types: Seq[String]): String = {
        // todo: the keccak256 implementation seems to agree with solc, but it's never been run on functions with arguments.
        keccak256(name + paren(types.mkString(",")))
    }

    /**
      * given a full function definition, produce its function selector hash.
      *
      * @param f the function definition
      * @return its selector hash
      */
    def hashOfFunctionDef(f: FunctionDefinition): String = {
        hashOfFunctionName(f.name, f.parameters.map(p => obsTypeToYulTypeAndSize(p.ntype)._1))
    }

    /**
      * traverse an obsidian type to compute the number of bytes needed to store it in memory.
      *
      * @param t the obsidian type of interest
      * @return the number of bytes of memory to allocate to store a value of t
      */
    def sizeOfObsType(t: ObsidianType): Int = {
        val pointer_size = 32

        t match {
            case primitiveType: PrimitiveType => primitiveType match {
                case IntType() => 32
                case BoolType() => 0
                case StringType() => assert(false, "size of string constants is unimplemented"); -1
                case Int256Type() => 256
                case UnitType() => 0
            }
            case nonPrimitiveType: NonPrimitiveType => nonPrimitiveType match {
                case ContractReferenceType(contractType, permission, remoteReferenceType) => pointer_size
                case StateType(contractType, stateNames, remoteReferenceType) =>
                    // one day, this should probably be ceil(log_2 (length of stateNames()))) bits
                    assert(false, "size of states is unimplemented"); -1
                case InterfaceContractType(name, simpleType) => pointer_size
                case GenericType(gVar, bound) =>
                    // todo: this may need to change; think about it more later
                    pointer_size
            }
            case BottomType() => 0
        }
    }

    /**
      * given a contract type, compute the number of bytes needed to store it in memory in the yul
      * object.
      *
      * @param ct the contract of interest
      * @return the number of bytes needed to store the fields of the contract
      */
    def sizeOfContract(ct: ContractTable): Int = {
        ct.allFields.toList.map(f => sizeOfObsType(f.typ)).sum
    }

    /** given a contract table and a field name, compute the number of bytes offset from the
      * beginning of that contract's area in memory to find the field.
      *
      * @param ct the contract table
      * @param name the field to look for
      * @return number of bytes offset
      */
    def offsetOfField(ct: ContractTable, name: String): Int = {
        ct.allFields.toList
            .takeWhile(f => f.name != name) // drop the suffix including and after the target
            .map(f => sizeOfObsType(f.typ)) // compute the sizes of everything before the target
            .sum // add up those sizes to get the offset
    }

    /**
      * given an expression, produce the name of the contract that it associates with.
      * WARNING: this is a stub! actually implementing this means reworking the translation to yul
      * to carry around type information for the expression being elaborated.
      *
      * @param e the expression of interest
      * @return the name of the contract for the expression, if there is one; raises an error otherwise
      */
    def getContractName(e: edu.cmu.cs.obsidian.parser.Expression): String = { //todo should this return an option? what's the invariant exactly?
        e.obstype match {
            case Some(value) => value match {
                case _: PrimitiveType => assert(false, s"primitive types do not have contract names"); ""
                case tau: NonPrimitiveType => tau match {
                    case ContractReferenceType(contractType, _, _) => contractType.contractName
                    case StateType(contractType, _, _) => contractType.contractName
                    case InterfaceContractType(_, _) => assert(false, "unimplemented"); ""
                    case GenericType(_, _) => assert(false, "unimplemented"); ""
                }
                case BottomType() => assert(false, s"the bottom type does not have a contract name"); ""
            }
            case None => assert(false, s"expression without a type annotation: ${e.toString}"); ""
        }
    }

    /**
      * given a contract name and a transaction name, produce the name of the contract in the flat
      * yul object that corresponds to it
      *
      * @param contractName    the name of the contract
      * @param transactionName the name of the transaction
      * @return the name of the Yul transaction for that contract
      */
    def transactionNameMapping(contractName: String, transactionName: String): String =
        s"${contractName}___${transactionName}"

    /**
      * if a and b do not contain "___", then
      * transactionNameUnmapping(transactionNameMapping(a,b)) == Some(a,b)
      *
      *
      * @param s
      * @return
      */
    def transactionNameUnmapping(s: String): Option[(String, String)] = {
        val halves: Array[String] = s.split("___")
        if (halves.length != 2) {
            None
        } else {
            Some(halves(0), halves(1))
        }
    }
}

package edu.cmu.cs.obsidian.codegen


import edu.cmu.cs.obsidian.codegen
import edu.cmu.cs.obsidian.parser.{ContractTable, Field, SymbolTable}
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
      * @param cond the expression to check for being zero
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
      * @param id the expression to check for being zero
      * @return the yul if-statement doing the check
      */
    def revertForwardIfZero(id: Expression): YulStatement =
        edu.cmu.cs.obsidian.codegen.If(apply("iszero", id), Block(Seq(ExpressionStatement(apply("revert_forward_1")))))

    /**
      * shorthand for building yul assignment statements, here assigning one expression to just one
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
        VariableDeclaration(Seq((id, Some(baseTypeToYulName(t)))), None)

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
        VariableDeclaration(Seq((id, Some(baseTypeToYulName(t)))), Some(e))

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

    /** if a given obsidian type is a base type that matches directly to a Yul base type,
      * produce the string that names the Yul type. assert otherwise.
      *
      * @param typ the obsidian type in question
      * @return the matching yul type name, if the argument is indeed a base type.
      */
    def baseTypeToYulName(typ: ObsidianType): String = {
        typ match {
            case primitiveType: PrimitiveType => primitiveType match {
                case IntType() => "u256"
                case BoolType() => "bool"
                case StringType() => "string"
                case Int256Type() => "int256"
                case UnitType() => assert(assertion = false, "unimplemented: unit type not encoded in Yul"); ""
            }
            case t: NonPrimitiveType => t.contractName
            case BottomType() => assert(assertion = false, "unimplemented: bottom type not encoded in Yul"); ""
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
            case _: NonPrimitiveType => assert(assertion = false, "width not implemented for non-primitive types!"); -1
            case BottomType() => assert(assertion = false, "width not implemented for the bottom type!"); -1
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
        hashOfFunctionName(f.name, f.parameters.map(p => baseTypeToYulName(p.typ)))
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
                case StringType() => assert(assertion = false, "size of string constants is unimplemented"); -1
                case Int256Type() => 256
                case UnitType() => 0
            }
            case nonPrimitiveType: NonPrimitiveType => nonPrimitiveType match {
                case ContractReferenceType(_, _, _) => pointer_size
                case StateType(_, _, _) =>
                    // todo: one day, this should probably be ceil(log_2 (length of stateNames()))) bits
                    assert(assertion = false, "size of states is unimplemented"); -1
                case InterfaceContractType(_, _) => pointer_size
                case GenericType(_, _) =>
                    // todo: this may need to change; think about it more later
                    pointer_size
            }
            case BottomType() => 0
        }
    }

    /** given a contract table, produce a sequence of its fields. note that there are simpler ways
      * to do this, e.g. ct.allFields, but they produce unordered collections. this is not suitable
      * for layout in memory, where we want the order to be predictable.
      *
      * @param ct the contract table of interest
      * @return the fields present in the contract table.
      */
    def fieldsOfContract(ct: ContractTable): Seq[Field] = {
        ct.contract.declarations.filter(decl => decl match {
            case Field(_, _, _, _) => true
            case _ => false
        }).map(decl => decl.asInstanceOf[Field])
    }

    /**
      * given a contract table, compute the number of bytes needed to store it in memory in the yul
      * object.
      *
      * @param ct the contract of interest
      * @return the number of bytes needed to store the fields of the contract
      */
    def sizeOfContract(ct: ContractTable): Int = {
        fieldsOfContract(ct).map(f => sizeOfObsType(f.typ)).sum
    }

    /** given the name of a contract and a symbol table, either produce the size that contract uses
      * when laid out in memory or assert if it's not present in the symbol table.
      *
      * @param contractName the contract to find the size of
      * @param st           the symbol table to look in
      * @return the size of the contract
      */
    def sizeOfContractST(contractName: String, st: SymbolTable): Int = {
        st.contract(contractName) match {
            case Some(ct) => Util.sizeOfContract(ct)
            case None => assert(assertion = false, "symbol table missing a contract"); -1
        }
    }

    /** given a contract table and a field name, compute the number of bytes offset from the
      * beginning of that contract's area in memory to find the field.
      *
      * @param ct   the contract table
      * @param name the field to look for
      * @return number of bytes offset
      */
    def offsetOfField(ct: ContractTable, name: String): Int = {
        fieldsOfContract(ct)
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
    def getContractName(e: edu.cmu.cs.obsidian.parser.Expression): String = {
        e.obstype match {
            case Some(value) => value match {
                case _: PrimitiveType => assert(assertion = false, s"primitive types do not have contract names"); ""
                case tau: NonPrimitiveType => tau match {
                    case ContractReferenceType(contractType, _, _) => contractType.contractName
                    case StateType(contractType, _, _) => contractType.contractName
                    case InterfaceContractType(_, _) => assert(assertion = false, "unimplemented"); ""
                    case GenericType(_, _) => assert(assertion = false, "unimplemented"); ""
                }
                case BottomType() => assert(assertion = false, s"the bottom type does not have a contract name"); ""
            }
            case None => assert(assertion = false, s"expression without a type annotation: ${e.toString}"); ""
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
        s"${contractName}___$transactionName"

    /**
      * if a and b do not contain three underscores in a row, then
      * transactionNameUnmapping(transactionNameMapping(a,b)) == Some(a,b)
      *
      * @param s the string to break
      * @return the two halves of the string
      */
    def transactionNameUnmapping(s: String): Option[(String, String)] = {
        val halves: Array[String] = s.split("___")
        if (halves.length != 2) {
            None
        } else {
            Some(halves(0), halves(1))
        }
    }

    /** given a contract table and a name of a field, produce the expression that computes
      * the address of that field offset into the contract
      *
      * @param ct the contract table
      * @param x  the field name
      * @return the expression computing the offset
      */
    def fieldFromThis(ct: ContractTable, x: String): Expression = {
        apply("add", Identifier("this"), intlit(Util.offsetOfField(ct, x)))
    }

    /** given a function definition, return a function definition that is the same but with an extra
      * first argument named `this`.
      *
      * @param f the function definition to transform
      * @return the transformed definition with an added first argument
      */
    def addThisArgument(f: FunctionDefinition): FunctionDefinition = {
        // todo: string type is a temporary hack here
        FunctionDefinition(f.name, Seq(TypedName("this", StringType())) ++ f.parameters, f.returnVariables, f.body)
    }

    /** given a function definition where the first argument is a named `this`, return the definition
      * without that argument but that is otherwise the same
      *
      * @param f the function definition to transform
      * @return the transformed definition without the first argument
      * @throws RuntimeException if the first argument of the function definition is not `this`
      */
    def dropThisArgument(f: FunctionDefinition): FunctionDefinition = {
        f.parameters match {
            case TypedName("this", StringType()) :: tl => FunctionDefinition(f.name, tl, f.returnVariables, f.body)
            case _ :: _ => throw new RuntimeException("dropping `this` argument from a sequence of args that doesn't start with `this`")
            case _ => throw new RuntimeException("dropping argument from empty list")
        }
    }

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
}

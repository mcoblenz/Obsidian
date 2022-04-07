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
    def boollit(b: Boolean): Literal = {
        b match {
            case true => intlit(1)
            case false => intlit(0)
        }
    }

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
        edu.cmu.cs.obsidian.codegen.If(cond, Block(Do(apply("revert", intlit(0), intlit(0)))))

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
        edu.cmu.cs.obsidian.codegen.If(apply("iszero", id), Block(Do(apply("revert_forward_1"))))

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
    def decl_0exp_t(id: Identifier, t: YulABIType): VariableDeclaration =
        VariableDeclaration(Seq((id, Some(t.toString))), None)

    /**
      * shorthand for building the yul expression that declares one variable with a type and no
      * initial value
      *
      * @param id the name of the variable to be declared
      * @param t  the type for id
      * @param e  the expression of type t to which id will be bound
      * @return the expression declaring the variable
      */
    def decl_0exp_t_init(id: Identifier, t: YulABIType, e: Expression): VariableDeclaration =
        VariableDeclaration(Seq((id, Some(t.toString))), Some(e))

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

    /** given an obsidian type, comptue the yul ABI type that it corresponds to
      *
      * @param typ the obsidian type in question
      * @return the Yul type that represents it
      */
    def obsTypeToYulType(typ: ObsidianType): YulABIType = {
        typ match {
            case primitiveType: PrimitiveType => primitiveType match {
                case IntType() => YATUInt32()
                case BoolType() => YATBool()
                case StringType() => YATString()
                case Int256Type() => YATUInt32()
                case UnitType() => throw new RuntimeException("unimplemented: unit type not encoded in Yul")
            }
            case t: NonPrimitiveType => YATContractName(t.contractName)
            case BottomType() => throw new RuntimeException("unimplemented: bottom type not encoded in Yul")
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
            case _: NonPrimitiveType => 1
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
        keccak256(name + paren(types.mkString(",")))
    }

    /**
      * given a full function definition, produce its function selector hash.
      *
      * @param f the function definition
      * @return its selector hash
      */
    def hashOfFunctionDef(f: FunctionDefinition): String = {
        hashOfFunctionName(f.name, f.parameters.map(p => p.typ.toString))
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

    /** given a yul type, compute how much space it takes up in memory, in bytes
      *
      * @param t the yul type
      * @return the amount of space it uses in bytes
      */
    def sizeOfYulType(t: YulABIType): Int = {
        t match {
            case YATAddress() => 32
            case YATUInt32() => 32
            case YATBool() => 32
            case YATContractName(_) => throw new RuntimeException("size of defined contracts not supported") //todo this might be 32 if it's just specific address or might need to call the size of OBStype method, which would add parameters here to do a look up
            case YATString() => throw new RuntimeException("size of strings not supported")
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

    /** given a contract table and a name of a field, produce the expression that computes
      * the address of that field offset into the contract
      *
      * @param ct the contract table
      * @param x  the field name
      * @return the expression computing the offset
      */
    def fieldFromThis(ct: ContractTable, x: String): Expression = {
        // todo: is hardcoding "this" a good idea? am i ever going to call it anything else?
        apply("add", Identifier("this"), intlit(Util.offsetOfField(ct, x)))
    }

    /** return the expression that sets a field from a contract either in memory or storage as appropriate
      *
      * @param ct        the information about the contract
      * @param fieldName the field name to set
      * @param value     the value to set it to
      * @return the statement that does the check and then sets
      */
    def updateField(ct: ContractTable, fieldName: String, value: Expression): YulStatement = {
        val address_of_field: Expression = fieldFromThis(ct, fieldName)
        ifInStorge(addr_to_check = address_of_field,
            true_case = Do(apply("sstore", address_of_field, value)), // todo double check why there isn't a mapToStorageAddr call here; i think it's because you only end up in this branch when `this` is already big enough
            false_case = Do(apply("mstore", address_of_field, value))
        )
    }


    /** given a field name and address, produce code that checks for finding it in memory or storage as appropriate
      *
      * @param ct          the context of the program
      * @param fieldName   the name of the filed
      * @param destination the place to store the contents of memory or storage (it must be initialized separately in the output yul)
      * @return the switch statement that checks and assigns as appropriate
      */
    def fetchField(ct: ContractTable, fieldName: String, destination: Identifier): YulStatement = {
        // todo (tidy) there's a lot of repeated code with the above; abstract it out
        val address_of_field = fieldFromThis(ct, fieldName)
        ifInStorge(addr_to_check = address_of_field,
            true_case = Seq(assign1(destination, apply("sload", address_of_field))),
            false_case = Seq(assign1(destination, apply("mload", address_of_field)))
        )
    }

    /** given a function definition, return a function definition that is the same but with an extra
      * first argument named `this`.
      *
      * @param f the function definition to transform
      * @return the transformed definition with an added first argument
      */
    def addThisArgument(f: FunctionDefinition): FunctionDefinition = {
        FunctionDefinition(f.name, Seq(TypedName("this", YATAddress())) ++ f.parameters, f.returnVariables, f.body, f.inDispatch)
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
            case TypedName("this", YATAddress()) :: tl => FunctionDefinition(f.name, tl, f.returnVariables, f.body, f.inDispatch)
            case _ :: _ => throw new RuntimeException("dropping `this` argument from a sequence of args that doesn't start with `this`")
            case _ => throw new RuntimeException("dropping argument from empty list")
        }
    }


    /** given an integer, return the name of the abi tuple encoder for that integer
      *
      * @param n the size of the tuples encoded
      * @return the name of the function used in the output
      */
    def abi_encode_name(n: Int): String = {
        s"abi_encode_tuple_to_fromStack${n.toString}"
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
    def write_abi_encode_tuple_from_stack(n: Int): FunctionDefinition = {
        val var_indices: Seq[Int] = Seq.tabulate(n)(i => i)
        val encode_lines: Seq[YulStatement] = var_indices.map(i =>
            ExpressionStatement(apply("abi_encode_t_uint256_to_t_uint256_fromStack",
                Identifier("value" + i.toString),
                apply("add", Identifier("headStart"), intlit((n - 1) * 32)))))

        val bod: Seq[YulStatement] = assign1(Identifier("tail"), apply("add", Identifier("headStart"), intlit(32 * n))) +: encode_lines
        FunctionDefinition(abi_encode_name(n),
            TypedName("headStart", YATUInt32()) +: var_indices.map(i => TypedName("value" + i.toString, YATUInt32())),
            Seq(TypedName("tail", YATUInt32())), Block(bod), inDispatch = false)
    }

    /** given an function definition, return the name of the abi tuple decode for that functions
      * arugments
      *
      * @param f the function whose arguments are to be decoded
      * @return the name of the function that will be emitted to decode the arguments
      */
    def abi_decode_tuple_name(f: FunctionDefinition): String = {
        s"abi_decode_tuple_${f.parameters.map(tn => tn.typ.toString).mkString}"
    }

    /** Given an obsidian type, produce the name of the function that will be emitted to decode
      * values of the corresponding yul type as parameters
      *
      * @param t the obsidian type to decode
      * @return the name of the function that does the decoding
      */
    def abi_decode_name(t: YulABIType): String = {
        s"abi_decode_${t.toString}"
    }

    /** Given an obsidian type, produce the function to emit to decode
      * values of the corresponding yul type as parameters
      *
      * @param t the obsidian type to decode
      * @return the yul function that does the decoding
      */
    def write_abi_decode(t: YulABIType): FunctionDefinition = {
        val offset = TypedName("offset", YATUInt32())
        val end = TypedName("end", YATUInt32())
        val ret = TypedName("ret", t)

        val bod = t match {
            case YATAddress() => throw new RuntimeException(s"abi decoding not implemented for ${t.toString}")
            case YATUInt32() =>
                Seq(
                    //value := calldataload(offset)
                    assign1(Identifier(ret.name), apply("calldataload", Identifier(offset.name)))
                )
            case YATBool() => throw new RuntimeException(s"abi decoding not implemented for ${t.toString}")
            case YATString() => throw new RuntimeException(s"abi decoding not implemented for ${t.toString}")
            case YATContractName(_) => throw new RuntimeException(s"abi decoding not implemented for ${t.toString}")
        }

        FunctionDefinition(name = abi_decode_name(t),
            parameters = Seq(offset, end),
            returnVariables = Seq(ret),
            body = Block(bod),
            inDispatch = false)
    }

    /** Given a function definition, provide a function that decodes parameters of the argument type
      * from the ABI.
      *
      * @param f the function to produce a parameter decoder for
      * @return the decoder for the parameters of the argument function
      */
    def write_abi_decode_tuple(f: FunctionDefinition): FunctionDefinition = {
        val retVars: Seq[TypedName] = f.parameters.zipWithIndex.map { case (tn, i) => TypedName(s"ret${i.toString}", tn.typ) }

        val start = TypedName("start", YATUInt32())
        val end = TypedName("end", YATUInt32())

        val offsets: Seq[Int] = f.parameters.scanLeft(0)({ (acc, tn) => acc + sizeOfYulType(tn.typ) })

        val bod: Seq[YulStatement] =
        // if slt(sub(end, start), SUM_OF_SIZES) { revert(0,0) }
            revertIf(apply("slt", apply("sub", Identifier(end.name), Identifier(start.name)), intlit(offsets.last))) +:
                // for each argument, `value0 := abi_decode_TYPE(add(headStart, offset), dataEnd)`
                f.parameters.zipWithIndex.zip(offsets).map {
                    case ((tn, i), off) =>
                        assign1(Identifier(s"ret${i.toString}"), apply(abi_decode_name(tn.typ), apply("add", Identifier(start.name), intlit(off)), Identifier(end.name)))
                }

        FunctionDefinition(name = abi_decode_tuple_name(f),
            parameters = Seq(start, end),
            returnVariables = retVars,
            body = Block(bod),
            inDispatch = false)
    }

    // storage is 2**256 big; this is (2**256)/2, which is the same as 1 << 255 or shl(255,1) in yul
    val storage_threshold: Expression = apply("shl", intlit(255), intlit(1))

    // storage addresses start at 0x8...0, but the first one that will actually come out the allocator is 128 above that.
    val first_storage_address = apply("add", storage_threshold, intlit(128))

    /** given an expression representing a memory address its corresponding place in storage
      *
      * @param x the expression representing the memory address
      * @return an expression that computes to the corresponding address in storage
      */
    def mapToStorageAddress(x: Expression): Expression = {
        apply("add", x, storage_threshold)
    }

    /** the returned expression evaluates to true iff the argument is above or equal to the storage threshold
      *
      * @param addr
      * @return
      */
    def compareToThresholdExp(addr: Expression): Expression = {
        apply("gt", addr, apply("sub", storage_threshold, intlit(1)))
    }

    /** convenience function for taking an expression and building a singleton statement sequence
      *
      * @param x
      * @return
      */
    def Do(x: Expression): Seq[YulStatement] = {
        Seq(ExpressionStatement(x))
    }

    /** given an expression that represents an address, compute the yul statement that checks if it's
      * a storage address or not and execute a sequence of statements in either case. if it does not
      * represent an address, the behaviour is undefined.
      *
      * @param addr_to_check the expression to check for being in storage
      * @param true_case     what to do if the address is a storage address
      * @param false_case    what to to if the address is not a storage address
      * @return the expression performing the check
      */
    def ifInStorge(addr_to_check: Expression, true_case: Seq[YulStatement], false_case: Seq[YulStatement]): YulStatement = {
        // note: gt(x,y) is x > y; to get x >= y, subtract 1 since they're integers
        edu.cmu.cs.obsidian.codegen.Switch(compareToThresholdExp(addr_to_check),
            Seq(Case(boollit(true), Block(true_case)),
                Case(boollit(false), Block(false_case))))
    }


    /** given info about a transaction, provide its name in the flattened representation
      *
      * @param contractName    the name of the contract that the transaction originates from
      * @param transactionName the name of the transaction itself
      * @param types           if the transaction is a constructor, the sequences of names of types that it takes; none otherwise.
      * @return
      */
    def flattenedName(contractName: String, transactionName: String, types: Option[Seq[String]]): String = {
        val suffix = types match {
            case Some(value) => hashOfFunctionName(contractName, value)
            case None => ""
        }
        s"${contractName}___$transactionName" + suffix
    }

    /** given the name of a contract, return the name of its copying tracer function
      *
      * @param name the name of the contract being traced
      * @return the name of the tracer
      */
    def nameTracer(name: String): String = {
        s"trace_$name"
    }

    /** given the name of a contract, return the name of its wiping function
      *
      * @param name the name of the contract being traced
      * @return the name of the tracer
      */
    def nameWiper(name: String): String = {
        s"wipe_$name"
    }

    /** given a Yul type, provide an appropriate default value for it.
      *
      * @param typ the type to provide a default for
      * @return the default value, if there is one
      */
    def defaultInitValue(typ: YulABIType): Literal = {
        typ match {
            case YATAddress() => hexlit("0x0")
            case YATUInt32() => intlit(5738) //todo
            case YATBool() => boollit(false)
            case YATString() => stringlit("")
            case YATContractName(name) => throw new RuntimeException("can't query for a default contract value")
        }
    }

    /** a constant for the name of the added field for counting references to each object
      *
      * @return
      */
    def refCountName: String = "__refcount"

    def retainName: String = "retain"

    def releaseName: String = "release"

    def deallocName: String = "dealloc"
}

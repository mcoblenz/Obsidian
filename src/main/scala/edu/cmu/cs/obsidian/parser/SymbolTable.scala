package edu.cmu.cs.obsidian.parser

import scala.collection.{Map, Seq}
import scala.collection.immutable.TreeMap
import edu.cmu.cs.obsidian.typecheck._

sealed trait DeclarationTable {
    def name: String

    /* merely returns "this" if this is a [ContractTable] already,
     * or gets the [ContractTable] of a [StateTable] */
    def contractTable: ContractTable
    def contract: Contract

    def ast: AST

    /* looks for a contract called [name] that's in scope
     * (either globally or in this particular contract) */
    def lookupContract(name: String): Option[ContractTable]
    def lookupField(name: String): Option[Field]
    def lookupTransaction(name: String): Option[Transaction]
    def lookupFunction(name: String): Option[Func]

    def lookupFieldRaw(name: String): Option[Field]
    def lookupTransactionRaw(name: String): Option[Transaction]
    def lookupFunctionRaw(name: String): Option[Func]
    def simpleType: SimpleType

    def indexDecl[T, TCast](decls: Seq[Declaration], tag: DeclarationTag): Map[String, TCast] = {
        var lookup = new TreeMap[String, TCast]()

        for (decl <- decls if decl.tag == tag) {
            lookup = lookup.updated(decl.name, decl.asInstanceOf[TCast])
        }

        lookup
    }
}

class StateTable(
        astNodeRaw: State,
        lexicallyInsideOf: ContractTable) extends DeclarationTable {

    assert(astNodeRaw != null)

    private var astNode: State = astNodeRaw
    private var fieldLookup: Map[String, Field] = indexDecl[ObsidianType, Field](ast.declarations, FieldDeclTag)
    private var txLookup: Map[String, Transaction] = indexDecl[ObsidianType, Transaction](ast.declarations, TransactionDeclTag)
    private var funLookup: Map[String, Func] = indexDecl[ObsidianType, Func](ast.declarations, FuncDeclTag)

    def contractTable: ContractTable = lexicallyInsideOf
    def contract: Contract = lexicallyInsideOf.contract

    val fieldLookupRaw: Map[String, Field] = {
        indexDecl[ParsableType, Field](astNodeRaw.declarations, FieldDeclTag)
    }

    val txLookupRaw: Map[String, Transaction] = {
        indexDecl[ParsableType, Transaction](astNodeRaw.declarations, TransactionDeclTag)
    }

    val funLookupRaw: Map[String, Func] = {
        indexDecl[ParsableType, Func](astNodeRaw.declarations, FuncDeclTag)
    }

    def name: String = astNodeRaw.name

    def simpleType = StateType(contract.name, astNodeRaw.name)

    def ast: State = astNode

    def lookupContract(name: String): Option[ContractTable] = contractTable.lookupContract(name)

    // Implements two-stage lookup for two nested scopes (state and then contract).
    private def doLookup[FoundType <: IsAvailableInStates](toFind: String,
                                    lookupFunction1: (String => Option[FoundType]),
                                    lookupFunction2: (String => Option[FoundType])): Option[FoundType] = {
        lookupFunction1(toFind) match {
            case x@Some(_) => x
            case None =>
                val found = lookupFunction2(toFind)
                if (found.isDefined) {
                    val availableIn = found.get.availableIn
                    if (availableIn.isDefined) {
                        val availableInCurrentState = availableIn.get.exists(p => p._1 == name)
                        if (availableInCurrentState) found else None
                    }
                    else found // The field is available in all states.
                }
                else None // There's no field by this name.
        }
    }

    def lookupField(fieldName: String): Option[Field] = {
        doLookup[Field](fieldName, fieldLookup.get, lexicallyInsideOf.lookupField)
    }

    def lookupTransaction(transactionName: String): Option[Transaction] = {
        doLookup[Transaction](transactionName, txLookup.get, lexicallyInsideOf.lookupTransaction)
    }

    def lookupFunction(functionName: String): Option[Func] = {
        doLookup[Func](functionName, funLookup.get, lexicallyInsideOf.lookupFunction)
    }

    def lookupFieldRaw(name: String): Option[Field] = {
        fieldLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFieldRaw(name)
        }
    }
    def lookupTransactionRaw(name: String): Option[Transaction] = {
        txLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupTransactionRaw(name)
        }
    }
    def lookupFunctionRaw(name: String): Option[Func] = {
        funLookupRaw.get(name) match {
            case x@Some(_) => x
            case None => lexicallyInsideOf.lookupFunctionRaw(name)
        }
    }
}

class ContractTable(
        val contract: Contract,
        symbolTable: SymbolTable,
        parentContract: Option[ContractTable]) extends DeclarationTable {
    assert (contract != null)

    def this(astNodeRaw: Contract, symbolTable: SymbolTable) =
        this(astNodeRaw, symbolTable, None)

    def this(astNodeRaw: Contract, symbolTable: SymbolTable, parentContract: ContractTable) =
        this(astNodeRaw, symbolTable, Some(parentContract))

    private var fieldLookup: Map[String, Field] = indexDecl[ObsidianType, Field](contract.declarations, FieldDeclTag)
    private var txLookup: Map[String, Transaction] = indexDecl[ObsidianType, Transaction](contract.declarations, TransactionDeclTag)
    private var funLookup: Map[String, Func] = indexDecl[ObsidianType, Func](contract.declarations, FuncDeclTag)

    def simpleType = JustContractType(name)

    val fieldLookupRaw: Map[String, Field] = {
        indexDecl[ParsableType, Field](contract.declarations, FieldDeclTag)
    }

    val txLookupRaw: Map[String, Transaction] = {
        indexDecl[ParsableType, Transaction](contract.declarations, TransactionDeclTag)
    }

    val funLookupRaw: Map[String, Func] = {
        indexDecl[ParsableType, Func](contract.declarations, FuncDeclTag)
    }

    val stateLookup: Map[String, StateTable] = {
        indexDecl[ParsableType, State](contract.declarations, StateDeclTag).mapValues(
            (st: State) => new StateTable(st, this)
        )
    }

    val childContractLookup: Map[String, ContractTable] = {
        indexDecl[ParsableType, Contract](contract.declarations, ContractDeclTag).mapValues(
            (ct: Contract) => new ContractTable(ct, symbolTable, this)
        )
    }

    val contractTable: ContractTable = this

    def ast: AST = contract

    def name: String = this.contract.name

    /* resolves a contract from the point of view of this contract. Two cases:
     * 1) [name] refers to a global contract and it's in the symbol table
     * 2) [name] refers to a child/nested contract of this contract
     */
    def lookupContract(name: String): Option[ContractTable] = {
        if (name == this.name) Some(this) else
        (symbolTable.contract(name), childContract(name)) match {
            case (_, Some(ct)) => Some(ct)
            case (Some(ct), _) => Some(ct)
            case _ => None
        }
    }

    def lookupField(name: String): Option[Field] = fieldLookup.get(name)
    def lookupTransaction(name: String): Option[Transaction] = txLookup.get(name)
    def lookupFunction(name: String): Option[Func] = funLookup.get(name)
    def lookupFieldRaw(name: String): Option[Field] = fieldLookupRaw.get(name)
    def lookupTransactionRaw(name: String): Option[Transaction] = txLookupRaw.get(name)
    def lookupFunctionRaw(name: String): Option[Func] = funLookupRaw.get(name)

    def state(name: String): Option[StateTable] = stateLookup.get(name)
    def possibleStates: Set[String] = stateLookup.values.map(_.name).toSet

    def childContract(name: String): Option[ContractTable] =
        childContractLookup.get(name)

    def parent: Option[ContractTable] = parentContract
    def hasParent: Boolean = parent.isDefined

    def constructors: Seq[Constructor] = {
        contract.declarations.filter(_.tag == ConstructorDeclTag)
                        .map(_.asInstanceOf[Constructor])
    }
}

class SymbolTable(astNodeRaw: Program) {
    var contractLookup: Map[String, ContractTable] = {
        var table = TreeMap[String, ContractTable]()
        for (contract <- astNodeRaw.contracts) {
            table = table.updated(contract.name, new ContractTable(contract, this))
        }
        table
    }

    def recordResolvedAST(ast: Program): Unit = {
        resolvedASTNode = ast
    }

    private var resolvedASTNode: Program = _

    def resolvedAST: Program = resolvedASTNode
    def ast: Program = if (resolvedASTNode != null) resolvedASTNode else astNodeRaw

    /* only retrieves top level contracts (i.e. not nested) */
    def contract: Function[String, Option[ContractTable]] = contractLookup.get
}

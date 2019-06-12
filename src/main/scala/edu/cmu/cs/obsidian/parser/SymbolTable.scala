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

    def contractType: ContractType

    def indexDecl[TCast](decls: Seq[Declaration], tag: DeclarationTag): Map[String, TCast] = {
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
    private var fieldLookup: Map[String, Field] = indexDecl[Field](ast.fields, FieldDeclTag)
    private var txLookup: Map[String, Transaction] = indexDecl[Transaction](ast.fields, TransactionDeclTag)

    def contractTable: ContractTable = lexicallyInsideOf
    def contract: Contract = lexicallyInsideOf.contract

    def name: String = astNodeRaw.name

    def nonPrimitiveType = StateType(contract.name, astNodeRaw.name, false)
    def contractType: ContractType = ContractType(name)

    def ast: State = astNode

    def lookupContract(name: String): Option[ContractTable] = contractTable.lookupContract(name)

    def allFields: Set[Field] = fieldLookup.values.toSet

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
                        val availableInCurrentState = availableIn.get.exists(p => p == name)
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

    private var fieldLookup: Map[String, Field] = indexDecl[Field](contract.declarations, FieldDeclTag)
    private var txLookup: Map[String, Transaction] = indexDecl[Transaction](contract.declarations, TransactionDeclTag)

    val allFields: Set[Field] = fieldLookup.values.toSet

    // We know we are in one of the given states. Which fields are available?
    def allFieldsAvailableInStates(stateNames: Set[String]) = {
        allFields.filter((f: Field) => f.availableIn.isEmpty || stateNames.subsetOf(f.availableIn.get))
    }

    def contractType = ContractType(name)


    val stateLookup: Map[String, StateTable] = {
        indexDecl[State](contract.declarations, StateDeclTag).mapValues(
            (st: State) => new StateTable(st, this)
        )
    }

    val childContractLookup: Map[String, ContractTable] = {
        indexDecl[Contract](contract.declarations, ContractDeclTag).mapValues(
            (ct: Contract) => new ContractTable(ct, symbolTable, this)
        )
    }

    val contractTable: ContractTable = this

    def ast: AST = contract

    def name: String = this.contract.name

    /* resolves a contract from the point of view of this contract. Three cases:
     * 1) [name] refers to a global contract and it's in the symbol table
     * 2) [name] refers to a child/nested contract of this contract
     * 3) [name] refers to a contract declared in an import of this program.
     */
    def lookupContract(name: String): Option[ContractTable] = {
        val localLookupResult =
            if (name == this.name) Some(this) else
                (symbolTable.contract(name), childContract(name)) match {
                    case (_, Some(ct)) => Some(ct)
                    case (Some(ct), _) => Some(ct)
                    case _ => None
                }
        if (localLookupResult.isDefined) {
            localLookupResult
        } // otherwise nothing was found
        else None

    }

    def lookupField(name: String): Option[Field] = fieldLookup.get(name)

    def lookupTransaction(name: String): Option[Transaction] = contract match {
        case obsContract: ObsidianContractImpl => txLookup.get(name)
        case javaContract: JavaFFIContractImpl =>
            val interfaceContractTable = lookupContract(javaContract.interface)
            interfaceContractTable match {
                case None => None
                case Some(t) => t.lookupTransaction(name)
            }
    }

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

class SymbolTable(program: Program) {
    var contractLookup: Map[String, ContractTable] = {
        var table = TreeMap[String, ContractTable]()
        for (contract <- program.contracts) {
            table = table.updated(contract.name, new ContractTable(contract, this))
        }
        table
    }

    def ast: Program = program

    /* only retrieves top level contracts (i.e. not nested) */
    def contract: Function[String, Option[ContractTable]] = contractLookup.get
}

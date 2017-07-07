package edu.cmu.cs.obsidian.parser

import scala.collection.{Map, Seq}
import scala.collection.immutable.TreeMap
import scala.reflect.{ClassTag, classTag}
import edu.cmu.cs.obsidian.typecheck._

sealed trait DeclarationTable {
    def name: String
    def ast: AST
    /* id if this is a [ContractTable] already, or gets the [ContractTable] of a [StateTable] */
    def contract: ContractTable
    /* looks for a contract called [name] that's in scope
     * (either globally or in this particular contract) */
    def contract(name: String): Option[ContractTable]
    def field(name: String): Option[Field]
    def transaction(name: String): Option[Transaction]
    def function(name: String): Option[Func]
    def simpleTypeOf: SimpleType
}

object SymbolTableHelpers {
    /* reflection is used here to get around generic type erasure */
    def indexDecl[D <: Declaration: ClassTag](decls: Seq[Declaration]): Map[String, D] = {
        var lookup = new TreeMap[String, D]()

        val classOfD = classTag[D].runtimeClass

        for (decl <- decls if classOfD.isInstance(decl)) {
            lookup = lookup.updated(decl.name, decl.asInstanceOf[D])
        }

        lookup
    }
}

class StateTable(astNode: State, insideOf: ContractTable) extends DeclarationTable {

    val fieldLookup: Map[String, Field] = {
        SymbolTableHelpers.indexDecl[Field](astNode.declarations)
    }

    val txLookup: Map[String, Transaction] = {
        SymbolTableHelpers.indexDecl[Transaction](astNode.declarations)
    }

    val funLookup: Map[String, Func] = {
        SymbolTableHelpers.indexDecl[Func](astNode.declarations)
    }

    def name: String = astNode.name
    def ast: State = astNode
    def contract: ContractTable = insideOf
    def contract(name: String): Option[ContractTable] = contract.contract(name)

    def field(name: String): Option[Field] = {
        fieldLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.field(name)
        }
    }
    def transaction(name: String): Option[Transaction] = {
        txLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.transaction(name)
        }
    }
    def function(name: String): Option[Func] = {
        funLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.function(name)
        }
    }

    def simpleTypeOf: SimpleType = StateType(this.contract.name, this.name)
}

class ContractTable(
        astNode: Contract,
        symbolTable: SymbolTable,
        parentContract: Option[ContractTable]) extends DeclarationTable {

    def this(astNode: Contract, symbolTable: SymbolTable) =
        this(astNode, symbolTable, None)

    def this(astNode: Contract, symbolTable: SymbolTable, parentContract: ContractTable) =
        this(astNode, symbolTable, Some(parentContract))

    val stateLookup: Map[String, StateTable] = {
        SymbolTableHelpers.indexDecl[State](astNode.declarations).mapValues(
            (st: State) => new StateTable(st, this)
        )
    }

    val fieldLookup: Map[String, Field] = {
        SymbolTableHelpers.indexDecl[Field](astNode.declarations)
    }

    val txLookup: Map[String, Transaction] = {
        SymbolTableHelpers.indexDecl[Transaction](astNode.declarations)
    }

    val funLookup: Map[String, Func] = {
        SymbolTableHelpers.indexDecl[Func](astNode.declarations)
    }

    val childContractLookup: Map[String, ContractTable] = {
        SymbolTableHelpers.indexDecl[Contract](astNode.declarations).mapValues(
            (ct: Contract) => new ContractTable(ct, symbolTable, this)
        )
    }

    def name: String = astNode.name

    def contract: ContractTable = this

    /* resolves a contract from the point of view of this contract. Two cases:
     * 1) [name] refers to a global contract and it's in the symbol table
     * 2) [name] refers to a child/nested contract of this contract
     */
    def contract(name: String): Option[ContractTable] = {
        if (name == this.name) Some(this) else
        (symbolTable.contract(name), childContract(name)) match {
            case (_, Some(ct)) => Some(ct)
            case (Some(ct), _) => Some(ct)
            case _ => None
        }
    }

    def ast: Contract = astNode
    def field(name: String): Option[Field] = fieldLookup.get(name)
    def transaction(name: String): Option[Transaction] = txLookup.get(name)
    def function(name: String): Option[Func] = funLookup.get(name)

    def state(name: String): Option[StateTable] = stateLookup.get(name)
    def possibleStates: Set[String] = stateLookup.values.map(_.name).toSet

    def childContract(name: String): Option[ContractTable] =
        childContractLookup.get(name)

    def parent: Option[ContractTable] = parentContract
    def hasParent: Boolean = parent.isDefined

    def constructors: Seq[Constructor] = {
        ast.declarations.filter(_.isInstanceOf[Constructor])
                        .map(_.asInstanceOf[Constructor])
    }

    def simpleTypeOf: SimpleType = JustContractType(this.name)
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

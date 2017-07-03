package edu.cmu.cs.obsidian.parser

import scala.collection.{Map, Seq}
import scala.collection.immutable.TreeMap
import scala.reflect.{ClassTag, classTag}

sealed trait SimpleType
case class JustContractType(contractName: String) extends SimpleType
case class StateType(contractName: String, stateName: String) extends SimpleType

/* This is necessarily different from the representation of types in the AST; it's
* unclear in the AST if a reference is "shared" or "owned" when it has no modifier */
sealed trait Type
case class ReadOnlyRef(t: SimpleType) extends Type
case class SharedRef(t: SimpleType) extends Type
case class OwnedRef(t: SimpleType) extends Type
case class IntType() extends Type
case class BoolType() extends Type
case class StringType() extends Type
/* Used to indicate an error in the type checker when a reasonable type cannot
 * otherwise be inferred */
case class BottomType() extends Type

sealed trait DeclarationTable {
    def name: String
    def ast: AST
    def contract: ContractTable
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

class ContractTable(astNode: Contract) extends DeclarationTable {

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

    def name: String = astNode.name
    def contract: ContractTable = this
    def ast: Contract = astNode
    def field(name: String): Option[Field] = fieldLookup.get(name)
    def transaction(name: String): Option[Transaction] = txLookup.get(name)
    def function(name: String): Option[Func] = funLookup.get(name)
    def state(name: String): Option[StateTable] = stateLookup.get(name)

    def constructors: Seq[Constructor] = {
        var constructors: List[Constructor] = Nil
        for (rawDecl <- astNode.declarations) {
            rawDecl match {
                case c: Constructor =>
                    constructors = c::constructors
                case _ => ()
            }
        }
        constructors
    }

    def simpleTypeOf: SimpleType = JustContractType(this.name)
}

class SymbolTable(program: Program) {
    var contractLookup: Map[String, ContractTable] = {
        var table = TreeMap[String, ContractTable]()
        for (contract <- program.contracts) {
            table = table.updated(contract.name, new ContractTable(contract))
        }
        table
    }

    def ast: Program = program
    def contract: Function[String, Option[ContractTable]] = contractLookup.get
}

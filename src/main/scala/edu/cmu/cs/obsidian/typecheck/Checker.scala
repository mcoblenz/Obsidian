package edu.cmu.cs.obsidian.typecheck

import edu.cmu.cs.obsidian.parser._
import scala.collection.immutable.TreeMap
import scala.collection.Map

sealed trait SimpleType
case class ContractType(contractName: String) extends SimpleType
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

class IndexedState(
                      state: State,
                      insideOf: IndexedContract,
                      fieldLookup: Map[String, Field],
                      txLookup: Map[String, Transaction],
                      funLookup: Map[String, Func]
                  ) {
    def getAst = state

    def getField(name: String): Option[Field] = {
        fieldLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.getField(name)
        }
    }
    def getTransaction(name: String): Option[Transaction] = {
        txLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.getTransaction(name)
        }
    }
    def getFunction(name: String): Option[Func] = {
        funLookup.get(name) match {
            case x@Some(_) => x
            case None => insideOf.getFunction(name)
        }
    }
}

class IndexedContract(
                         contract: Contract,
                         stateLookup: Map[String, IndexedState],
                         fieldLookup: Map[String, Field],
                         txLookup: Map[String, Transaction],
                         funLookup: Map[String, Func]
                     ) {
    def getAst = contract
    def getField = fieldLookup.get(_)
    def getTransaction = txLookup.get(_)
    def getFunction = funLookup.get(_)
    def getState = stateLookup.get(_)
}

class IndexedProgram(
                        program: Program,
                        contractLookup: Map[String, IndexedContract]) {
    def getAst = program
    def getContract = contractLookup.get(_)
}

class Checker {

    var progInfo: IndexedProgram = null

    def translateType(t: AstType): Type = {

        val getModifier = (t: SimpleType, mods: Seq[TypeModifier], contractName: String) => {
            val contract = progInfo.getContract(contractName).get.getAst
            // TODO: what is the behavior of a contract that is not labeled?
            val mod = contract.mod match {
                case Some(m) => m
                case None => IsOwned
            }

            /* if a reference is 'readonly', it is labeled as such; otherwise, it is 'owned'/'shared', based on the
             * declaration of the contract itself */
            if (mods.contains(IsReadOnly)) {
                new ReadOnlyRef(t)
            } else if (mod == IsOwned) {
                new OwnedRef(t)
            } else {
                // TODO: are main contracts always deemed shared by the type system?
                new SharedRef(t)
            }
        }

        t match {
            case AstIntType() => new IntType()
            case AstBoolType() => new BoolType()
            case AstStringType() => new StringType()
            case AstContractType(mods, name) =>
                getModifier(new ContractType(name), mods, name)
            case AstStateType(mods, nameC, nameS) =>
                getModifier(new StateType(nameC, nameS), mods, nameC)
        }
    }

    def checkExpr(insideOf: (Statement, Transaction with Func, State with Contract),
                  context: Map[String, Type],
                  e: Expression): Either[String, Type] = {
         e match {
             case Variable(x: String) =>
                 context get x match {
                     case Some(t) => Right(t)
                     case None => Left(f"variable '$x' not ")
                 }
             case NumLiteral(value: Int) => Right(new IntType())
             case StringLiteral(value: String) => Right(new StringType())
             case TrueLiteral() => Right(new BoolType())
             case FalseLiteral() => Right(new BoolType())
             case This() => Left("Not Implemented.") // todo
             case Conjunction(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case Disjunction(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case LogicalNegation(e: Expression) => Left("Not Implemented.") // todo
             case Add(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case Subtract(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case Divide(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case Multiply(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case Equals(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case GreaterThan(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case GreaterThanOrEquals(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case LessThan(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case LessThanOrEquals(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case NotEquals(e1: Expression, e2: Expression) => Left("Not Implemented.") // todo
             case Dereference(e: Expression, f: String) => Left("Not Implemented.") // todo
             case LocalInvocation(name: String, args: Seq[Expression]) => Left("Not Implemented.") // todo
             case Invocation(recipient: Expression, name: String, args: Seq[Expression]) => Left("Not Implemented.") // todo
             case Construction(name: String, args: Seq[Expression]) => Left("Not Implemented.") // todo
        }
    }

    def checkStatement(insideOf: (Transaction with Func, State with Contract),
                       context: Map[String, Type],
                       s: Statement): Option[String] = {
        s match {
            case VariableDecl(typ: AstType, varName: String) => None // todo
            case VariableDeclWithInit(typ: AstType, varName: String, e: Expression) => None // todo
            case Return => None // todo
            case ReturnExpr(e: Expression) => None // todo
            case Transition(newStateName: String, updates: Seq[(Variable, Expression)]) => None // todo
            case Assignment(assignTo: Expression, e: Expression) => None // todo
            case Throw() => None // todo
            case If(eCond: Expression, s: Seq[Statement]) => None // todo
            case IfThenElse(eCond: Expression, s1: Seq[Statement], s2: Seq[Statement]) => None // todo
            case TryCatch(s1: Seq[Statement], s2: Seq[Statement]) => None // todo
            case Switch(e: Expression, cases: Seq[SwitchCase]) => None // todo
            case LocalInvocation(name: String, args: Seq[Expression]) => None // todo
            case Invocation(recipient: Expression, name: String, args: Seq[Expression]) => None // todo
            case Construction(name: String, args: Seq[Expression]) => None // todo
            case expr => Some(s"Statement $expr has no side effects")
        }
    }

    def checkSimpleType(st: SimpleType): Option[String] = {
        st match {
            case ContractType(name) =>
                val lookup = progInfo.getContract(name)
                if (lookup.isEmpty) Some(s"Couldn't find a contract named $name")
                else None
            case StateType(cName, sName) =>
                val ctLookup = progInfo.getContract(cName)
                if (ctLookup.isEmpty) return Some(s"Couldn't find a contract named $cName")
                val stLookup = ctLookup.get.getState(sName)
                if (stLookup.isEmpty) return Some(s"Couldn't find a state named $sName in contract $cName")
                None
        }
    }

    def checkField(field: Field): Option[String] = {
        translateType(field.typ) match {
            case OwnedRef(simple) => checkSimpleType(simple)

            case SharedRef(StateType(_, _)) => Some(s"State-specific types are not safe for 'shared' references")
            case SharedRef(simple) => checkSimpleType(simple)

            case ReadOnlyRef(StateType(_, _)) => Some(s"State-specific types are not safe for 'readonly' references")
            case ReadOnlyRef(simple) => checkSimpleType(simple)

            case _ => None
        }
    }

    def checkTransaction(tx: Transaction, insideOf: Contract): Option[String] = {
        None // todo
    }

    def checkFunc(func: Func, insideOf: Contract): Option[String] = {
        None // todo
    }

    def checkContract(contract: Contract): Option[String] = {
        None // todo
    }

    def indexDecl(decls: Seq[Declaration]): (Map[String, Field], Map[String, Transaction], Map[String, Func]) = {
        var fieldLookup = new TreeMap[String, Field]()
        var txLookup = new TreeMap[String, Transaction]()
        var funLookup = new TreeMap[String, Func]()

        for (decl <- decls) {
            decl match {
                case f@Field(_, name) => fieldLookup = fieldLookup.updated(name, f)
                case t@Transaction(name, _, _, _, _) => txLookup = txLookup.updated(name, t)
                case f@Func(name, _, _, _) => funLookup = funLookup.updated(name, f)
                case _ => ()
            }
        }

        (fieldLookup, txLookup, funLookup)
    }

    def indexState(state: State, insideOf: IndexedContract): IndexedState = {
        val (fieldLookup, txLookup, funLookup) = indexDecl(state.declarations)

        new IndexedState(state, insideOf, fieldLookup, txLookup, funLookup)
    }

    def indexContract(contract: Contract): IndexedContract = {
        val (fieldLookup, txLookup, funLookup) = indexDecl(contract.declarations)

        /* this is mutable essentially in order to easily allow the construction of a circular reference between
         * an [IndexedState] object and its containing [IndexedContract] */
        val stateLookup = new collection.mutable.HashMap[String, IndexedState]()

        val indexed = new IndexedContract(contract, stateLookup, fieldLookup, txLookup, funLookup)

        /* now that we have the [IndexedContract], we index states and put them in the map */

        for (decl <- contract.declarations) {
            decl match {
                case s@State(name, _) => stateLookup.put(name, indexState(s, indexed))
                case _ => ()
            }
        }

        indexed
    }

    /* this doesn't actually check anything, but indexes data by name so that searching for a
     * contract/field/tx/function is easy/fast */
    def indexProgram(program: Program): Unit = {
        var contractLookup = new TreeMap[String, IndexedContract]
        for (contract <- program.contracts) {
            contractLookup = contractLookup.updated(contract.name, indexContract(contract))
        }

        progInfo = new IndexedProgram(program, contractLookup)
    }

    def checkProgram(program: Program): Option[String] = {
        indexProgram(program)
        for (contract <- program.contracts) {
            checkContract(contract) match {
                case err@Some(_) => return err
                case _ => ()
            }
        }

        None
    }
}

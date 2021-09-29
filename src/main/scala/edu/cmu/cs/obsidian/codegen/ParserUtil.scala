package edu.cmu.cs.obsidian

import edu.cmu.cs.obsidian.parser._
import edu.cmu.cs.obsidian.typecheck.ObsidianType

package object ParserUtil {
    /**
      * traverse the structure of an expression and compute whether or not a given property holds
      * on its type annotation. (nb: this is a map-reduce and there may be a slick scala OOP way to
      * save actually writing it out by hand. it's also not clear to me that boolean is the best
      * return type; option might be more informative, so that you can return information about the
      * place where the property fails.)
      *
      * this is in its own file to avoid some nasty import conflicts
      *
      * @param e    the expression to traverse
      * @param prop the property of interest
      * @return true if every type annotation in the expression has the property; false otherwise
      */
    def expressionWithTypeProperty(e: edu.cmu.cs.obsidian.parser.Expression, prop: Option[ObsidianType] => Boolean): Boolean = {
        e match {
            case expression: AtomicExpression => expression match {
                case ReferenceIdentifier(name, typ) => prop(typ)
                case NumLiteral(_) => true
                case parser.StringLiteral(_) => true
                case TrueLiteral() => true
                case FalseLiteral() => true
                case This(typ) => prop(typ)
                case Parent() => true
            }
            case expression: UnaryExpression => expression match {
                case LogicalNegation(e) => expressionWithTypeProperty(e, prop)
                case Negate(e) => expressionWithTypeProperty(e, prop)
                case Dereference(e, _) => expressionWithTypeProperty(e, prop)
                case Disown(e) => expressionWithTypeProperty(e, prop)
            }
            case expression: BinaryExpression => prop(expression.obstype) &&
                (expression match {
                    case Conjunction(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case Disjunction(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case Add(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case StringConcat(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case Subtract(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case Divide(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case Multiply(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case Mod(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case parser.Equals(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case GreaterThan(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case GreaterThanOrEquals(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case LessThan(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case LessThanOrEquals(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                    case NotEquals(e1, e2) => expressionWithTypeProperty(e1, prop) && expressionWithTypeProperty(e2, prop)
                })
            case LocalInvocation(name, genericParams, params, args, typ) => prop(typ) && args.forall(ePrime => expressionWithTypeProperty(ePrime, prop))
            case Invocation(recipient, genericParams, params, name, args, isFFIInvocation, typ) => expressionWithTypeProperty(recipient, prop) && prop(typ) && args.forall(ePrime => expressionWithTypeProperty(ePrime, prop))
            case Construction(contractType, args, isFFIInvocation, typ) => prop(typ) && args.forall(ePrime => expressionWithTypeProperty(ePrime, prop))
            case StateInitializer(stateName, fieldName, typ) => prop(typ)
        }
    }

    def statementWithExpTypeProperty(s: Statement, prop: Option[ObsidianType] => Boolean): Boolean = {
        s match {
            case e: Expression => expressionWithTypeProperty(e, prop)
            case VariableDecl(typ, varName) => true
            case VariableDeclWithInit(typ, varName, e) => expressionWithTypeProperty(e, prop)
            case VariableDeclWithSpec(typIn, typOut, varName) => true
            case Return() => true
            case ReturnExpr(e) => expressionWithTypeProperty(e, prop)
            case Transition(newStateName, updates, thisPermission) => updates match {
                case Some(updates) => updates.forall(u => expressionWithTypeProperty(u._2, prop))
                case None => true
            }
            case Assignment(assignTo, e) => expressionWithTypeProperty(assignTo, prop) && expressionWithTypeProperty(e, prop)
            case Revert(maybeExpr) => maybeExpr match {
                case Some(e) => expressionWithTypeProperty(e, prop)
                case None => true
            }
            case If(eCond, s) => expressionWithTypeProperty(eCond, prop) && s.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case IfThenElse(eCond, s1, s2) => expressionWithTypeProperty(eCond, prop) && s1.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)) && s2.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case IfInState(e, ePerm, typeState, s1, s2) => expressionWithTypeProperty(e, prop) && s1.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)) && s2.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case TryCatch(s1, s2) => s1.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)) && s2.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case Switch(e, cases) => expressionWithTypeProperty(e, prop) && cases.forall((sc: SwitchCase) => sc.body.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)))
            case StaticAssert(e, typeState) => expressionWithTypeProperty(e, prop)
        }
    }

    def declarationWithExpTypeProperty(d: Declaration, prop: Option[ObsidianType] => Boolean): Boolean = {
        d match {
            case declaration: InvokableDeclaration => declaration match {
                case Constructor(name, args, resultType, body) => body.forall((s: Statement) => statementWithExpTypeProperty(s, prop))
                case Transaction(name, params, args, retType, ensures, body, isStatic, isPrivate, thisType, thisFinalType, initialFieldTypes, finalFieldTypes) =>
                    body.forall((s: Statement) => statementWithExpTypeProperty(s, prop)) && ensures.forall((e: Ensures) => expressionWithTypeProperty(e.expr, prop))
            }
            case TypeDecl(name, typ) => true
            case Field(isConst, typ, name, availableIn) => true
            case State(name, fields, isAsset) => true // can fields have expressions in them? i don't think so
            case contract: Contract => contractWithExpTypeProperty(contract, prop)
        }
    }


    def contractWithExpTypeProperty(c: Contract, prop: Option[ObsidianType] => Boolean): Boolean = {
        c match {
            case ObsidianContractImpl(modifiers, name, params, bound, declarations, transitions, isInterface, sp) =>
                declarations.forall((d: Declaration) => declarationWithExpTypeProperty(d, prop))
            case JavaFFIContractImpl(name, interface, javaPath, sp, declarations) => assert(false, "we don't support these yet"); true
        }
    }

    def symbolTableWithExpTypeProperty(st: SymbolTable, prop: Option[ObsidianType] => Boolean): Boolean = {
        st.ast.contracts.forall((c: Contract) => contractWithExpTypeProperty(c, prop))
    }
}

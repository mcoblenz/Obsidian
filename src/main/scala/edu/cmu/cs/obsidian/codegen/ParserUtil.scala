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
    def expressionHasTypeProperty(e: edu.cmu.cs.obsidian.parser.Expression, prop: Option[ObsidianType] => Boolean): Boolean = {
        e match {
            case expression: AtomicExpression => expression match {
                case ReferenceIdentifier(_, typ) => prop(typ)
                case NumLiteral(_) => true
                case parser.StringLiteral(_) => true
                case TrueLiteral() => true
                case FalseLiteral() => true
                case This(typ) => prop(typ)
                case Parent() => true
            }
            case expression: UnaryExpression => expression match {
                case LogicalNegation(e) => expressionHasTypeProperty(e, prop)
                case Negate(e) => expressionHasTypeProperty(e, prop)
                case Dereference(e, _) => expressionHasTypeProperty(e, prop)
                case Disown(e) => expressionHasTypeProperty(e, prop)
            }
            case expression: BinaryExpression => prop(expression.obstype) &&
                (expression match {
                    case Conjunction(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case Disjunction(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case Add(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case StringConcat(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case Subtract(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case Divide(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case Multiply(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case Mod(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case parser.Equals(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case GreaterThan(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case GreaterThanOrEquals(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case LessThan(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case LessThanOrEquals(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                    case NotEquals(e1, e2) => expressionHasTypeProperty(e1, prop) && expressionHasTypeProperty(e2, prop)
                })
            case LocalInvocation(_, _, _, args, typ) => prop(typ) && args.forall(ePrime => expressionHasTypeProperty(ePrime, prop))
            case Invocation(recipient, _, _, _, args, _, typ) => expressionHasTypeProperty(recipient, prop) && prop(typ) && args.forall(ePrime => expressionHasTypeProperty(ePrime, prop))
            case Construction(_, args, _, typ) => prop(typ) && args.forall(ePrime => expressionHasTypeProperty(ePrime, prop))
            case StateInitializer(_, _, typ) => prop(typ)
        }
    }

    def statementWithExpTypeProperty(s: Statement, prop: Option[ObsidianType] => Boolean): Boolean = {
        s match {
            case e: Expression => expressionHasTypeProperty(e, prop)
            case VariableDecl(_, _) => true
            case VariableDeclWithInit(_, _, e) => expressionHasTypeProperty(e, prop)
            case VariableDeclWithSpec(_, _, _) => true
            case Return() => true
            case ReturnExpr(e) => expressionHasTypeProperty(e, prop)
            case Transition(_, updates, _) => updates match {
                case Some(updates) => updates.forall(u => expressionHasTypeProperty(u._2, prop))
                case None => true
            }
            case Assignment(assignTo, e) => expressionHasTypeProperty(assignTo, prop) && expressionHasTypeProperty(e, prop)
            case Revert(maybeExpr) => maybeExpr match {
                case Some(e) => expressionHasTypeProperty(e, prop)
                case None => true
            }
            case If(eCond, s) => expressionHasTypeProperty(eCond, prop) && s.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case IfThenElse(eCond, s1, s2) => expressionHasTypeProperty(eCond, prop) && s1.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)) && s2.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case IfInState(e, _, _, s1, s2) => expressionHasTypeProperty(e, prop) && s1.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)) && s2.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case TryCatch(s1, s2) => s1.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)) && s2.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop))
            case Switch(e, cases) => expressionHasTypeProperty(e, prop) && cases.forall((sc: SwitchCase) => sc.body.forall((sPrime: Statement) => statementWithExpTypeProperty(sPrime, prop)))
            case StaticAssert(e, _) => expressionHasTypeProperty(e, prop)
        }
    }

    def declarationWithExpTypeProperty(d: Declaration, prop: Option[ObsidianType] => Boolean): Boolean = {
        d match {
            case declaration: InvokableDeclaration => declaration match {
                case Constructor(_, _, _, body) => body.forall((s: Statement) => statementWithExpTypeProperty(s, prop))
                case Transaction(_, _, _, _, ensures, body, _, _, _, _, _, _) =>
                    body.forall((s: Statement) => statementWithExpTypeProperty(s, prop)) && ensures.forall((e: Ensures) => expressionHasTypeProperty(e.expr, prop))
            }
            case TypeDecl(_, _) => true
            case Field(_, _, _, _) => true
            case State(_, _, _) => true
            case contract: Contract => contractWithExpTypeProperty(contract, prop)
        }
    }


    def contractWithExpTypeProperty(c: Contract, prop: Option[ObsidianType] => Boolean): Boolean = {
        c match {
            case ObsidianContractImpl(_, _, _, _, declarations, _, _, _) =>
                declarations.forall((d: Declaration) => declarationWithExpTypeProperty(d, prop))
            case JavaFFIContractImpl(_, _, _, _, _) => assert(assertion = false, "we don't support these yet"); true
        }
    }

    def symbolTableWithExpTypeProperty(st: SymbolTable, prop: Option[ObsidianType] => Boolean): Boolean = {
        st.ast.contracts.forall((c: Contract) => contractWithExpTypeProperty(c, prop))
    }

    /**
      * given an expression e and a type t, compute the expression that is the same except that the obstype annotation
      * at the top level of the AST has been replaced with Some(t). this isn't an instance of the Scala generated .copy()
      * function because of the exact inheritance structure of Expressions.
      *
      * note that this does not recurr into e, we just change the top level annotation if there is one.
      *
      * @param e       the expression to update
      * @param newType the new e
      * @return the updated expression
      */
    def updateExprType(e: Expression, newType: ObsidianType): Expression = {
        e match {
            case expression: AtomicExpression => expression match {
                case r@ReferenceIdentifier(_, _) => r.copy(obstype = Some(newType)).setLoc(r)
                case NumLiteral(_) => e
                case StringLiteral(_) => e
                case TrueLiteral() => e
                case FalseLiteral() => e
                case r@This(_) => r.copy(obstype = Some(newType)).setLoc(r)
                case Parent() => e
            }
            case expression: UnaryExpression => expression // none of the unary expressions have constructors that take the obstype as an argument
            case expression: BinaryExpression => expression // ditto
            case r@LocalInvocation(_, _, _, _, _) => r.copy(obstype = Some(newType)).setLoc(r)
            case r@Invocation(_, _, _, _, _, _, _) => r.copy(obstype = Some(newType)).setLoc(r)
            case r@Construction(_, _, _, _) => r.copy(obstype = Some(newType)).setLoc(r)
            case r@StateInitializer(_, _, _) => r.copy(obstype = Some(newType)).setLoc(r)
        }
    }
}

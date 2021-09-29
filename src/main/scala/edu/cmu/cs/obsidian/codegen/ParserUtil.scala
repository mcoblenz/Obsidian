package edu.cmu.cs.obsidian

import edu.cmu.cs.obsidian.typecheck.ObsidianType
import edu.cmu.cs.obsidian.parser._

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

}

package idealised.DPIA.Types

import idealised.DPIA._
import idealised.DPIA.Phrases._

import scala.language.{postfixOps, reflectiveCalls}

object TypeOf {
  def apply[T <: PhraseType](phrase: Phrase[T]): T = {
    (phrase match {
      case x: Identifier[T] => x.t
      case Lambda(x, p) => x.t -> p.t
      case Apply(p, q) =>
        p.t match {
          case FunctionType(_, t2) => t2
          case null => null
        }

      case NatDependentLambda(a, p) => (a: NatIdentifier) -> p.t

      case TypeDependentLambda(a, p) => (a: DataTypeIdentifier) -> p.t

      case NatDependentApply(p, e) =>
        p.t match {
          case NatDependentFunctionType(a, t) => t `[` e `/` a `]`
          case null => null
        }

      case TypeDependentApply(p, e) =>
        p.t match {
          case TypeDependentFunctionType(a, t) => t `[` e `/` a `]`
          case null => null
        }

      case Pair(p, q) => p.t x q.t

      case Proj1(p) =>
        p.t match {
          case PairType(t1, _) => t1
          case null => null
        }

      case Proj2(p) =>
        p.t match {
          case PairType(_, t2) => t2
          case null => null
        }

      case IfThenElse(cond, thenP, elseP) =>
        assert(thenP.t == elseP.t)
        thenP.t

      case Literal(_, t) => t

      case UnaryOp(op, x) =>
        x.t match {
          case ExpType(dt) => x.t
          case null => null
        }

      case BinOp(op, lhs, rhs) =>
        op match {
          case BinOp.Op.GT | BinOp.Op.LT => exp"[$bool]"
          case _ => (lhs.t.dataType, rhs.t.dataType) match {
            case (t1, t2) if t1 == t2 => ExpType(t1)
            case (IndexType(n), `int`) =>
              ExpType(IndexType(n))
//              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(n, OperationalSemantics.evalIntExp(rhs))))
            case (`int`, IndexType(n)) =>
              ExpType(IndexType(n))
//              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(OperationalSemantics.evalIntExp(lhs), n)))
            case (IndexType(n), IndexType(m)) =>
//              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(n, m)))
              ExpType(IndexType(n))
            case (null, null) => null
          }
        }

      case c: Primitive[_] => c.t
    }).asInstanceOf[T]
  }
}

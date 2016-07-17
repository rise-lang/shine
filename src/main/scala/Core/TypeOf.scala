package Core

import scala.language.postfixOps
import scala.language.reflectiveCalls

object TypeOf {
  def apply[T <: PhraseType](phrase: Phrase[T]): T = {
    (phrase match {
      case x: IdentPhrase[T] => x.t
      case LambdaPhrase(x, p) => x.t -> p.t
      case ApplyPhrase(p, q) =>
        p.t match {
          case FunctionType(_, t2) => t2
          case null => null
        }

      case NatDependentLambdaPhrase(a, p) => a -> p.t

      case TypeDependentLambdaPhrase(dt, p) => dt -> p.t

      case NatDependentApplyPhrase(p, ae) =>
        p.t match {
          case NatDependentFunctionType(a, t) => t `[` ae `/` a `]`
          case null => null
        }

      case TypeDependentApplyPhrase(p, dt) =>
        p.t match {
          case TypeDependentFunctionType(a, t) => t `[` dt `/` a `]`
          case null => null
        }

      case PairPhrase(p, q) => p.t x q.t

      case Proj1Phrase(p) =>
        p.t match {
          case PairType(t1, _) => t1
          case null => null
        }

      case Proj2Phrase(p) =>
        p.t match {
          case PairType(_, t2) => t2
          case null => null
        }

      case IfThenElsePhrase(cond, thenP, elseP) =>
        thenP.t

      case LiteralPhrase(_, t) => t

      case UnaryOpPhrase(op, x) =>
        x.t match {
          case ExpType(dt) => x.t
          case null => null
        }

      case BinOpPhrase(op, lhs, rhs) =>
        op match {
          case BinOpPhrase.Op.GT | BinOpPhrase.Op.LT => exp"[$bool]"
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

      case c: Combinator[_] => c.t
    }).asInstanceOf[T]
  }
}

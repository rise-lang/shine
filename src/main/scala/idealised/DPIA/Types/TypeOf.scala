package idealised.DPIA.Types

import idealised.DPIA._
import idealised.DPIA.Phrases._
import idealised.SurfaceLanguage.Operators

import scala.language.{postfixOps, reflectiveCalls}

object TypeOf {
  def apply[T <: PhraseType](phrase: Phrase[T]): T = {
    (phrase match {
      case x: Identifier[T] => x.t
      case Lambda(x, p) => x.t -> p.t
      case Apply(p, q) =>
        assert(p.t.outT == q.t)
        p.t.outT

      case NatDependentLambda(a, p) => (a: NatIdentifier) -> p.t

      case TypeDependentLambda(a, p) => (a: DataTypeIdentifier) -> p.t

      case NatDependentApply(p, e) =>
        p.t.t `[` e `/` p.t.x `]`

      case TypeDependentApply(p, e) =>
        p.t.t `[` e `/` p.t.x `]`

      case Pair(p, q) => p.t x q.t

      case Proj1(p) => p.t.t1

      case Proj2(p) => p.t.t2

      case IfThenElse(_, thenP, elseP) =>
        assert(thenP.t == elseP.t)
        thenP.t

      case Literal(_, t) => t

      case UnaryOp(_, x) => x.t

      case BinOp(op, lhs, rhs) =>
        op match {
          case Operators.Binary.GT | Operators.Binary.LT => exp"[$bool]"
          case _ => (lhs.t.dataType, rhs.t.dataType) match {
            case (t1, t2) if t1 == t2 => ExpType(t1)
            // TODO: Think about this more thoroughly ...
            case (IndexType(n), `int`) =>
              ExpType(IndexType(n))
//              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(n, OperationalSemantics.evalIntExp(rhs))))
            case (`int`, IndexType(n)) =>
              ExpType(IndexType(n))
//              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(OperationalSemantics.evalIntExp(lhs), n)))
            case (IndexType(n), IndexType(_)) =>
//              ExpType(IndexType(OperationalSemantics.toScalaOp(op)(n, m)))
              ExpType(IndexType(n))
          }
        }

      case c: Primitive[_] => c.t
    }).asInstanceOf[T]
  }
}

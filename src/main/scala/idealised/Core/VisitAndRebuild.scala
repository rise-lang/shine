package idealised.Core

import idealised.Core.OperationalSemantics.IndexData

object VisitAndRebuild {

  class Visitor {
    def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = Continue(p, this)
    def apply(ae: Nat): Nat = ae
    def apply[T <: DataType](dt: T): T = dt

    abstract class Result[+T]
    case class Stop[T <: PhraseType](p: Phrase[T]) extends Result[Phrase[T]]
    case class Continue[T <: PhraseType](p: Phrase[T], v: Visitor) extends Result[Phrase[T]]
  }

  def apply[T <: PhraseType](phrase: Phrase[T], v: Visitor): Phrase[T] = {
    v(phrase) match {
      case r: v.Stop[T]@unchecked => r.p
      case c: v.Continue[T]@unchecked =>
        val v = c.v
        (c.p match {
          case i: Identifier[T] =>
            val t = i.t match {
              case ExpType(dt) => ExpType(v(dt))
              case AccType(dt) => AccType(v(dt))
              case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => VarType(v(dt1))
              case null => null
              case _ => throw new Exception("This should not happen")
            }
            Identifier(i.name, t)

          case Lambda(x, p) =>
            apply(x, v) match {
              case newX: Identifier[_] => Lambda(newX, apply(p, v))
              case _ => throw new Exception("This should not happen")
            }

          case Apply(p, q) =>
            Apply(apply(p, v), apply(q, v))

          case NatDependentLambda(a, p) =>
            NatDependentLambda(a, apply(p, v))

          case TypeDependentLambda(dt, p) =>
            TypeDependentLambda(dt, apply(p, v))

          case NatDependentApply(p, ae) =>
            NatDependentApply(apply(p, v), ae)

          case TypeDependentApply(p, dt) =>
            TypeDependentApply(apply(p, v), dt)

          case Pair(p, q) => Pair(apply(p, v), apply(q, v))

          case Proj1(p) => Proj1(apply(p, v))

          case Proj2(p) => Proj2(apply(p, v))

          case IfThenElse(cond, thenP, elseP) =>
            IfThenElse(apply(cond, v), apply(thenP, v), apply(elseP, v))

          case Literal(d, t) => d match {
            case IndexData(i) => Literal(IndexData(v(i)), v(t))
            case _ => Literal(d, v(t))
          }

          case UnaryOp(op, x) => UnaryOp(op, apply(x, v))

          case BinOp(op, lhs, rhs) => BinOp(op, apply(lhs, v), apply(rhs, v))

          case c: Primitive[T] => c.visitAndRebuild(v)
        }).asInstanceOf[Phrase[T]]
    }
  }

}

package Core

import Core.OperationalSemantics.IndexData

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
          case i: IdentPhrase[T] =>
            val t = i.t match {
              case ExpType(dt) => ExpType(v(dt))
              case AccType(dt) => AccType(v(dt))
              case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => VarType(v(dt1))
              case null => null
              case _ => throw new Exception("This should not happen")
            }
            IdentPhrase(i.name, t)

          case LambdaPhrase(x, p) =>
            apply(x, v) match {
              case newX: IdentPhrase[_] => LambdaPhrase(newX, apply(p, v))
              case _ => throw new Exception("This should not happen")
            }

          case ApplyPhrase(p, q) =>
            ApplyPhrase(apply(p, v), apply(q, v))

          case NatDependentLambdaPhrase(a, p) =>
            NatDependentLambdaPhrase(a, apply(p, v))

          case TypeDependentLambdaPhrase(dt, p) =>
            TypeDependentLambdaPhrase(dt, apply(p, v))

          case NatDependentApplyPhrase(p, ae) =>
            NatDependentApplyPhrase(apply(p, v), ae)

          case TypeDependentApplyPhrase(p, dt) =>
            TypeDependentApplyPhrase(apply(p, v), dt)

          case PairPhrase(p, q) => PairPhrase(apply(p, v), apply(q, v))

          case Proj1Phrase(p) => Proj1Phrase(apply(p, v))

          case Proj2Phrase(p) => Proj2Phrase(apply(p, v))

          case IfThenElsePhrase(cond, thenP, elseP) =>
            IfThenElsePhrase(apply(cond, v), apply(thenP, v), apply(elseP, v))

          case LiteralPhrase(d, t) => d match {
            case IndexData(i) => LiteralPhrase(IndexData(v(i)), v(t))
            case _ => LiteralPhrase(d, v(t))
          }

          case UnaryOpPhrase(op, x) => UnaryOpPhrase(op, apply(x, v))

          case BinOpPhrase(op, lhs, rhs) => BinOpPhrase(op, apply(lhs, v), apply(rhs, v))

          case c: Combinator[T] => c.visitAndRebuild(v)
        }).asInstanceOf[Phrase[T]]
    }
  }

}

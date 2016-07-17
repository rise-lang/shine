package Core

import Core.OperationalSemantics.IndexData

object VisitAndRebuild {

  class fun {
    def pre[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = Continue(p, this)
    def post[T <: PhraseType](p: Phrase[T]): Phrase[T] = p
    def apply(ae: Nat): Nat = ae
    def apply[T <: DataType](dt: T): T = dt

    abstract class Result[+T]
    case class Stop[T <: PhraseType](p: Phrase[T]) extends Result[Phrase[T]]
    case class Continue[T <: PhraseType](p: Phrase[T], f: fun) extends Result[Phrase[T]]
  }

  def apply[T <: PhraseType](phrase: Phrase[T], f: fun): Phrase[T] = {
    f.pre(phrase) match {
      case r: f.Stop[T]@unchecked => r.p
      case c: f.Continue[T]@unchecked =>
        val f = c.f
        f.post(c.p match {
          case i: IdentPhrase[T] =>
            val t = i.t match {
              case ExpType(dt) => ExpType(f(dt))
              case AccType(dt) => AccType(f(dt))
              case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => VarType(f(dt1))
              case null => null
              case _ => throw new Exception("This should not happen")
            }
            IdentPhrase(i.name, t)

          case LambdaPhrase(x, p) =>
            apply(x, f) match {
              case newX: IdentPhrase[_] => LambdaPhrase(newX, apply(p, f))
              case _ => throw new Exception("This should not happen")
            }

          case ApplyPhrase(p, q) =>
            ApplyPhrase(apply(p, f), apply(q, f))

          case NatDependentLambdaPhrase(a, p) =>
            NatDependentLambdaPhrase(a, apply(p, f))

          case TypeDependentLambdaPhrase(dt, p) =>
            TypeDependentLambdaPhrase(dt, apply(p, f))

          case NatDependentApplyPhrase(p, ae) =>
            NatDependentApplyPhrase(apply(p, f), ae)

          case TypeDependentApplyPhrase(p, dt) =>
            TypeDependentApplyPhrase(apply(p, f), dt)

          case PairPhrase(p, q) => PairPhrase(apply(p, f), apply(q, f))

          case Proj1Phrase(p) => Proj1Phrase(apply(p, f))

          case Proj2Phrase(p) => Proj2Phrase(apply(p, f))

          case IfThenElsePhrase(cond, thenP, elseP) =>
            IfThenElsePhrase(apply(cond, f), apply(thenP, f), apply(elseP, f))

          case LiteralPhrase(d, t) => d match {
            case IndexData(i) => LiteralPhrase(IndexData(f(i)), f(t))
            case _ => LiteralPhrase(d, f(t))
          }

          case UnaryOpPhrase(op, x) => UnaryOpPhrase(op, apply(x, f))

          case BinOpPhrase(op, lhs, rhs) => BinOpPhrase(op, apply(lhs, f), apply(rhs, f))

          case c: Combinator[T] => c.visitAndRebuild(f)
        }).asInstanceOf[Phrase[T]]
    }
  }

}

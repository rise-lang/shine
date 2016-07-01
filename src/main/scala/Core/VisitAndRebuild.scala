package Core

import apart.arithmetic.ArithExpr

object VisitAndRebuild {

  class fun {
    def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = Continue(p, this)
    def apply(ae: ArithExpr): ArithExpr = ae
    def apply[T <: DataType](dt: T): T = dt

    abstract class Result[+T]
    case class Stop[T <: PhraseType](p: Phrase[T]) extends Result[Phrase[T]]
    case class Continue[T <: PhraseType](p: Phrase[T], f: fun) extends Result[Phrase[T]]

  }

  def apply[T <: PhraseType](p: Phrase[T], f: fun): Phrase[T] = {
    f(p) match {
      case r: f.Stop[T]@unchecked => r.p
      case c: f.Continue[T]@unchecked =>
        val f = c.f
        (c.p match {
          case i: IdentPhrase[_] =>
            val t = i.t match {
              case ExpType(dt) => ExpType(f(dt))
              case AccType(dt) => AccType(f(dt))
              case PairType(ExpType(dt1), AccType(dt2)) if dt1 == dt2 => VarType(f(dt1))
              case null => null
              case _ => throw new Exception("This should not happen")
            }
            IdentPhrase(i.name, t)
          case l: LiteralPhrase => l
          case l: LambdaPhrase[_, _] =>
            val newParam = apply(l.param, f) match {
              case p: IdentPhrase[_] => p
              case _ => throw new Exception("This should not happen")
            }
            LambdaPhrase(newParam, apply(l.body, f))
          case l: NatDependentLambdaPhrase[_] => NatDependentLambdaPhrase(l.x, apply(l.body, f))
          case app: ApplyPhrase[a, T] => ApplyPhrase(apply(app.fun, f), apply(app.arg, f))
          case app: NatDependentApplyPhrase[T] => NatDependentApplyPhrase(apply(app.fun, f), app.arg)
          case pair: PairPhrase[_, _] => PairPhrase(apply(pair.fst, f), apply(pair.snd, f))
          case p: Proj1Phrase[T, b] => Proj1Phrase(apply(p.pair, f))
          case p: Proj2Phrase[a, T] => Proj2Phrase(apply(p.pair, f))
          case i: IfThenElsePhrase[T] => IfThenElsePhrase(apply(i.cond, f), apply(i.thenP, f), apply(i.elseP, f))
          case u: UnaryOpPhrase => UnaryOpPhrase(u.op, apply(u.p, f))
          case b: BinOpPhrase => BinOpPhrase(b.op, apply(b.lhs, f), apply(b.rhs, f))
          case e: ExpPattern => e.visitAndRebuild(f)
          case a: AccPattern => a.visitAndRebuild(f)
          case c: IntermediateCommandPattern => c.visitAndRebuild(f)
        }).asInstanceOf[Phrase[T]]
    }
  }

}

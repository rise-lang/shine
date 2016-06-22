package Core

object VisitAndRebuild {

  abstract class fun {
    def apply[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]]

    abstract class Result[+T]
    case class Replace[T <: PhraseType](p: Phrase[T]) extends Result[Phrase[T]]
    case class Continue(f: fun) extends Result[Nothing]

  }

  def apply[T <: PhraseType](p: Phrase[T], f: fun): Phrase[T] = {
    f(p) match {
      case r: f.Replace[T]@unchecked => r.p
      case c: f.Continue =>
        val f = c.f
        val res = (p match {
          case _: IdentPhrase[_] => p
          case _: LiteralPhrase => p
          case l: LambdaPhrase[_, _] => LambdaPhrase(l.param, apply(l.body, f))
          case app: ApplyPhrase[a, T] => ApplyPhrase(apply(app.fun, f), apply(app.arg, f))
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
        res.t = p.t
        res
    }
  }

}

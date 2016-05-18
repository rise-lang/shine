package Rewriting

import Core._
import ExpPatterns._
import AccPatterns._
import CommandPatterns._

object Rewrite {
  def rewrite[T <: PhraseType](p: Phrase[T]): Phrase[T] = {
    RewriteRules.rules.foreach( rule => {
      if (rule.rewrite.isDefinedAt(p)) { return rule(p) } // return on the first rule that fired
    } )
    p // return p if no rule fired
  }

  def apply[T <: PhraseType](p: Phrase[T]): Phrase[T] = {
    val res = (rewrite(p) match {
      case i: IdentPhrase[_]                  => i
      case l: LambdaPhrase[_, _]              => LambdaPhrase(l.param, apply(l.body))
      case app: ApplyPhrase[a, T]             => ApplyPhrase(apply(app.fun), apply(app.arg))
      case pair: PairPhrase[a, b]             => PairPhrase(apply(pair.fst), apply(pair.snd))
      case p: Proj1Phrase[T, b]               => Proj1Phrase(apply(p.pair))
      case p: Proj2Phrase[a, T]               => Proj1Phrase(apply(p.pair))
      case i: IfThenElsePhrase[T]             => IfThenElsePhrase(apply(i.cond), apply(i.thenP), apply(i.elseP))
      case l: LiteralPhrase                   => l
      case BinOpPhrase(op, lhs, rhs)          => BinOpPhrase(op, apply(lhs), apply(rhs))
      case ExpPatternPhrase(pattern)          =>
        ExpPatternPhrase(pattern match {
          case Record(fst, snd)               => Record(apply(fst), apply(snd))
          case Fst(record)                    => Fst(apply(record))
          case Snd(record)                    => Snd(apply(record))
          case Length(array)                  => Length(apply(array))
          case Idx(array, index)              => Idx(apply(array), apply(index))
          case _                              => pattern
        })
      case AccPatternPhrase(pattern)          =>
        AccPatternPhrase(pattern match {
          case RecordAcc(fst, snd)            => RecordAcc(apply(fst), apply(snd))
          case FstAcc(record)                 => FstAcc(record)
          case SndAcc(record)                 => SndAcc(record)
          case IdxAcc(array, index)           => IdxAcc(array, index)
          case _                              => pattern
        })
      case CommandPatternPhrase(pattern)      =>
        CommandPatternPhrase(pattern match {
          case s: Skip                        => s
          case Seq(c1, c2)                    => Seq(apply(c1), apply(c2))
          case New(f)                         => New(apply(f))
          case Assign(lhs, rhs)               => Assign(apply(lhs), apply(rhs))
          case For(n, body)                   => For(apply(n), apply(body))
          case _                              => pattern
        })
    }).asInstanceOf[Phrase[T]]
    res.t = p.t
    res
  }
}

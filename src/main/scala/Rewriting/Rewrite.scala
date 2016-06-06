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
      case UnaryOpPhrase(op, x)               => UnaryOpPhrase(op, apply(x))
      case BinOpPhrase(op, lhs, rhs)          => BinOpPhrase(op, apply(lhs), apply(rhs))
      case p: ExpPattern                      =>
        p match {
          case Record(fst, snd)               => Record(apply(fst), apply(snd))
          case Fst(record)                    => Fst(apply(record))
          case Snd(record)                    => Snd(apply(record))
          case Length(array)                  => Length(apply(array))
          case Idx(array, index)              => Idx(apply(array), apply(index))
          case _                              => p
        }
      case p: AccPattern                      =>
        p match {
          case RecordAcc(fst, snd)            => RecordAcc(apply(fst), apply(snd))
          case FstAcc(record)                 => FstAcc(record)
          case SndAcc(record)                 => SndAcc(record)
          case IdxAcc(array, index)           => IdxAcc(array, index)
          case _                              => p
        }
      case p: CommandPattern                  =>
        p match {
          case s: Skip                        => s
          case Seq(c1, c2)                    => Seq(apply(c1), apply(c2))
          case New(dt, as, f)                 => New(dt, as, apply(f))
          case Assign(lhs, rhs)               => Assign(apply(lhs), apply(rhs))
          case For(n, body)                   => For(apply(n), apply(body))
          case _                              => p
        }
    }).asInstanceOf[Phrase[T]]
    res.t = p.t
    res
  }
}

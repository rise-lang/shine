package Rewriting

import Core._

object Rewrite {
  def rewrite[T <: PhraseType](p: Phrase[T]): Phrase[T] = {
    RewriteRules.rules.foreach( rule => {
      if (rule.rewrite.isDefinedAt(p)) { return rule(p) } // return on the first rule that fired
    } )
    p // return p if no rule fired
  }

  def apply[T <: PhraseType](p: Phrase[T]): Phrase[T] = {
    val res = (rewrite(p) match {
      case i: IdentPhrase[_]                        => i
      case l: LambdaPhrase[_, _]                    => LambdaPhrase(l.param, apply(l.body))
      case app: ApplyPhrase[a, T]                   => ApplyPhrase(apply(app.fun), apply(app.arg))
      case pair: PairPhrase[a, b]                   => PairPhrase(apply(pair.fst), apply(pair.snd))
      case p: Proj1Phrase[T, b]                     => Proj1Phrase(apply(p.pair))
      case p: Proj2Phrase[a, T]                     => Proj1Phrase(apply(p.pair))
      case RecordExpPhase(fields@_*)                  => RecordExpPhase(fields.map(apply):_*)
      case FieldAccessExpPhrase(n, record)             => FieldAccessExpPhrase(n, apply(record))
      case LengthPhrase(array)                => LengthPhrase(apply(array))
      case ArrayExpAccessPhrase(array, index) => ArrayExpAccessPhrase(apply(array), apply(index))
      case ArrayAccAccessPhrase(array, index) => ArrayAccAccessPhrase(apply(array), apply(index))
      case s: SkipPhrase                      => s
      case SeqPhrase(c1, c2)                        => SeqPhrase(apply(c1), apply(c2))
      case NewPhrase(f)                       => NewPhrase(apply(f))
      case AssignPhrase(lhs, rhs)                   => AssignPhrase(apply(lhs), apply(rhs))
      case i: IfThenElsePhrase[T]                   => IfThenElsePhrase(apply(i.cond), apply(i.thenP), apply(i.elseP))
      case ForPhrase(n, body)                 => ForPhrase(apply(n), apply(body))
      case l: LiteralPhrase                         => l
      case BinOpPhrase(op, lhs, rhs)                => BinOpPhrase(op, apply(lhs), apply(rhs))
      case ExpPatternPhrase(pattern)          => ExpPatternPhrase(pattern)
      case CommandPatternPhrase(pattern)      => CommandPatternPhrase(pattern)
    }).asInstanceOf[Phrase[T]]
    res.t = p.t
    res
  }
}
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
      case i: Ident[_]                        => i
      case l: Lambda[_, _]                    => Lambda(l.param, apply(l.body))
      case app: Apply[a, T]                   => Apply(apply(app.fun), apply(app.arg))
      case pair: Pair[a, b]                   => Pair(apply(pair.fst), apply(pair.snd))
      case p: Proj1[T, b]                     => Proj1(apply(p.pair))
      case p: Proj2[a, T]                     => Proj1(apply(p.pair))
      case Record(fields@_*)                  => Record(fields.map(apply):_*)
      case FieldAccess(n, record)             => FieldAccess(n, apply(record))
      case LengthPhrase(array)                => LengthPhrase(apply(array))
      case ArrayExpAccessPhrase(array, index) => ArrayExpAccessPhrase(apply(array), apply(index))
      case ArrayAccAccessPhrase(array, index) => ArrayAccAccessPhrase(apply(array), apply(index))
      case s: SkipPhrase                      => s
      case Seq(c1, c2)                        => Seq(apply(c1), apply(c2))
      case NewPhrase(f)                       => NewPhrase(apply(f))
      case Assign(lhs, rhs)                   => Assign(apply(lhs), apply(rhs))
      case i: IfThenElse[T]                   => IfThenElse(apply(i.cond), apply(i.thenP), apply(i.elseP))
      case ForPhrase(n, body)                 => ForPhrase(apply(n), apply(body))
      case l: Literal                         => l
      case BinOp(op, lhs, rhs)                => BinOp(op, apply(lhs), apply(rhs))
      case ExpPatternPhrase(pattern)          => ExpPatternPhrase(pattern)
      case CommandPatternPhrase(pattern)      => CommandPatternPhrase(pattern)
    }).asInstanceOf[Phrase[T]]
    res.t = p.t
    res
  }
}
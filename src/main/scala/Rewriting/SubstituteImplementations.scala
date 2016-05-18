package Rewriting

import Core._
import CommandPatterns._
import Core.PhraseType.->

object SubstituteImplementations {

  def apply(p: Phrase[CommandType]): Phrase[CommandType] = {
    p match {
      case CommandPatternPhrase(pattern) => pattern match {
        case a: Assign      => a.asPhrase
        case For(n, body)   => For(n, applyFun(body)).asPhrase
        case m: MapI        => apply(m.impl)
        case New(f)         => New(applyFun(f)).asPhrase
        case r: ReduceIAcc  => apply(r.impl)
        case r: ReduceIExp  => apply(r.impl)
        case Seq(c1, c2)    => Seq(apply(c1), apply(c2)).asPhrase
        case s: Skip        => s.asPhrase
      }
      case ApplyPhrase(fun, arg) => ApplyPhrase(applyFun(fun), arg)
      case _: IdentPhrase[CommandType]       => p
      case _: IfThenElsePhrase[CommandType]  => p
      case _: Proj1Phrase[CommandType, _]    => p
      case _: Proj2Phrase[_, CommandType]    => p
    }
  }

  private def applyFun[T <: PhraseType](p: Phrase[T -> CommandType]): Phrase[T -> CommandType] = {
    p match {
      case LambdaPhrase(param, body) =>
        LambdaPhrase(param, apply(body))
      case _ => throw new Exception("This should never happen")
    }
  }

}

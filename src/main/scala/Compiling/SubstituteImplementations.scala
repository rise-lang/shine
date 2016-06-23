package Compiling

import Core._
import Core.PhraseType.{->, `(nat)->`}

object SubstituteImplementations {

  def apply(p: Phrase[CommandType]): Phrase[CommandType] = {
    p match {
      case p : IntermediateCommandPattern   => p.substituteImpl
      case ApplyPhrase(fun, arg)            => ApplyPhrase(applyFun(fun), arg)
      case NatDependentApplyPhrase(fun,arg) => NatDependentApplyPhrase(applyNatDependentFun(fun), arg)
      case _: IdentPhrase[CommandType]      => p
      case _: IfThenElsePhrase[CommandType] => p
      case _: Proj1Phrase[CommandType, _]   => p
      case _: Proj2Phrase[_, CommandType]   => p
    }
  }

  def applyFun[T <: PhraseType](p: Phrase[T -> CommandType]): Phrase[T -> CommandType] = {
    p match {
      case LambdaPhrase(param, body) =>
        LambdaPhrase(param, apply(body))
      case _ => throw new Exception("This should never happen")
    }
  }

  def applyNatDependentFun(p: Phrase[`(nat)->`[CommandType]]): Phrase[`(nat)->`[CommandType]] = {
    p match {
      case NatDependentLambdaPhrase(x, body) =>
        NatDependentLambdaPhrase(x, apply(body))
      case _ => throw new Exception("This should neve happen")
    }
  }


  def applyBinaryFun[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> (T2 -> CommandType)]): Phrase[T1 -> (T2 -> CommandType)] = {
    p match {
      case LambdaPhrase(p1, body) =>
        LambdaPhrase(p1, applyFun(body))
      case _ => throw new Exception("This should never happen")
    }
  }

}

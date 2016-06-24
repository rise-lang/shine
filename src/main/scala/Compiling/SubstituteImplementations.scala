package Compiling

import Core._
import Core.PhraseType.{->, `(nat)->`}

import scala.collection.mutable


object SubstituteImplementations {

  class Environment {
    val addressspace = mutable.Map[String, AddressSpace]()
  }

  def apply(p: Phrase[CommandType], env: Environment): Phrase[CommandType] = {
    p match {
      case p : IntermediateCommandPattern   => p.substituteImpl(env)
      case ApplyPhrase(fun, arg)            => ApplyPhrase(applyFun(fun, env), arg)
      case NatDependentApplyPhrase(fun,arg) => NatDependentApplyPhrase(applyNatDependentFun(fun, env), arg)
      case _: IdentPhrase[CommandType]      => p
      case _: IfThenElsePhrase[CommandType] => p
      case _: Proj1Phrase[CommandType, _]   => p
      case _: Proj2Phrase[_, CommandType]   => p
    }
  }

  def applyFun[T <: PhraseType](p: Phrase[T -> CommandType],
                                env: Environment): Phrase[T -> CommandType] = {
    p match {
      case LambdaPhrase(param, body) =>
        LambdaPhrase(param, apply(body, env))
      case _ => throw new Exception("This should never happen")
    }
  }

  def applyNatDependentFun(p: Phrase[`(nat)->`[CommandType]],
                           env: Environment): Phrase[`(nat)->`[CommandType]] = {
    p match {
      case NatDependentLambdaPhrase(x, body) =>
        NatDependentLambdaPhrase(x, apply(body, env))
      case _ => throw new Exception("This should neve happen")
    }
  }


  def applyBinaryFun[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> (T2 -> CommandType)],
                                                         env: Environment): Phrase[T1 -> (T2 -> CommandType)] = {
    p match {
      case LambdaPhrase(p1, body) =>
        LambdaPhrase(p1, applyFun(body, env))
      case _ => throw new Exception("This should never happen")
    }
  }

  def applyNatDependentBinaryFun[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[`(nat)->`[T1 -> (T2 -> CommandType)]],
                                                                     env: Environment): Phrase[`(nat)->`[T1 -> (T2 -> CommandType)]] = {
    p match {
      case NatDependentLambdaPhrase(x, body) =>
        NatDependentLambdaPhrase(x, applyBinaryFun(body, env))
      case _ => throw new Exception("This should never happen")
    }
  }

}

package Core

import PhraseType._

// TODO: Discuss with Bob: this excludes if (as the condition needs to be properly evaluated)
object Lift {

  def liftFunction[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
    p match {
      case l: LambdaPhrase[T1, T2] => (arg: Phrase[T1]) => OperationalSemantics.substitute(arg, `for` = l.param, in = l.body)
      case app: ApplyPhrase[a, T1 -> T2] =>
        val fun = liftFunction(app.fun)
        liftFunction(fun(app.arg))
      case p1: Proj1Phrase[T1 -> T2, b] =>
        val pair = liftPair(p1.pair)
        liftFunction(pair._1)
      case p2: Proj2Phrase[a, T1 -> T2] =>
        val pair = liftPair(p2.pair)
        liftFunction(pair._2)
      case IdentPhrase(_) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftPair[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
    p match {
      case i: IdentPhrase[T1 x T2] => (IdentPhrase[T1](i.name), IdentPhrase[T2](i.name))
      case pair: PairPhrase[T1, T2] => (pair.fst, pair.snd)
      case app: ApplyPhrase[a, T1 x T2] =>
        val fun = liftFunction(app.fun)
        liftPair(fun(app.arg))
      case p1: Proj1Phrase[T1 x T2, b] =>
        val pair = liftPair(p1.pair)
        liftPair(pair._1)
      case p2: Proj2Phrase[a, T1 x T2] =>
        val pair = liftPair(p2.pair)
        liftPair(pair._2)
      case IdentPhrase(_) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

}
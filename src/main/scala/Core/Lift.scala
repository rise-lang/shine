package Core

import apart.arithmetic.ArithExpr
import scala.language.postfixOps
import scala.language.reflectiveCalls

// TODO: Discuss with Bob: this excludes if (as the condition needs to be properly evaluated)
object Lift {

  def liftNatDependentFunction[T <: PhraseType](p: Phrase[`(nat)->`[T]]): (ArithExpr => Phrase[T]) = {
    p match {
      case l: NatDependentLambdaPhrase[T] =>
        (arg: ArithExpr) => l.body `[` arg `/` l.x `]`
      case app: ApplyPhrase[a, `(nat)->`[T]] =>
        val fun = liftFunction(app.fun)
        liftNatDependentFunction(fun(app.arg))
      case app: NatDependentApplyPhrase[`(nat)->`[T]] =>
        val fun = liftNatDependentFunction(app.fun)
        liftNatDependentFunction(fun(app.arg))
      case p1: Proj1Phrase[`(nat)->`[T], b] =>
        val pair = liftPair(p1.pair)
        liftNatDependentFunction(pair._1)
      case p2: Proj2Phrase[a, `(nat)->`[T]] =>
        val pair = liftPair(p2.pair)
        liftNatDependentFunction(pair._2)
      case IdentPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftFunction[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
    p match {
      case l: LambdaPhrase[T1, T2] =>
        (arg: Phrase[T1]) => l.body `[` arg  `/` l.param `]`
      case app: ApplyPhrase[a, T1 -> T2] =>
        val fun = liftFunction(app.fun)
        liftFunction(fun(app.arg))
      case app: NatDependentApplyPhrase[T1 -> T2] =>
        val fun = liftNatDependentFunction(app.fun)
        liftFunction(fun(app.arg))
      case p1: Proj1Phrase[T1 -> T2, b] =>
        val pair = liftPair(p1.pair)
        liftFunction(pair._1)
      case p2: Proj2Phrase[a, T1 -> T2] =>
        val pair = liftPair(p2.pair)
        liftFunction(pair._2)
      case IdentPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftPair[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
    p match {
      case i: IdentPhrase[T1 x T2] =>
        if (i.t == null) {
          val t1 = null.asInstanceOf[T1]
          val t2 = null.asInstanceOf[T2]
          (IdentPhrase[T1](i.name, t1), IdentPhrase[T2](i.name, t2))
        } else {
          (IdentPhrase[T1](i.name, i.t.t1), IdentPhrase[T2](i.name, i.t.t2))
        }
      case pair: PairPhrase[T1, T2] => (pair.fst, pair.snd)
      case app: ApplyPhrase[a, T1 x T2] =>
        val fun = liftFunction(app.fun)
        liftPair(fun(app.arg))
      case app: NatDependentApplyPhrase[T1 x T2] =>
        val fun = liftNatDependentFunction(app.fun)
        liftPair(fun(app.arg))
      case p1: Proj1Phrase[T1 x T2, b] =>
        val pair = liftPair(p1.pair)
        liftPair(pair._1)
      case p2: Proj2Phrase[a, T1 x T2] =>
        val pair = liftPair(p2.pair)
        liftPair(pair._2)
      case IdentPhrase(_, _) | IfThenElsePhrase(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

}
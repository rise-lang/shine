package idealised.DPIA

import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{ExpType, Kind, PhraseType}

import scala.language.{postfixOps, reflectiveCalls}

object Lifting {
  import lift.core.lifting.{Expanding, Reducing, Result}

  def liftDependentFunction[K <: Kind, T <: PhraseType](p: Phrase[K `()->:` T]): K#T => Phrase[T] = {
    p match {
      case l: DepLambda[K, T] =>
        (arg: K#T) => PhraseType.substitute(arg, `for`=l.x, in=l.body)
      case app: Apply[_, K `()->:` T] =>
        val fun = liftFunction(app.fun).reducing
        liftDependentFunction(fun(app.arg))
      case DepApply(f, arg) =>
        val fun = liftDependentFunction(f)
        liftDependentFunction(fun(arg))
      case p1: Proj1[K `()->:` T, b] =>
        val pair = liftPair(p1.pair)
        liftDependentFunction(pair._1)
      case p2: Proj2[a, K `()->:` T] =>
        val pair = liftPair(p2.pair)
        liftDependentFunction(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) | LetNat(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftFunction[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 ->: T2]): Result[Phrase[T1] => Phrase[T2]] = {
    def chain[T3 <: PhraseType, T4 <: PhraseType](r: Result[Phrase[T3 ->: T4]]): Result[Phrase[T3] => Phrase[T4]] =
      r.bind(liftFunction, f => Expanding((a: Phrase[T3]) => Apply(f, a)))

    p match {
      case l: Lambda[T1, T2] =>
        Reducing((arg: Phrase[T1]) => l.body `[` arg  `/` l.param `]`)
      case app: Apply[_, T1 ->: T2] =>
        chain(liftFunction(app.fun).map(lf => lf(app.arg)))
      case DepApply(f, arg) =>
        val fun = liftDependentFunction(f)
        liftFunction(fun(arg))
      case p1: Proj1[T1 ->: T2, b] =>
        val pair = liftPair(p1.pair)
        liftFunction(pair._1)
      case p2: Proj2[a, T1 ->: T2] =>
        val pair = liftPair(p2.pair)
        liftFunction(pair._2)
      case _ => chain(Expanding(p))
    }
  }

  @scala.annotation.tailrec
  def liftFunctionToNatLambda[T <: PhraseType](p: Phrase[ExpType ->: T]): Nat => Phrase[T] = {
    p match {
      case l: Lambda[ExpType, T] =>
        (arg: Nat) => l.body `[` arg  `/` NatIdentifier(l.param.name) `]`
      case app: Apply[_, ExpType ->: T] =>
        val fun = liftFunction(app.fun).reducing
        liftFunctionToNatLambda(fun(app.arg))
      case DepApply(f, arg) =>
        val fun = liftDependentFunction(f)
        liftFunctionToNatLambda(fun(arg))
      case p1: Proj1[ExpType ->: T, b] =>
        val pair = liftPair(p1.pair)
        liftFunctionToNatLambda(pair._1)
      case p2: Proj2[a, ExpType ->: T] =>
        val pair = liftPair(p2.pair)
        liftFunctionToNatLambda(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) | LetNat(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }


  def liftPair[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
    p match {
      case i: Identifier[T1 x T2] =>
        (Identifier[T1](i.name + "_1", i.t.t1), Identifier[T2](i.name + "_2", i.t.t2))
      case pair: PhrasePair[T1, T2] => (pair.fst, pair.snd)
      case app: Apply[_, T1 x T2] =>
        val fun = liftFunction(app.fun).reducing
        liftPair(fun(app.arg))
      case DepApply(f, arg) =>
        val fun = liftDependentFunction(f)
        liftPair(fun(arg))
      case p1: Proj1[T1 x T2, b] =>
        val pair = liftPair(p1.pair)
        liftPair(pair._1)
      case p2: Proj2[a, T1 x T2] =>
        val pair = liftPair(p2.pair)
        liftPair(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) | LetNat(_, _, _)=>
        throw new Exception("This should never happen")
    }
  }
}
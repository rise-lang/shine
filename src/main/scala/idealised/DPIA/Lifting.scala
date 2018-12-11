package idealised.DPIA

import idealised.DPIA.Phrases._
import idealised.DPIA.Types.{DataType, ExpType, PhraseType}
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage._
import lift.arithmetic.NamedVar

import scala.language.{postfixOps, reflectiveCalls}

// TODO: Discuss with Bob: this excludes if (as the condition needs to be properly evaluated)
object Lifting {

  def liftNatDependentFunction[T <: PhraseType](p: Phrase[`(nat)->`[T]]): (Nat => Phrase[T]) = {
    p match {
      case l: NatDependentLambda[T] =>
        (arg: Nat) => l.body `[` arg `/` l.x `]`
      case app: Apply[_, `(nat)->`[T]] =>
        val fun = liftFunction(app.fun)
        liftNatDependentFunction(fun(app.arg))
      case app: NatDependentApply[`(nat)->`[T]] =>
        val fun = liftNatDependentFunction(app.fun)
        liftNatDependentFunction(fun(app.arg))
      case app: TypeDependentApply[`(nat)->`[T]] =>
        val fun = liftTypeDependentFunction(app.fun)
        liftNatDependentFunction(fun(app.arg))
      case p1: Proj1[`(nat)->`[T], b] =>
        val pair = liftPair(p1.pair)
        liftNatDependentFunction(pair._1)
      case p2: Proj2[a, `(nat)->`[T]] =>
        val pair = liftPair(p2.pair)
        liftNatDependentFunction(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftTypeDependentFunction[T <: PhraseType](p: Phrase[`(dt)->`[T]]): (DataType => Phrase[T]) = {
    p match {
      case l: TypeDependentLambda[T] =>
        (arg: DataType) => l.body `[` arg `/` l.x `]`
      case app: Apply[_, `(dt)->`[T]] =>
        val fun = liftFunction(app.fun)
        liftTypeDependentFunction(fun(app.arg))
      case app: NatDependentApply[`(dt)->`[T]] =>
        val fun = liftNatDependentFunction(app.fun)
        liftTypeDependentFunction(fun(app.arg))
      case app: TypeDependentApply[`(dt)->`[T]] =>
        val fun = liftTypeDependentFunction(app.fun)
        liftTypeDependentFunction(fun(app.arg))
      case p1: Proj1[`(dt)->`[T], b] =>
        val pair = liftPair(p1.pair)
        liftTypeDependentFunction(pair._1)
      case p2: Proj2[a, `(dt)->`[T]] =>
        val pair = liftPair(p2.pair)
        liftTypeDependentFunction(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftFunction[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 -> T2]): (Phrase[T1] => Phrase[T2]) = {
    p match {
      case l: Lambda[T1, T2] =>
        (arg: Phrase[T1]) =>l.body `[` arg  `/` l.param `]`
      case app: Apply[_, T1 -> T2] =>
        val fun = liftFunction(app.fun)
        liftFunction(fun(app.arg))
      case app: NatDependentApply[T1 -> T2] =>
        val fun = liftNatDependentFunction(app.fun)
        liftFunction(fun(app.arg))
      case app: TypeDependentApply[T1 -> T2] =>
        val fun = liftTypeDependentFunction(app.fun)
        liftFunction(fun(app.arg))
      case p1: Proj1[T1 -> T2, b] =>
        val pair = liftPair(p1.pair)
        liftFunction(pair._1)
      case p2: Proj2[a, T1 -> T2] =>
        val pair = liftPair(p2.pair)
        liftFunction(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

  def liftFunctionToNatLambda[T <: PhraseType](p: Phrase[ExpType -> T]): (Nat => Phrase[T]) = {
    p match {
      case l: Lambda[ExpType, T] =>
        (arg: Nat) => l.body `[` arg  `/` NamedVar(l.param.name) `]`
      case app: Apply[_, ExpType -> T] =>
        val fun = liftFunction(app.fun)
        liftFunctionToNatLambda(fun(app.arg))
      case app: NatDependentApply[ExpType -> T] =>
        val fun = liftNatDependentFunction(app.fun)
        liftFunctionToNatLambda(fun(app.arg))
      case app: TypeDependentApply[ExpType -> T] =>
        val fun = liftTypeDependentFunction(app.fun)
        liftFunctionToNatLambda(fun(app.arg))
      case p1: Proj1[ExpType -> T, b] =>
        val pair = liftPair(p1.pair)
        liftFunctionToNatLambda(pair._1)
      case p2: Proj2[a, ExpType -> T] =>
        val pair = liftPair(p2.pair)
        liftFunctionToNatLambda(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }


  def liftPair[T1 <: PhraseType, T2 <: PhraseType](p: Phrase[T1 x T2]): (Phrase[T1], Phrase[T2]) = {
    p match {
      case i: Identifier[T1 x T2] =>
        (Identifier[T1](i.name + "_1", i.t.t1), Identifier[T2](i.name + "_2", i.t.t2))
      case pair: Pair[T1, T2] => (pair.fst, pair.snd)
      case app: Apply[_, T1 x T2] =>
        val fun = liftFunction(app.fun)
        liftPair(fun(app.arg))
      case app: NatDependentApply[T1 x T2] =>
        val fun = liftNatDependentFunction(app.fun)
        liftPair(fun(app.arg))
      case app: TypeDependentApply[T1 x T2] =>
        val fun = liftTypeDependentFunction(app.fun)
        liftPair(fun(app.arg))
      case p1: Proj1[T1 x T2, b] =>
        val pair = liftPair(p1.pair)
        liftPair(pair._1)
      case p2: Proj2[a, T1 x T2] =>
        val pair = liftPair(p2.pair)
        liftPair(pair._2)
      case Identifier(_, _) | IfThenElse(_, _, _) =>
        throw new Exception("This should never happen")
    }
  }

}
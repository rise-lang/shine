package shine.DPIA.Compilation

import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.MakePair

import scala.language.reflectiveCalls

object TranslationToImperative {
  def apply(p: Phrase[ExpType])
           (implicit context: TranslationContext): Phrase[CommType] = {
    val outT = p.t
    val out = identifier("output", AccType(outT.dataType))
    acc(p)(out)
  }

  def acc(E: Phrase[ExpType])
         (A: Phrase[AccType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    AcceptorTranslation.acc(E)(A)
  }

  def con(E: Phrase[ExpType])
         (C: Phrase[ExpType ->: CommType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    ContinuationTranslation.con(E)(C)
  }

  def fedAcc(env: Map[Identifier[ExpType], Identifier[AccType]])
            (E: Phrase[ExpType])
            (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    E match {
      case ep: ExpPrimitive with FedeT => ep.fedeTranslation(env)(C)
      case ep: ExpPrimitive => throw new Exception(s"$ep does not support the Fede Translation")
      case x: Identifier[ExpType] =>
        env.get(x) match {
          case Some(o) => C(o)
          case None => ???
        }

      // on the fly beta-reduction
      case Apply(fun, arg) => fedAcc(env)(
        Lifting.liftFunction(fun).reducing(arg))(C)
      case DepApply(fun, arg) => arg match {
        case a: Nat => fedAcc(env)(
          Lifting.liftDependentFunction[NatKind, ExpType](
            fun.asInstanceOf[Phrase[NatKind `()->:` ExpType]])(a))(C)
        case a: DataType => fedAcc(env)(
          Lifting.liftDependentFunction[DataKind, ExpType](
            fun.asInstanceOf[Phrase[DataKind `()->:` ExpType]])(a))(C)
      }

      case IfThenElse(cond, thenP, elseP) => ???

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")

      case LetNat(_, _, _) => throw new Exception("This should never happen")
      case _ => ???
    }
  }

  def str(E: Phrase[ExpType])
    (C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
    (implicit context: TranslationContext)
  : Phrase[CommType] = {
    E match {
      case ep: ExpPrimitive with StreamT => ep.streamTranslation(C)
      case ep: ExpPrimitive => Compilation.TranslationToImperative.translateArrayToStream(ep, C)

      // on the fly beta-reduction
      case Apply(fun, arg) => str(Lifting.liftFunction(fun).reducing(arg))(C)
      case DepApply(fun, arg) => arg match {
        case a: Nat => str(
          Lifting.liftDependentFunction[NatKind, ExpType](
          fun.asInstanceOf[Phrase[NatKind `()->:` ExpType]])(a)
        )(C)
        case a: DataType => str(
          Lifting.liftDependentFunction[DataKind, ExpType](
            fun.asInstanceOf[Phrase[DataKind `()->:` ExpType]])(a)
        )(C)
      }

      case _ => translateArrayToStream(E, C)
    }
  }

  // TODO: works for arrays, but not streams
  def translateArrayToStream(
    E: Phrase[ExpType],
    C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.DSL._

    E.t match {
      case ExpType(ArrayType(n, dt), read) =>
        C(nFun(i => fun(expT(dt, read) ->: (comm: CommType))(k =>
          con(E `@` i)(k)
        ), arithexpr.arithmetic.RangeAdd(0, n, 1)))
      case _ => throw new Exception("this should not happen")
    }
  }
}

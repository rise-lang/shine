package shine.DPIA.Compilation

import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.Pair

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
    E match {
      // on the fly beta-reduction
      case Apply(fun, arg) => acc(Lifting.liftFunction(fun).reducing(arg))(A)
      case DepApply(fun, arg) => arg match {
        case a: Nat =>
          acc(Lifting.liftDependentFunction[NatKind, ExpType](
            fun.asInstanceOf[ Phrase[NatKind `()->:` ExpType]])(a))(A)
        case a: DataType =>
          acc(Lifting.liftDependentFunction[DataKind, ExpType](
            fun.asInstanceOf[Phrase[DataKind `()->:` ExpType]])(a))(A)
      }

      case e
        if TypeCheck.notContainingArrayType(e.t.dataType)
          && e.t.accessType == read =>
        //FIXME
        // The pattern matching is needed in order to generate separate
        // assignments to elements of pairs (structs), because the AMD SDK
        // cannot deal with literal struct assignments or definitions (C99).
        e match {
          case Pair(dt1, dt2, _, fst, snd) =>
            acc(fst)(pairAcc1(dt1, dt2, A)) `;`
              acc(snd)(pairAcc2(dt1, dt2, A))
          case _ =>
            con(e)(λ(e.t)(a => A :=| e.t.dataType | a))
        }

      case c: Literal => A :=|c.t.dataType| c

      case x: Identifier[ExpType] => A :=|x.t.dataType| x

      case n: Natural => A :=|n.t.dataType| n

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          A :=|u.t.dataType| UnaryOp(op, x)
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            A :=|b.t.dataType| BinOp(op, x, y)
          ))
        ))

      case ep: ExpPrimitive => ep.acceptorTranslation(A)

      case LetNat(binder, defn, body) => LetNat(binder, defn, acc(body)(A))

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if` (x) `then` acc(thenP)(A) `else` acc(elseP)(A)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")
    }
  }

  def fedAcc(env: Map[Identifier[ExpType], Identifier[AccType]])
            (E: Phrase[ExpType])
            (C: Phrase[AccType ->: AccType]) : Phrase[AccType] = {
    E match {
      case ep: ExpPrimitive => ep.fedeTranslation(env)(C)
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

  def con(E: Phrase[ExpType])
         (C: Phrase[ExpType ->: CommType])
         (implicit context: TranslationContext): Phrase[CommType] = {
    E match {
      case x: Identifier[ExpType] => C(x)

      case c: Literal => C(c)

      case n: Natural => C(n)

      case u@UnaryOp(op, e) =>
        con(e)(λ(u.t)(x =>
          C(UnaryOp(op, x))
        ))

      case b@BinOp(op, e1, e2) =>
        con(e1)(λ(b.t)(x =>
          con(e2)(λ(b.t)(y =>
            C(BinOp(op, x, y))
          ))
        ))

      case ep: ExpPrimitive => ep.continuationTranslation(C)

      // on the fly beta-reduction
      case Apply(fun, arg) => con(Lifting.liftFunction(fun).reducing(arg))(C)
      case DepApply(fun, arg) => arg match {
        case a: Nat =>
          con(Lifting.liftDependentFunction[NatKind, ExpType](
            fun.asInstanceOf[Phrase[NatKind `()->:` ExpType]])(a))(C)
        case a: DataType =>
          con(Lifting.liftDependentFunction[DataKind, ExpType](
            fun.asInstanceOf[Phrase[DataKind `()->:` ExpType]])(a))(C)
      }

      case IfThenElse(cond, thenP, elseP) =>
        con(cond)(λ(cond.t) { x =>
          `if`(x) `then` con(thenP)(C) `else` con(elseP)(C)
        })

      case Proj1(_) => throw new Exception("This should never happen")
      case Proj2(_) => throw new Exception("This should never happen")

      case LetNat(_, _, _) => throw new Exception("This should never happen")
    }
  }

  def str(E: Phrase[ExpType])
    (C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
    (implicit context: TranslationContext)
  : Phrase[CommType] = {
    E match {
      case ep: ExpPrimitive => ep.streamTranslation(C)

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

package shine.DPIA

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.primitives.imperative._
import shine.DPIA.Phrases.{Operators, _}
import shine.DPIA.Semantics.OperationalSemantics.{FloatData, IntData}
import shine.DPIA.Types._
import shine.DPIA.primitives.functional.{DepIdx, Idx, IdxVec, NatAsIndex}

import scala.language.implicitConversions

package object DSL {

  implicit class BinOps(lhs: Phrase[ExpType]) {
    def +(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.ADD, lhs, rhs)
    def -(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.SUB, lhs, rhs)
    def *(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.MUL, lhs, rhs)
    def /(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.DIV, lhs, rhs)
    def %(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.MOD, lhs, rhs)
    def >(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.GT, lhs, rhs)
    def <(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.LT, lhs, rhs)
    def =:=(rhs: Phrase[ExpType]) = BinOp(Operators.Binary.EQ, lhs, rhs)
    def unary_- = UnaryOp(Operators.Unary.NEG, lhs)
  }

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def `@`(index: Phrase[ExpType]): Idx = (index.t, e.t) match {
      case (ExpType(IndexType(n1), _), ExpType(ArrayType(n2, dt), _)) if n1 == n2 =>
        Idx(n1, dt, index, e)
      case x => error(x.toString, "(exp[idx(n), _], exp[n.dt, _])")
    }

    def `@`(index: Nat): Idx = e.t match {
      case ExpType(ArrayType(n, dt), _) =>
        Idx(n, dt, NatAsIndex(n, Natural(index)), e)
      case x => error(x.toString, "exp[n.dt, _]")
    }


    def `@v`(index: Phrase[ExpType]): IdxVec = (index.t, e.t) match {
      case (ExpType(IndexType(n1), _), ExpType(VectorType(n2, st), _)) if n1 == n2 =>
        IdxVec(n1, st, index, e)
      case x => error(x.toString, "(exp[idx(n), _], exp[st<n>, _])")
    }

    def `@d`(index: Nat):DepIdx = e.t match {
      case ExpType(depArray:DepArrayType, _) => DepIdx(depArray.size, depArray.elemFType, index, e)
      case x => error(x.toString, "exp[n.(i:Nat) -> dt, _]")
    }
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]): IdxAcc = (index.t, a.t) match {
      case (ExpType(IndexType(n1), _), AccType(ArrayType(n2, dt))) if n1 == n2 =>
        IdxAcc(n1, dt, index, a)
      case x => error(x.toString, "(exp[idx(n), _], acc[n.dt])")
    }

    def `@`(index: Nat): IdxAcc = a.t match {
      case AccType(ArrayType(n, dt)) =>
        IdxAcc(n, dt, NatAsIndex(n, Natural(index)), a)
      case x => error(x.toString, "acc[n.dt]")
    }

    def `@v`(index: Phrase[ExpType]): IdxVecAcc = (index.t, a.t) match {
      case (ExpType(IndexType(n1), _), AccType(VectorType(n2, st))) if n1 == n2 =>
        IdxVecAcc(n1, st, index, a)
      case x => error(x.toString, "(exp[idx(n), _], acc[n.dt])")
    }

    def `@d`(index: Nat):DepIdxAcc = a.t match {
      case AccType(depAT:DepArrayType) => DepIdxAcc(depAT.size, depAT.elemFType, index, a)
      case x => error(x.toString, "acc[n.(i:Nat) -> dt]")
    }
  }

  //noinspection TypeAnnotation
  implicit class AssignmentHelper(lhs: Phrase[AccType]) {
    def :=|(dt: DataType) = new {
      def |(rhs: Phrase[ExpType])
           (implicit context: TranslationContext): Phrase[CommType] = {
        context.assign(dt, lhs, rhs)
      }
    }
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 ->: T2]) {
    def apply(arg: Phrase[T1]): Phrase[T2] = Lifting.liftFunction(fun).value(arg)

    def $(arg: Phrase[T1]): Phrase[T2] = apply(arg)
  }

  implicit class CallExpLambda[T <: PhraseType](fun: Phrase[ExpType ->: T]) {
    def apply(arg: Phrase[ExpType]): Phrase[T] = CallLambda[ExpType, T](fun)(arg)

    def $(arg: Phrase[ExpType]): Phrase[T] = apply(arg)
  }

  implicit class CallNatDependentLambda[T <: PhraseType](fun: Phrase[`(nat)->:`[T]]) {
    def apply(arg: Nat): Phrase[T] =
      Lifting.liftDependentFunction[NatKind, T](fun)(arg)

    def $(arg: Nat): Phrase[T] = apply(arg)
  }

  implicit class CallTypeDependentLambda[T <: PhraseType](fun: Phrase[`(dt)->:`[T]]) {
    def apply(arg: DataType): Phrase[T] =
      Lifting.liftDependentFunction[DataKind, T](fun)(arg)

    def $(arg: DataType): Phrase[T] = apply(arg)
  }

  implicit class FunComp[T1 <: PhraseType, T2 <: PhraseType](f: Phrase[T1 ->: T2]) {
    def o[T3 <: PhraseType](g: Phrase[T3 ->: T1]): Phrase[T3 ->: T2] = {
      λ(g.t.inT)(arg => f(g(arg)))
    }
  }

  implicit class SequentialComposition(c1: Phrase[CommType]) {
    def `;`(c2: Phrase[CommType]): Phrase[CommType] = Seq(c1, c2)
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def rd: Proj1[ExpType, AccType] = π1(v)
    def wr: Proj2[ExpType, AccType] = π2(v)
  }

  implicit class PairExtensions[T1 <: PhraseType, T2 <: PhraseType](v: Phrase[T1 x T2]) {
    def _1: Proj1[T1, T2] = π1(v)
    def _2: Proj2[T1, T2] = π2(v)
  }

  def mapTransientNat(natExpr: Phrase[ExpType], f: Nat => Nat): Phrase[ExpType] = {
    Phrase.Internal.exprFromTransientNat(
      Phrase.Internal.transientNatFromExpr(natExpr).map(f))
  }

  implicit def toLiteralInt(i: Int): Literal = Literal(IntData(i))
  implicit def toLiteralFloat(f: Float): Literal = Literal(FloatData(f))

  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): PhrasePair[T1, T2] =
    Phrases.PhrasePair(pair._1, pair._2)
}

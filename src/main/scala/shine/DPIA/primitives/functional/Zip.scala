package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.{ZipAcc1, ZipAcc2}
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Zip(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     access: AccessType,
                     e1: Phrase[ExpType],
                     e2: Phrase[ExpType]
                    ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  e1 :: expT(n`.`dt1, access)
  e2 :: expT(n`.`dt2, access)
  override val t: ExpType = expT(n`.`(dt1 x dt2), access)

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, e1), OperationalSemantics.eval(s, e2)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          PairData(p._1, p._2)
        })
      case _ => throw new Exception("This should not happen")
    }
  }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    acc(e1)(ZipAcc1(n, dt1, dt2, A)) `;`
      acc(e2)(ZipAcc2(n, dt1, dt2, A))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(e1)(λ(expT(n`.`dt1, read))(x =>
      con(e2)(λ(expT(n`.`dt2, read))(y =>
        C(Zip(n, dt1, dt2, access, x, y)) )) ))

  override def streamTranslation(C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])
                                (implicit context: TranslationContext): Phrase[CommType] = {
    val i = NatIdentifier("i")
    str(e1)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(next1 =>
    str(e2)(fun((i: NatIdentifier) ->:
      (expT(dt2, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(next2 =>
      C(nFun(i => fun(expT(dt1 x dt2, read) ->: (comm: CommType))(k =>
        Apply(DepApply[NatKind, (ExpType ->: CommType) ->: CommType](next1, i),
        fun(expT(dt1, read))(x1 =>
        Apply(DepApply[NatKind, (ExpType ->: CommType) ->: CommType](next2, i),
        fun(expT(dt2, read))(x2 =>
          k(MakePair(dt1, dt2, read, x1, x2))
        ))))
      ), arithexpr.arithmetic.RangeAdd(0, n, 1)))
    ))))
  }
}

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
import shine.DPIA.primitives.imperative.ReorderAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Reorder(n: Nat,
                         dt: DataType,
                         access: AccessType,
                         idxF: Phrase[ExpType ->: ExpType],
                         idxFinv: Phrase[ExpType ->: ExpType],
                         input: Phrase[ExpType]
                        ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  idxF :: expT(idx(n), read) ->: expT(idx(n), read)
  idxFinv :: expT(idx(n), read) ->: expT(idx(n), read)
  input :: expT(n`.`dt, access)
  override val t: ExpType = expT(n`.`dt, access)

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType], Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]): Phrase[AccType] =
    fedAcc(env)(input)(λ(accT(C.t.inT.dataType))(o => ReorderAcc(n, dt, idxFinv, C(o))))

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    acc(input)(ReorderAcc(n, dt, idxFinv, A))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(input)(λ(expT(n`.`dt, read))(x =>
      C(Reorder(n, dt, access, idxF, idxFinv, x))))

  override def eval(s: Store): Data = {
    import shine.DPIA.Semantics.OperationalSemantics._
    val idxFE = OperationalSemantics.eval(s, idxF)
    OperationalSemantics.eval(s, input) match {
      case ArrayData(a) =>
        val res = new scala.Array[Data](a.length)
        for (i <- a.indices) {
          res(i) = a(OperationalSemantics.evalIndexExp(s, idxFE(i)).eval)
        }
        ArrayData(res.toVector)
      case _ => throw new Exception("This should not happen")
    }
  }
}

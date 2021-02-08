package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.cuda.primitives.imperative.WmmaMMA
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class TensorMatMultAdd(m: Nat,
                                  n: Nat,
                                  k: Nat,
                                  layoutA: MatrixLayout,
                                  layoutB: MatrixLayout,
                                  dataType: DataType,
                                  dataTypeAcc: DataType,
                                  aMatrix: Phrase[ExpType],
                                  bMatrix: Phrase[ExpType],
                                  cMatrix: Phrase[ExpType]) extends ExpPrimitive with AccT {
  aMatrix :: ExpType(FragmentType(m, k, n, dataType, FragmentKind.AMatrix, layoutA), read)
  bMatrix :: ExpType(FragmentType(k, n, m, dataType, FragmentKind.BMatrix, layoutB), read)
  cMatrix :: ExpType(FragmentType(m, n, k, dataTypeAcc), read)

  override val t: ExpType = ExpType(FragmentType(m, n, k, dataTypeAcc), write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    con(aMatrix)(λ(ExpType(FragmentType(m, n, k, dataType, FragmentKind.AMatrix, layoutA), read))(aMatrix =>
      con(bMatrix)(λ(ExpType(FragmentType(m, n, k, dataType, FragmentKind.BMatrix, layoutB), read))(bMatrix =>
        con(cMatrix)(λ(ExpType(FragmentType(m, n, k, dataTypeAcc), read))(cMatrix =>
            WmmaMMA(m, n, k, layoutA, layoutB, dataType, dataTypeAcc, aMatrix, bMatrix, cMatrix, A)))))))
  }
}

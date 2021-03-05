package shine.cuda.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL._
import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.cuda.primitives.imperative.WmmaMMA
import shine.macros.Primitive.expPrimitive

/**
  * Executes an MMA instruction using (multiple) Tensor Cores ({@link WmmaMMA}). <br>
  * Returns a `Accumulator`-fragment as result of: aMatrix * bMatrix + cMatrix
  * (inplace operations using the same variable as `Acceptor` in `acceptorTranslation` and
  * as `cMatrix` are possible). <br>
  * This primitive needs to be executed by a full warp!
  * @param m            number of rows of the `aMatrix`
  * @param n            number of columns of the `bMatrix` and the `cMatrix`
  * @param k            number of columns of the `aMatrix` and number of rows of the `bMatrix`
  * @param layoutA      layout of the `aMatrix`
  * @param layoutB      layout of the `bMatrix`
  * @param dataType     datatype of elements of `aMatrix` and `bMatrix` ({@link FragmentType#datatype})
  * @param dataTypeAcc  datatype of elements of `cMatrix` and the resultMatrix ({@link FragmentType#datatype})
  * @param aMatrix      first factor of type fragment
  * @param bMatrix      second factor of type fragment
  * @param cMatrix      accumulator of type fragment which is added to the product of `aMatrix` * `bMatrix`
  */
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

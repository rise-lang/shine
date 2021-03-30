package shine.cuda.primitives.imperative

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

/**
  * Executes an MMA instruction using (multiple) Tensor Cores. <br>
  * Calculates: aMatrix * bMatrix + cMatrix <br>
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
  * @param resultMatrix fragment-Accumulator in which the result is stored
  *                     (inplace operations using the `cMatrix` as resultMatrix is possible)
  */
@comPrimitive
case class WmmaMMA(m: Nat,
                   n: Nat,
                   k: Nat,
                   layoutA : MatrixLayout,
                   layoutB : MatrixLayout,
                   dataType: DataType,
                   dataTypeAcc: DataType,
                   aMatrix: Phrase[ExpType],
                   bMatrix: Phrase[ExpType],
                   cMatrix: Phrase[ExpType],
                   resultMatrix: Phrase[AccType]
                  ) extends CommandPrimitive {
  aMatrix :: ExpType(FragmentType(m, k, n, dataType, FragmentKind.AMatrix, layoutA), read)
  bMatrix :: ExpType(FragmentType(k, n, m, dataType, FragmentKind.BMatrix, layoutB), read)
  cMatrix :: ExpType(FragmentType(m, n, k, dataTypeAcc), read)
  resultMatrix :: AccType(FragmentType(m, n, k, dataTypeAcc))

  override def prettyPrint: String =
    s"WmmaMMA(${PrettyPhrasePrinter(aMatrix)}, ${PrettyPhrasePrinter(bMatrix)}," +
      s"${PrettyPhrasePrinter(cMatrix)}, ${PrettyPhrasePrinter(resultMatrix)})"
}

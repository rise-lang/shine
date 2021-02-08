package shine.cuda.primitives.imperative

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

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

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.cuda.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class AsMatrix(val rows: Nat, val columns: Nat, val layers: Nat, val dt: DataType, val input: Phrase[ExpType]) extends ExpPrimitive {
  {
    input :: expT(FragmentType(rows, columns, layers, dt, FragmentKind.Accumulator, MatrixLayout.None), read)
  }
  override val t: ExpType = expT(ArrayType(rows, ArrayType(columns, dt)), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): AsMatrix = new AsMatrix(v.nat(rows), v.nat(columns), v.nat(layers), v.data(dt), VisitAndRebuild(input, v))
}

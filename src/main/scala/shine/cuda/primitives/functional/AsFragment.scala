// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.cuda.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class AsFragment(val rows: Nat, val columns: Nat, val layers: Nat, val dt: DataType, val frag: Fragment, val layout: MatrixLayout, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(ArrayType(rows, ArrayType(columns, dt)), read)
    true
  }
  override val t: ExpType = expT(FragmentType(rows, columns, layers, dt, frag, layout), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): AsFragment = new AsFragment(v.nat(rows), v.nat(columns), v.nat(layers), v.data(dt), frag, layout, VisitAndRebuild(input, v))
}

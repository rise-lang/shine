// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.GAP8.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class FunConv7x7(val w: Nat, val h: Nat, val bias: Nat, val dt: DataType, val in: Phrase[ExpType], val filter: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    in :: expT(ArrayType(w, ArrayType(h, dt)), read)
    filter :: expT(ArrayType(7, ArrayType(7, dt)), read)
    true
  }
  override val t: ExpType = expT(ArrayType(w - 6, ArrayType(h - 6, dt)), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): FunConv7x7 = new FunConv7x7(v.nat(w), v.nat(h), v.nat(bias), v.data(dt), VisitAndRebuild(in, v), VisitAndRebuild(filter, v))
}
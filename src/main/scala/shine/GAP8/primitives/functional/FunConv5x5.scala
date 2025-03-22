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
final case class FunConv5x5(val h: Nat, val w: Nat, val dt: DataType, val bias: Nat, val in: Phrase[ExpType], val filter: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    in :: expT(ArrayType(h, ArrayType(w, dt)), read)
    filter :: expT(ArrayType(5, ArrayType(5, dt)), read)
    true
  }
  override val t: ExpType = expT(ArrayType(h - 4, ArrayType(w - 4, dt)), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): FunConv5x5 = new FunConv5x5(v.nat(h), v.nat(w), v.data(dt), v.nat(bias), VisitAndRebuild(in, v), VisitAndRebuild(filter, v))
}

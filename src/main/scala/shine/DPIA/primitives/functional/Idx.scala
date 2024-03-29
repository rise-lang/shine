// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Idx(val n: Nat, val dt: DataType, val index: Phrase[ExpType], val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    index :: expT(IndexType(n), read)
    array :: expT(ArrayType(n, dt), read)
    true
  }
  override val t: ExpType = expT(dt, read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Idx = new Idx(v.nat(n), v.data(dt), VisitAndRebuild(index, v), VisitAndRebuild(array, v))
}

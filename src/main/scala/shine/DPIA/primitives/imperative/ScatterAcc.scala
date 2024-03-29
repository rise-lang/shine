// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class ScatterAcc(val n: Nat, val m: Nat, val dt: DataType, val indices: Phrase[ExpType], val array: Phrase[AccType]) extends AccPrimitive {
  assert {
    indices :: expT(ArrayType(n, IndexType(m)), read)
    array :: accT(ArrayType(m, dt))
    true
  }
  override val t: AccType = accT(ArrayType(n, dt))
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ScatterAcc = new ScatterAcc(v.nat(n), v.nat(m), v.data(dt), VisitAndRebuild(indices, v), VisitAndRebuild(array, v))
}

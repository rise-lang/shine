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
final case class AsScalarAcc(val n: Nat, val m: Nat, val dt: DataType, val array: Phrase[AccType]) extends AccPrimitive {
  assert {
    array :: accT(ArrayType(m * n, dt))
    true
  }
  override val t: AccType = accT(ArrayType(m, VectorType(n, dt)))
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): AsScalarAcc = new AsScalarAcc(v.nat(n), v.nat(m), v.data(dt), VisitAndRebuild(array, v))
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class UnzipAcc(val n: Nat, val dt1: DataType, val dt2: DataType, val a: Phrase[AccType]) extends AccPrimitive {
  assert {
    a :: accT(PairType(ArrayType(n, dt1), ArrayType(n, dt2)))
    true
  }
  override val t: AccType = accT(ArrayType(n, PairType(dt1, dt2)))
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): UnzipAcc = new UnzipAcc(v.nat(n), v.data(dt1), v.data(dt2), VisitAndRebuild(a, v))
}

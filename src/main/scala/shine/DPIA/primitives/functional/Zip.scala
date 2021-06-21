// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.Types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Zip(val n: Nat, val dt1: DataType, val dt2: DataType, val a: AccessType, val e1: Phrase[ExpType], val e2: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    e1 :: expT(ArrayType(n, dt1), a)
    e2 :: expT(ArrayType(n, dt2), a)
    true
  }
  override val t: ExpType = expT(ArrayType(n, PairType(dt1, dt2)), a)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Zip = new Zip(v.nat(n), v.data(dt1), v.data(dt2), v.access(a), VisitAndRebuild(e1, v), VisitAndRebuild(e2, v))
}

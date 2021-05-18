// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class Generate(val n: Nat, val dt: DataType, val f: Phrase[FunType[ExpType, ExpType]]) extends ExpPrimitive {
  assert {
    f :: FunType(expT(IndexType(n), read), expT(dt, read))
    true
  }
  override val t: ExpType = expT(ArrayType(n, dt), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Generate = new Generate(v.nat(n), v.data(dt), VisitAndRebuild(f, v))
}

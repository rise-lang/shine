// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.cuda.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.Types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class GlobalToSharedAcc(val dt: DataType, val pipe: Phrase[ExpType], val outputShared: Phrase[AccType]) extends AccPrimitive {
  assert {
    pipe :: expT(OpaqueType("pipeline"), read)
    outputShared :: accT(dt)
    true
  }
  override val t: AccType = accT(dt)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): GlobalToSharedAcc = new GlobalToSharedAcc(v.data(dt), VisitAndRebuild(pipe, v), VisitAndRebuild(outputShared, v))
}

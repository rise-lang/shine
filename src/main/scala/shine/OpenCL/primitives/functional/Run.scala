// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.OpenCL.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.Types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Run(localSize: shine.OpenCL.LocalSize, globalSize: shine.OpenCL.GlobalSize)(val dt: DataType, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(dt, write)
    true
  }
  override val t: ExpType = expT(dt, write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Run = new Run(localSize.visitAndRebuild(v), globalSize.visitAndRebuild(v))(v.data(dt), VisitAndRebuild(input, v))
  def unwrap: (DataType, Phrase[ExpType]) = (dt, input)
}

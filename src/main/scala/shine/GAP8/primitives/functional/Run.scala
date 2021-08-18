// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.GAP8.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.Types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Run(cores: Nat)(val dt: DataType, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(dt, write)
    true
  }
  override val t: ExpType = expT(dt, write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Run = new Run(v.nat(cores))(v.data(dt), VisitAndRebuild(input, v))
  def unwrap: (DataType, Phrase[ExpType]) = (dt, input)
}
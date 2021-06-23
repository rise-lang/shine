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
final case class SyncPipeline(val pipe: Phrase[ExpType]) extends CommandPrimitive {
  assert {
    pipe :: expT(OpaqueType("pipeline"), read)
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): SyncPipeline = new SyncPipeline(VisitAndRebuild(pipe, v))
}

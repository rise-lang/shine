// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.cuda.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class GlobalToShared(val dt: DataType, val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    input :: expT(dt, write)
    true
  }
  override val t: ExpType = expT(dt, read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): GlobalToShared = new GlobalToShared(v.data(dt), VisitAndRebuild(input, v))
}

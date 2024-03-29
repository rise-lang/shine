// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Cast(val dt1: DataType, val dt2: DataType, val e: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    e :: expT(dt1, read)
    true
  }
  override val t: ExpType = expT(dt2, read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Cast = new Cast(v.data(dt1), v.data(dt2), VisitAndRebuild(e, v))
}

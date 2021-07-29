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
final case class PairAcc1(val dt1: DataType, val dt2: DataType, val pair: Phrase[AccType]) extends AccPrimitive {
  assert {
    pair :: accT(PairType(dt1, dt2))
    true
  }
  override val t: AccType = accT(dt1)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): PairAcc1 = new PairAcc1(v.data(dt1), v.data(dt2), VisitAndRebuild(pair, v))
}

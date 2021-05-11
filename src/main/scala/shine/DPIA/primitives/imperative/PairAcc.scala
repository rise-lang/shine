// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class PairAcc(val dt1: DataType, val dt2: DataType, val fst: Phrase[AccType], val snd: Phrase[AccType]) extends AccPrimitive {
  assert {
    fst :: accT(dt1)
    snd :: accT(dt2)
    true
  }
  override val t: AccType = accT(PairType(dt1, dt2))
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): PairAcc = new PairAcc(v.data(dt1), v.data(dt2), VisitAndRebuild(fst, v), VisitAndRebuild(snd, v))
}

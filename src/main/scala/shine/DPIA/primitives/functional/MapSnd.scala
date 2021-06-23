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
final case class MapSnd(val a: AccessType, val dt1: DataType, val dt2: DataType, val dt3: DataType, val f: Phrase[FunType[ExpType, ExpType]], val pair: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    f :: FunType(expT(dt2, a), expT(dt3, a))
    pair :: expT(PairType(dt1, dt2), a)
    true
  }
  override val t: ExpType = expT(PairType(dt1, dt3), a)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): MapSnd = new MapSnd(v.access(a), v.data(dt1), v.data(dt2), v.data(dt3), VisitAndRebuild(f, v), VisitAndRebuild(pair, v))
}

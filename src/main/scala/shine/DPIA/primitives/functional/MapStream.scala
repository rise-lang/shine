// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class MapStream(val n: Nat, val dt1: DataType, val dt2: DataType, val f: Phrase[FunType[ExpType, ExpType]], val array: Phrase[ExpType]) extends ExpPrimitive {
  {
    f :: FunType(expT(ArrayType(n, dt1), read), expT(ArrayType(n, dt2), write))
    array :: expT(ArrayType(n, dt1), read)
  }
  override val t: ExpType = expT(ArrayType(n, dt2), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): MapStream = new MapStream(v.nat(n), v.data(dt1), v.data(dt2), VisitAndRebuild(f, v), VisitAndRebuild(array, v))
}

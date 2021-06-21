// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.Types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class MapRead(val n: Nat, val dt1: DataType, val dt2: DataType, val f: Phrase[FunType[ExpType, FunType[FunType[ExpType, CommType], CommType]]], val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    f :: FunType(expT(dt1, read), FunType(FunType(expT(dt2, read), comm), comm))
    input :: expT(ArrayType(n, dt1), read)
    true
  }
  override val t: ExpType = expT(ArrayType(n, dt2), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): MapRead = new MapRead(v.nat(n), v.data(dt1), v.data(dt2), VisitAndRebuild(f, v), VisitAndRebuild(input, v))
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class CircularBuffer(val n: Nat, val alloc: Nat, val sz: Nat, val dt1: DataType, val dt2: DataType, val load: Phrase[FunType[ExpType, ExpType]], val input: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    load :: FunType(expT(dt1, read), expT(dt2, write))
    input :: expT(ArrayType(n - 1 + sz, dt1), read)
    true
  }
  override val t: ExpType = expT(ArrayType(n, ArrayType(sz, dt2)), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): CircularBuffer = new CircularBuffer(v.nat(n), v.nat(alloc), v.nat(sz), v.data(dt1), v.data(dt2), VisitAndRebuild(load, v), VisitAndRebuild(input, v))
}

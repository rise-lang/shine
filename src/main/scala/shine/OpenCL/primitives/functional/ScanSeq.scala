// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.OpenCL.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.Types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class ScanSeq(unroll: Boolean)(val n: Nat, val a: AddressSpace, val dt1: DataType, val dt2: DataType, val f: Phrase[FunType[ExpType, FunType[ExpType, ExpType]]], val init: Phrase[ExpType], val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    f :: FunType(expT(dt1, read), FunType(expT(dt2, read), expT(dt2, write)))
    init :: expT(dt2, write)
    array :: expT(ArrayType(n, dt1), read)
    true
  }
  override val t: ExpType = expT(ArrayType(n, dt2), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ScanSeq = new ScanSeq(unroll)(v.nat(n), v.addressSpace(a), v.data(dt1), v.data(dt2), VisitAndRebuild(f, v), VisitAndRebuild(init, v), VisitAndRebuild(array, v))
  def unwrap: (Nat, AddressSpace, DataType, DataType, Phrase[FunType[ExpType, FunType[ExpType, ExpType]]], Phrase[ExpType], Phrase[ExpType]) = (n, a, dt1, dt2, f, init, array)
}

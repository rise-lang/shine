// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class Partition(val n: Nat, val m: Nat, val lenF: NatToNat, val dt: DataType, val array: Phrase[ExpType]) extends ExpPrimitive {
  {
    array :: expT(ArrayType(n, dt), read)
  }
  override val t: ExpType = expT(DepArrayType(m, n2dtFun { (i: NatIdentifier) => ArrayType(lenF(i), dt) }), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Partition = new Partition(v.nat(n), v.nat(m), v.natToNat(lenF), v.data(dt), VisitAndRebuild(array, v))
}

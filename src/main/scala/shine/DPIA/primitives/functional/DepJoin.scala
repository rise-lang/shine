// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class DepJoin(val n: Nat, val lenF: NatToNat, val dt: DataType, val array: Phrase[ExpType]) extends ExpPrimitive {
  {
    array :: expT(DepArrayType(n, n2dtFun { (i: NatIdentifier) => ArrayType(lenF(i), dt) }), read)
  }
  override val t: ExpType = expT(ArrayType(BigSum(from = 0, upTo = n - 1, (i: Nat) => lenF(i)), dt), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): DepJoin = new DepJoin(v.nat(n), v.natToNat(lenF), v.data(dt), VisitAndRebuild(array, v))
}

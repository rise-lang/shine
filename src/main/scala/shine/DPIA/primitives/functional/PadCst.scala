// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
final case class PadCst(val n: Nat, val l: Nat, val r: Nat, val dt: DataType, val padExp: Phrase[ExpType], val array: Phrase[ExpType]) extends ExpPrimitive {
  {
    padExp :: expT(dt, read)
    array :: expT(ArrayType(n, dt), read)
  }
  override val t: ExpType = expT(ArrayType(l + n + r, dt), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): PadCst = new PadCst(v.nat(n), v.nat(l), v.nat(r), v.data(dt), VisitAndRebuild(padExp, v), VisitAndRebuild(array, v))
}

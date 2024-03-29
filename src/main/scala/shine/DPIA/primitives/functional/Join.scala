// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.DPIA.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class Join(val n: Nat, val m: Nat, val a: Access, val dt: DataType, val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    array :: expT(ArrayType(n, ArrayType(m, dt)), a)
    true
  }
  override val t: ExpType = expT(ArrayType(n * m, dt), a)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Join = new Join(v.nat(n), v.nat(m), v.access(a), v.data(dt), VisitAndRebuild(array, v))
}

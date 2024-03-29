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
final case class NatAsIndex(val n: Nat, val e: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    e :: expT(NatType, read)
    true
  }
  override val t: ExpType = expT(IndexType(n), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): NatAsIndex = new NatAsIndex(v.nat(n), VisitAndRebuild(e, v))
}

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
final case class ForNat(unroll: Boolean)(val n: Nat, val loopBody: Phrase[DepFunType[NatIdentifier, INat, CommType]]) extends CommandPrimitive {
  assert {
    loopBody :: ({
      val i = loopBody.t.x
      DepFunType(NatKind, i, comm)
    })
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ForNat = new ForNat(unroll)(v.nat(n), VisitAndRebuild(loopBody, v))
  def unwrap: (Nat, Phrase[DepFunType[NatIdentifier, INat, CommType]]) = (n, loopBody)
}

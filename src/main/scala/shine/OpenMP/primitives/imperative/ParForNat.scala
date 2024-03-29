// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.OpenMP.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class ParForNat(val n: Nat, val ft: NatToData, val out: Phrase[AccType], val body: Phrase[DepFunType[NatIdentifier, FunType[AccType, CommType]]]) extends CommandPrimitive {
  assert {
    out :: accT(DepArrayType(n, ft))
    body :: ({
      val i = body.t.x
      DepFunType(NatKind, i, FunType(accT(NatToDataApply(ft, i)), comm))
    })
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ParForNat = new ParForNat(v.nat(n), v.natToData(ft), VisitAndRebuild(out, v), VisitAndRebuild(body, v))
}

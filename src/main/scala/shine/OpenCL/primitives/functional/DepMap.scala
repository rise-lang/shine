// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.OpenCL.primitives.functional
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, Type => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class DepMap(level: shine.OpenCL.ParallelismLevel, dim: Int)(val n: Nat, val ft1: NatToData, val ft2: NatToData, val f: Phrase[DepFunType[NatIdentifier, FunType[ExpType, ExpType]]], val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    f :: ({
      val m = f.t.x
      DepFunType(NatKind, m, FunType(expT(NatToDataApply(ft1, m), read), expT(NatToDataApply(ft2, m), write)))
    })
    array :: expT(DepArrayType(n, ft1), read)
    true
  }
  override val t: ExpType = expT(DepArrayType(n, ft2), write)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): DepMap = new DepMap(level, dim)(v.nat(n), v.natToData(ft1), v.natToData(ft2), VisitAndRebuild(f, v), VisitAndRebuild(array, v))
  def unwrap: (Nat, NatToData, NatToData, Phrase[DepFunType[NatIdentifier, FunType[ExpType, ExpType]]], Phrase[ExpType]) = (n, ft1, ft2, f, array)
}

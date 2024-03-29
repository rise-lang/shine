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
final case class TransposeDepArray(val n: Nat, val m: Nat, val ft: NatToData, val array: Phrase[ExpType]) extends ExpPrimitive {
  assert {
    array :: expT(ArrayType(n, DepArrayType(m, ft)), read)
    true
  }
  override val t: ExpType = expT(DepArrayType(n, n2dtFun { (i: NatIdentifier) => ArrayType(n, NatToDataApply(ft, i)) }), read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): TransposeDepArray = new TransposeDepArray(v.nat(n), v.nat(m), v.natToData(ft), VisitAndRebuild(array, v))
}

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package shine.cuda.primitives.imperative
import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import rise.core.types.{ FunType => _, DepFunType => _, TypePlaceholder => _, TypeIdentifier => _, ExprType => _, _ }
import rise.core.types.DataType._
import rise.core.types.Kind.{ Identifier => _, _ }
import shine.DPIA._
final case class ForFragment(val rows: Nat, val columns: Nat, val layers: Nat, val dt: DataType, val frag: Fragment, val layout: MatrixLayout, val in: Phrase[ExpType], val out: Phrase[AccType], val fun: Phrase[FunType[ExpType, FunType[AccType, CommType]]]) extends CommandPrimitive {
  assert {
    in :: expT(FragmentType(rows, columns, layers, dt, frag, layout), read)
    out :: accT(FragmentType(rows, columns, layers, dt, frag, layout))
    fun :: FunType(expT(dt, read), FunType(accT(dt), comm))
    true
  }
  override val t: CommType = comm
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ForFragment = new ForFragment(v.nat(rows), v.nat(columns), v.nat(layers), v.data(dt), frag, layout, VisitAndRebuild(in, v), VisitAndRebuild(out, v), VisitAndRebuild(fun, v))
}

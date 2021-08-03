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
final case class ForeignFunctionCall(funDecl: rise.core.ForeignFunction.Decl, n: Int)(val inTs: Seq[DataType], val outT: DataType, val args: Seq[Phrase[ExpType]]) extends ExpPrimitive {
  assert {
    {
      typeAssert(args.length == n, "args" + ".length == " + "n" + " is not true")
      typeAssert(inTs.length == n, "inTs" + ".length == " + "n" + " is not true")
      args.zip(inTs).foreach({
        case (args, inTs) =>
          args :: expT(inTs, read)
      })
    }
    true
  }
  override val t: ExpType = expT(outT, read)
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): ForeignFunctionCall = new ForeignFunctionCall(funDecl, n)(inTs.map(v.data), v.data(outT), args.map(VisitAndRebuild(_, v)))
  def unwrap: (Seq[DataType], DataType, Seq[Phrase[ExpType]]) = (inTs, outT, args)
}

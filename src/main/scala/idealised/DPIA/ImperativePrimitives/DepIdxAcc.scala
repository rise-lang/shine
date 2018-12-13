package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class DepIdxAcc(n: Nat,
                           i:NatIdentifier,
                           dt: DataType,
                           index: Phrase[ExpType],
                           array: Phrase[AccType])
  extends AccPrimitive with GeneratableAcc {

  private def makeDt(x:Nat):DataType = DataType.substitute(x, `for`=i, `in`=dt)

  override val `type`: AccType =
    (n: Nat) -> (dt: DataType) ->
      (index :: exp"[idx($n)]") ->
      (array :: acc"[${DepArrayType(n, makeDt)}]") ->
      acc"[${makeDt(Lifting.liftIndexExpr(index))}]"

  override def eval(s: Store): AccIdentifier = {
//    val arrayE = OperationalSemantics.eval(s, array)
//    val indexE = OperationalSemantics.eval(s, index) match {
//      case IntData(i) => i
//      case _ => throw new Exception("This should not happen")
//    }
//    ArrayAccessIdentifier(arrayE, indexE)
    ???
  }

  override def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment, path: Path): Expr = {
//    gen.codeGenIdxAcc(index, array, env, path, gen)
    ???
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DepIdxAcc(fun(n), fun(i).asInstanceOf[NatIdentifier], fun(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(array)}[${PrettyPhrasePrinter(index)}]"

  override def xmlPrinter: Elem =
    <depIdxAcc n={ToString(n)} i={ToString(i)} dt={ToString(dt)}>
      <input type={ToString(array.t)}>
        {Phrases.xmlPrinter(array)}
      </input>
      <index type={ToString(index.t)}>
        {Phrases.xmlPrinter(index)}
      </index>
    </depIdxAcc>
}

object DepIdxAcc {
  def apply(index: Phrase[ExpType],
            array: Phrase[AccType]): DepIdxAcc = {
    (index.t, array.t) match {
      case (ExpType(IndexType(n1)), AccType(DepArrayType(n2, i, dt_))) if n1 == n2 =>
        DepIdxAcc(n1, i, dt_, index, array)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }
}

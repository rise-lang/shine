package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class IdxVecAcc(n: Nat,
                           st: ScalarType,
                           index: Phrase[ExpType],
                           vector: Phrase[AccType])
  extends AccPrimitive with GeneratableAcc {

  override val `type`: AccType =
    (n: Nat) -> (st: ScalarType) ->
      (index :: exp"[idx($n)]") ->
        (vector :: acc"[${VectorType(n, st)}]") ->
          acc"[$st]"

  override def eval(s: Store): AccIdentifier = {
    val vectorE = OperationalSemantics.eval(s, vector)
    val indexE = OperationalSemantics.eval(s, index) match {
      case IntData(i) => i
      case _ => throw new Exception("This should not happen")
    }
    VectorAccessIdentifier(vectorE, indexE)
  }

  override def codeGen[Environment, Path, Stmt, Expr, Decl, Ident](gen: CodeGenerator[Environment, Path, Stmt, Expr, Decl, Ident])(env: Environment, path: Path): Expr = {
    gen.codeGenIdxVecAcc(index, vector, env, path, gen)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    IdxVecAcc(fun(n), fun(st), VisitAndRebuild(index, fun), VisitAndRebuild(vector, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(vector)}[${PrettyPhrasePrinter(index)}]"

  override def xmlPrinter: Elem =
    <idxVecAcc n={ToString(n)} st={ToString(st)}>
      <output>
        {Phrases.xmlPrinter(vector)}
      </output>
      <index>
        {Phrases.xmlPrinter(index)}
      </index>
    </idxVecAcc>
}

object IdxVecAcc {
  def apply(index: Phrase[ExpType],
            vector: Phrase[AccType]): IdxVecAcc = {
    (index.t, vector.t) match {
      case (ExpType(IndexType(n1)), AccType(VectorType(n2, st))) if n1 == n2 =>
        IdxVecAcc(n1, st, index, vector)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }
}

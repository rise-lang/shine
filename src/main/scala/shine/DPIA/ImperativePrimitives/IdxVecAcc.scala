package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class IdxVecAcc(n: Nat,
                           st: ScalarType,
                           index: Phrase[ExpType],
                           vector: Phrase[AccType])
  extends AccPrimitive {

  index :: expT(idx(n), read)
  vector :: accT(vec(n, st))
  override val t: AccType = accT(st)

  override def eval(s: Store): AccIdentifier = {
    val vectorE = OperationalSemantics.eval(s, vector)
    val indexE = OperationalSemantics.eval(s, index) match {
      case IntData(i) => i
      case _ => throw new Exception("This should not happen")
    }
    VectorAccessIdentifier(vectorE, indexE)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    IdxVecAcc(fun.nat(n), fun.data(st), VisitAndRebuild(index, fun), VisitAndRebuild(vector, fun))
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
      case (ExpType(IndexType(n1), _: read.type), AccType(VectorType(n2, st))) if n1 == n2 =>
        IdxVecAcc(n1, st, index, vector)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }
}

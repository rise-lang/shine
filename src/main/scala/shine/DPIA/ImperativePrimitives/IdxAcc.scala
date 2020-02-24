package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class IdxAcc(n: Nat,
                        dt: DataType,
                        index: Phrase[ExpType],
                        array: Phrase[AccType])
  extends AccPrimitive {

  index :: expT(idx(n), read)
  array :: accT(n`.`dt)
  override val t: AccType = accT(dt)

  override def eval(s: Store): AccIdentifier = {
    val arrayE = OperationalSemantics.eval(s, array)
    val indexE = OperationalSemantics.eval(s, index) match {
      case IntData(i) => i
      case _ => throw new Exception("This should not happen")
    }
    ArrayAccessIdentifier(arrayE, indexE)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    IdxAcc(fun.nat(n), fun.data(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(array)}[${PrettyPhrasePrinter(index)}]"

  override def xmlPrinter: Elem =
    <idxAcc n={ToString(n)} dt={ToString(dt)}>
      <output>
        {Phrases.xmlPrinter(array)}
      </output>
      <index>
        {Phrases.xmlPrinter(index)}
      </index>
    </idxAcc>
}

object IdxAcc {
  def apply(index: Phrase[ExpType],
            array: Phrase[AccType]): IdxAcc = {
    (index.t, array.t) match {
      case (ExpType(IndexType(n1), _: read.type), AccType(ArrayType(n2, dt_))) if n1 == n2 =>
        IdxAcc(n1, dt_, index, array)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }
}
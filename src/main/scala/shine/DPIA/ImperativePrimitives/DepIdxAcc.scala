package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class DepIdxAcc(n: Nat,
                           ft:NatToData,
                           index: Nat,
                           array: Phrase[AccType])
  extends AccPrimitive {

  array :: accT(n`.d`ft)
  override val t: AccType = accT(ft(index))

  override def eval(s: Store): AccIdentifier = {
//    val arrayE = OperationalSemantics.eval(s, array)
//    val indexE = OperationalSemantics.eval(s, index) match {
//      case IntData(i) => i
//      case _ => throw new Exception("This should not happen")
//    }
//    ArrayAccessIdentifier(arrayE, indexE)
    ???
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DepIdxAcc(fun.nat(n), fun.natToData(ft), fun.nat(index), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(array)}[$index]"

  override def xmlPrinter: Elem =
    <depIdxAcc n={ToString(n)} ft={ToString(ft)} index={ToString(index)}>
      <input type={ToString(array.t)}>
        {Phrases.xmlPrinter(array)}
      </input>
    </depIdxAcc>
}


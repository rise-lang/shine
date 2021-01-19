package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

import scala.xml.Elem

@accPrimitive
final case class IdxAcc(n: Nat,
                        dt: DataType,
                        index: Phrase[ExpType],
                        array: Phrase[AccType]
                       )extends AccPrimitive {
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
}

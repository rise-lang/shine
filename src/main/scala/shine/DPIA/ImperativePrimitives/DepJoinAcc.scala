package shine.DPIA.ImperativePrimitives

import arithexpr.arithmetic.BigSum
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class DepJoinAcc(n: Nat,
                            lenF:NatToNat,
                            dt: DataType,
                            array: Phrase[AccType])
  extends AccPrimitive
{

  array :: accT(BigSum(from=0, upTo = n-1, i => lenF(i))`.`dt)
  override val t: AccType = accT(n`.d`{ i => ArrayType(lenF(i), dt) })

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DepJoinAcc(fun.nat(n), fun.natToNat(lenF), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String =
    s"(joinAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <joinAcc n={ToString(n)} lenF={ToString(lenF)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </joinAcc>
}

package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.{ArithExpr, BigSum}

import scala.xml.Elem

final case class DepJoinAcc(n: Nat,
                            lenID: NatIdentifier,
                            lenBody: Nat,
                            dt: DataType,
                            array: Phrase[AccType])
  extends AccPrimitive
{

  val lenF:Nat => Nat = (x:Nat) => ArithExpr.substitute(lenBody, scala.collection.Map((lenID, x)))

  override val `type`: AccType =
    (n: Nat) -> (lenBody: Nat) -> (dt: DataType) ->
      (array :: acc"[${BigSum(from=0, upTo = n-1, `for`=lenID, lenBody)}.$dt]") ->
      acc"[${DepArrayType(n, i => ArrayType(lenF(i), dt))}]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DepJoinAcc(fun(n), fun(lenID).asInstanceOf[NatIdentifier], fun(lenBody), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String =
    s"(joinAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <joinAcc n={ToString(n)} lenID={ToString(lenID)} lenBody={ToString(lenBody)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </joinAcc>
}

package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._
import lift.arithmetic.{ArithExpr, BigSum}

import scala.xml.Elem

final case class DepJoinAcc(n: Nat,
                            lenF:NatNatTypeFunction,
                            dt: DataType,
                            array: Phrase[AccType])
  extends AccPrimitive
{


  override val t: AccType =
    (n: Nat) -> (lenF: NatNatTypeFunction) ->
      (array :: acc"[${BigSum(from=0, upTo = n-1, `for`=lenF.x, lenF.body)}.$dt]") ->
      acc"[${DepArrayType(n, i => ArrayType(lenF(i), dt))}]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DepJoinAcc(fun(n), fun(lenF), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String =
    s"(joinAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <joinAcc n={ToString(n)} lenF={ToString(lenF)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </joinAcc>
}

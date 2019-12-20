package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import arithexpr.arithmetic.{ArithExpr, BigSum}

import scala.xml.Elem

final case class DepJoinAcc(n: Nat,
                            lenF:NatToNat,
                            dt: DataType,
                            array: Phrase[AccType])
  extends AccPrimitive
{


  override val t: AccType =
    (n: Nat) ->: (lenF: NatToNat) ->:
      (array :: acc"[${BigSum(from=0, upTo = n-1, i => lenF(i))}.$dt]") ->:
        acc"[${DepArrayType(n, i => ArrayType(lenF(i), dt))}]"

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

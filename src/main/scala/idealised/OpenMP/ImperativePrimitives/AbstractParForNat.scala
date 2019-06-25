package idealised.OpenMP.ImperativePrimitives


import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractParForNat(val n: Nat,
                                 val ft:NatToData,
                                 val out: Phrase[AccType],
                                 val body: Phrase[`(nat)->`[AccType -> CommType]])
  extends CommandPrimitive {

  override val t: CommType = {

    (n: Nat) -> (ft: NatToData) ->
      (out :: acc"[${DepArrayType(n, ft)}]") ->
      (body :: t"(${body.t.x}:nat) -> acc[${ft(body.t.x)}] -> comm") ->
      comm
  }
  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    makeParForNat(fun(n), fun(ft), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(parForNat $n $ft ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <parForNat n={ToString(n)} ft={ToString(ft)}>
      <output type={ToString(AccType(DepArrayType(n, ft)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(body.t.x -> (AccType({ft(body.t.x)}) -> CommType()))}>
        {Phrases.xmlPrinter(body)}
      </body>
    </parForNat>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  def makeParForNat: (Nat, NatToData, Phrase[AccType], Phrase[`(nat)->`[AccType -> CommType]]) => AbstractParForNat

}

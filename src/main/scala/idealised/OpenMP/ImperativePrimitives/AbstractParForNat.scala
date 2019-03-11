package idealised.OpenMP.ImperativePrimitives


import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

abstract class AbstractParForNat(val n: Nat,
                                 val ft:NatDataTypeFunction,
                                 val out: Phrase[AccType],
                                 val body: Phrase[`(nat)->`[AccType -> CommandType]])
  extends CommandPrimitive {

  override lazy val `type`: CommandType = {

    (n: Nat) -> (ft: NatDataTypeFunction) ->
      (out :: acc"[${DepArrayType(n, ft)}]") ->
      (body :: t"(${body.t.n}:nat) -> acc[${ft(body.t.n)}] -> comm") ->
      comm
  }
  override def eval(s: Store): Store = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommandType] = {
    makeParForNat(fun(n), fun(ft), VisitAndRebuild(out, fun), VisitAndRebuild(body, fun))
  }

  override def prettyPrint: String =
    s"(parForNat $n $ft ${PrettyPhrasePrinter(out)} ${PrettyPhrasePrinter(body)})"


  override def xmlPrinter: Elem =
    <parForNat n={ToString(n)} ft={ToString(ft)}>
      <output type={ToString(AccType(ArrayType(n, ft.body)))}>
        {Phrases.xmlPrinter(out)}
      </output>
      <body type={ToString(body.t.n -> (AccType({ft(body.t.n)}) -> CommandType()))}>
        {Phrases.xmlPrinter(body)}
      </body>
    </parForNat>.copy(label = {
      val name = this.getClass.getSimpleName
      Character.toLowerCase(name.charAt(0)) + name.substring(1)
    })

  def makeParForNat: (Nat, NatDataTypeFunction, Phrase[AccType], Phrase[`(nat)->`[AccType -> CommandType]]) => AbstractParForNat

}

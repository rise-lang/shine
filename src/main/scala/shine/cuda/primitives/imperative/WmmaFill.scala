package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}

import scala.xml.Elem

case class WmmaFill(m: Nat,
                    n: Nat,
                    k: Nat,
                    dataType: DataType,
                    fill: Phrase[ExpType],
                    fragment: Phrase[AccType]
                   ) extends CommandPrimitive {

  fill :: ExpType(dataType, read)
  fragment :: AccType(WmmaAccumulator(m, n, k, dataType))

  override def eval(s: Store): Store = ???

  override def prettyPrint: String =
    s"WmmaFill(${PrettyPhrasePrinter(fill)}, ${PrettyPhrasePrinter(fragment)})"

  override def xmlPrinter: Elem =
    <wmmaFill dt={ToString(dataType)}>
      <fill>
        {Phrases.xmlPrinter(fill)}
      </fill>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
    </wmmaFill>

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    WmmaFill(fun.nat(m), fun.nat(n), fun.nat(k), fun.data(dataType), VisitAndRebuild(fill, fun),
      VisitAndRebuild(fragment, fun))
  }
}

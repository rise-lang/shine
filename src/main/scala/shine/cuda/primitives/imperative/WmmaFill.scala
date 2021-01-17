package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}

import scala.xml.Elem

case class WmmaFill(rows: Nat,
                    columns: Nat,
                    d3: Nat,
                    dataType: DataType,
                    fill: Phrase[ExpType],
                    fragmentType: FragmentType,
                    layout: MatrixLayout,
                    fragment: Phrase[AccType]
                   ) extends CommandPrimitive {

  fill :: ExpType(dataType, read)
  fragment :: AccType(Fragment(rows, columns, d3, dataType, fragmentType, layout))

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
    WmmaFill(fun.nat(rows), fun.nat(columns), fun.nat(d3), fun.data(dataType), VisitAndRebuild(fill, fun),
      fragmentType, layout, VisitAndRebuild(fragment, fun))
  }
}

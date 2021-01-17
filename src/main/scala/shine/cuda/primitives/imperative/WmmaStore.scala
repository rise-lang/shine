package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}

import scala.xml.Elem

final case class WmmaStore(rows: Nat,
                           columns: Nat,
                           d3: Nat,
                           dataType: DataType,
                           fragment: Phrase[ExpType],
                           matrixTile: Phrase[AccType]
                          ) extends CommandPrimitive {

  fragment :: ExpType(Fragment(rows, columns, d3, dataType), read)
  matrixTile :: AccType(ArrayType(rows, ArrayType(columns, dataType)))

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = {
    s"wmmaStore(${PrettyPhrasePrinter(fragment)}, ${PrettyPhrasePrinter(matrixTile)})"
  }

  override def xmlPrinter: Elem =
    <wmmaStore>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
      <matrixTile>
        {Phrases.xmlPrinter(matrixTile)}
      </matrixTile>
    </wmmaStore>

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    WmmaStore(fun.nat(rows), fun.nat(columns), fun.nat(d3), fun.data(dataType), VisitAndRebuild(fragment, fun),
      VisitAndRebuild(matrixTile, fun))
  }
}


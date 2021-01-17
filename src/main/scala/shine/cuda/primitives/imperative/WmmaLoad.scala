package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}

import scala.xml.Elem

final case class WmmaLoad(rows: Nat,
                          columns: Nat,
                          d3: Nat,
                          dataType: DataType,
                          fragType: FragmentType,
                          layout: MatrixLayout,
                          matrixTile: Phrase[ExpType],
                          fragment: Phrase[AccType]
                         ) extends CommandPrimitive {

  fragment :: ExpType(Fragment(rows, columns, d3, dataType, fragType, layout), write)
  matrixTile :: ExpType(ArrayType(rows, ArrayType(columns, dataType)), read)

  override def eval(s: Store): Store = ???

  override def prettyPrint: String = {
    s"wmmaLoad(${PrettyPhrasePrinter(matrixTile)}, ${PrettyPhrasePrinter(fragment)})"
  }

  override def xmlPrinter: Elem =
    <wmmaLoad>
      <matrixTile>
        {Phrases.xmlPrinter(matrixTile)}
      </matrixTile>
      <fragment>
        {Phrases.xmlPrinter(fragment)}
      </fragment>
    </wmmaLoad>

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[CommType] = {
    WmmaLoad(fun.nat(rows), fun.nat(columns), fun.nat(d3), fun.data(dataType), fragType, layout,
      VisitAndRebuild(matrixTile, fun), VisitAndRebuild(fragment, fun))
  }
}

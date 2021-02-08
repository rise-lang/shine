package shine.cuda.primitives.imperative

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

@comPrimitive
case class WmmaFill(rows: Nat,
                    columns: Nat,
                    d3: Nat,
                    dataType: DataType,
                    fill: Phrase[ExpType],
                    fragmentType: FragmentKind,
                    layout: MatrixLayout,
                    fragment: Phrase[AccType]
                   ) extends CommandPrimitive {
  fill :: ExpType(dataType, read)
  fragment :: AccType(FragmentType(rows, columns, d3, dataType, fragmentType, layout))

  override def prettyPrint: String =
    s"WmmaFill(${PrettyPhrasePrinter(fill)}, ${PrettyPhrasePrinter(fragment)})"
}

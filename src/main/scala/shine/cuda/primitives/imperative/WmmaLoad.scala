package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}
import shine.macros.Primitive.comPrimitive

import scala.xml.Elem

@comPrimitive
final case class WmmaLoad(rows: Nat,
                          columns: Nat,
                          d3: Nat,
                          dataType: DataType,
                          fragType: FragmentKind,
                          layout: MatrixLayout,
                          matrixTile: Phrase[ExpType],
                          fragment: Phrase[AccType]
                         ) extends CommandPrimitive {
  fragment :: ExpType(FragmentType(rows, columns, d3, dataType, fragType, layout), write)
  matrixTile :: ExpType(ArrayType(rows, ArrayType(columns, dataType)), read)

  override def prettyPrint: String = {
    s"wmmaLoad(${PrettyPhrasePrinter(matrixTile)}, ${PrettyPhrasePrinter(fragment)})"
  }
}

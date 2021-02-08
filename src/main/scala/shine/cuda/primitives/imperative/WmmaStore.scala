package shine.cuda.primitives.imperative

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class WmmaStore(rows: Nat,
                           columns: Nat,
                           d3: Nat,
                           dataType: DataType,
                           fragment: Phrase[ExpType],
                           matrixTile: Phrase[AccType]
                          ) extends CommandPrimitive {
  fragment :: ExpType(FragmentType(rows, columns, d3, dataType), read)
  matrixTile :: AccType(ArrayType(rows, ArrayType(columns, dataType)))

  override def prettyPrint: String = {
    s"wmmaStore(${PrettyPhrasePrinter(fragment)}, ${PrettyPhrasePrinter(matrixTile)})"
  }
}


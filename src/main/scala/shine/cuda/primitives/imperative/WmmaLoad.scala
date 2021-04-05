package shine.cuda.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{Nat, Phrases}
import shine.macros.Primitive.comPrimitive

import scala.xml.Elem

/**
  * Loads a tile of a matrix into a fragment. <br>
  * This primitive needs to be executed by a full warp!
  * @param rows          number of rows of the fragment ({@link FragmentType#rows})
  * @param columns       number of columns of the fragment ({@link FragmentType#columns})
  * @param d3            third dimension which is used in the MMA operation ({@link FragmentType#d3})
  * @param dataType      dataType of the elements in the fragment ({@link FragmentType#datatype})
  * @param fragmentKind  kind of the fragment ({@link FragmentType#fragmentKind})
  * @param layout        layout of the fragment ({@link FragmentType#layout})
  * @param matrixTile    matrix tile which should be loaded into the fragment
  * @param fragment      fragment-Acceptor into which the `matrixTile` should be loaded
  */
@comPrimitive
final case class WmmaLoad(rows: Nat,
                          columns: Nat,
                          d3: Nat,
                          dataType: DataType,
                          fragmentKind: FragmentKind,
                          layout: MatrixLayout,
                          matrixTile: Phrase[ExpType],
                          fragment: Phrase[AccType]
                         ) extends CommandPrimitive {
  fragment :: ExpType(FragmentType(rows, columns, d3, dataType, fragmentKind, layout), write)
  matrixTile :: ExpType(ArrayType(rows, ArrayType(columns, dataType)), read)

  override def prettyPrint: String = {
    s"wmmaLoad(${PrettyPhrasePrinter(matrixTile)}, ${PrettyPhrasePrinter(fragment)})"
  }
}

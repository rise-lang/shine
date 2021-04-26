package shine.cuda.primitives.imperative

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.comPrimitive

/**
  * Stores the elements from a fragment with fragmentKind `Accumulator` into a
  * matrix tile which resides in shared or global memory. <br>
  * This primitive needs to be executed by a full warp!
  * @param rows       number of rows of the fragment ({@link FragmentType#rows})
  * @param columns    number of columns of the fragment ({@link FragmentType#columns})
  * @param d3         third dimension which is used in the MMA operation ({@link FragmentType#d3})
  * @param dataType   dataType of the elements in the fragment ({@link FragmentType#datatype})
  * @param fragment   fragment from which the elements should be stored
  * @param matrixTile matrixTile-Acceptor in which the elements should be stored
  */
@comPrimitive
final case class WmmaStore(rows: Nat,
                           columns: Nat,
                           d3: Nat,
                           dataType: DataType,
                           fragment: Phrase[ExpType],
                           matrixTile: Phrase[AccType]
                          ) extends CommandPrimitive {
  fragment :: ExpType(FragmentType(rows, columns, d3, dataType, FragmentKind.Accumulator, MatrixLayout.None), read)
  matrixTile :: AccType(ArrayType(rows, ArrayType(columns, dataType)))

  override def prettyPrint: String = {
    s"wmmaStore(${PrettyPhrasePrinter(fragment)}, ${PrettyPhrasePrinter(matrixTile)})"
  }
}


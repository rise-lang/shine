package shine.cuda.primitives.functional

import shine.DPIA.Nat
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.macros.Primitive.expPrimitive

object AsFragment{
  def apply(rows: Nat, columns: Nat, d3: Nat, dataType: DataType, fragmentType: FragmentKind, matrix: Phrase[ExpType]):
  AsFragment = AsFragment(rows, columns, d3, dataType, fragmentType, matrix, MatrixLayoutIdentifier("ml"))
}

/**
  * Returns a fragment from a matrix tile which must resides in shared or global memory ({@link WmmaLoad}). <br>
  * This primitive needs to be executed by a full warp!
  * @param rows          number of rows of the fragment ({@link FragmentType#rows})
  * @param columns       number of columns of the fragment ({@link FragmentType#columns})
  * @param d3            third dimension which is used in the MMA operation ({@link FragmentType#d3})
  * @param dataType      dataType of the elements in the fragment ({@link FragmentType#datatype})
  * @param fragmentKind  kind of the fragment ({@link FragmentType#fragmentKind})
  * @param layout        layout of the fragment ({@link FragmentType#layout}).
  *                      The layout will be infered in Codegeneration. Hence a `MatrixLayoutIdentifier` can be
  *                      used as layout.
  */
@expPrimitive
case class AsFragment(rows: Nat,
                      columns: Nat,
                      d3: Nat,
                      dataType: DataType,
                      fragmentKind: FragmentKind,
                      matrix: Phrase[ExpType],
                      layout: MatrixLayout) extends ExpPrimitive{

  matrix :: ExpType(ArrayType(rows, ArrayType(columns, dataType)), write)
  override val t: ExpType = ExpType(FragmentType(rows, columns, d3, dataType, fragmentKind, layout), write)
}
